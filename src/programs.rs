use syn::parse_quote;

use crate::graft::{graft, item_fn};

fn nop_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn nop() {
            return;
        }
    })
}

fn add_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs + rhs;
            return c;
        }
    })
}

fn add_u32_overwrite_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_u32_overwrite(lhs: u32, rhs: u32) -> u32 {
            let mut c: u32 = lhs + rhs;
            c = c + rhs;
            return c;
        }
    })
}

fn add_bfe_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_bfe(lhs: BFieldElement, rhs: BFieldElement) -> BFieldElement {
            let c: BFieldElement = lhs + rhs;
            return c;
        }
    })
}

fn add_xfe_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_xfe(lhs: XFieldElement, rhs: XFieldElement) -> XFieldElement {
            let c: XFieldElement = lhs + rhs;
            return c;
        }
    })
}

fn add_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        // using `add_u64` as function name here would create a name-clash
        // between a dependency and the function we are compiling.
        fn add_u64_test(lhs: u64, rhs: u64) -> u64 {
            let c: u64 = lhs + rhs;
            return c;
        }
    })
}

fn and_bool_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn boolean_and_bool(lhs: bool, rhs: bool) -> bool {
            let c: bool = lhs && rhs;
            return c;
        }
    })
}

fn bitwise_and_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn bitwise_and_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs & rhs;
            return c;
        }
    })
}

fn bitwise_and_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn bitwise_and_u64(lhs: u64, rhs: u64) -> u64 {
            let c: u64 = lhs & rhs;
            return c;
        }
    })
}

fn right_child_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn right_child(node_index: u64) -> u64 {
            return node_index - 1u64;
        }
    })
}

fn left_child_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn left_child(node_index: u64, height: u32) -> u64 {
            return node_index - (1u64 << height);
        }
    })
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use rand::{thread_rng, Rng, RngCore};
    use tasm_lib::get_init_tvm_stack;
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::ast::{self};
    use crate::tasm::compile;
    use crate::types::annotate_fn;

    use super::*;

    fn graft_check_compile_prop(item_fn: &syn::ItemFn) -> String {
        // parse test
        let mut function = graft(item_fn);

        // type-check and annotate
        annotate_fn(&mut function);

        println!("{:#?}", function);

        // compile
        let tasm = compile(&function);
        let tasm_string: String = tasm.iter().map(|instr| instr.to_string()).join("\n");
        println!("{}", tasm_string);
        tasm_string
    }

    fn compile_execute_and_compare_prop(
        function_name: &str,
        item_fn: &syn::ItemFn,
        input_args: Vec<ast::ExprLit>,
        expected_outputs: Vec<ast::ExprLit>,
    ) {
        let code = graft_check_compile_prop(item_fn);
        let code = format!(
            "
            call {function_name}
            halt

            {code}"
        );

        let mut stack = get_init_tvm_stack();
        for input_arg in input_args {
            // TODO: Rewrite this using `to_sequence()` from the Hashable trait
            let mut bfe_sequence: Vec<BFieldElement> = match input_arg {
                ast::ExprLit::Bool(b) => vec![BFieldElement::new(b as u64)],
                ast::ExprLit::U32(v) => vec![BFieldElement::new(v as u64)],
                ast::ExprLit::U64(v) => vec![
                    BFieldElement::new(v >> 32),
                    BFieldElement::new(v & u32::MAX as u64),
                ],
                ast::ExprLit::BFE(bfe) => vec![bfe],
                ast::ExprLit::XFE(xfe) => xfe.coefficients.to_vec(),
                ast::ExprLit::Digest(digest) => digest.values().to_vec(),
            };
            stack.append(&mut bfe_sequence);
        }

        let mut expected_final_stack = get_init_tvm_stack();
        for output in expected_outputs {
            // TODO: Rewrite this using `to_sequence()` from the Hashable trait
            let mut bfe_sequence: Vec<BFieldElement> = match output {
                ast::ExprLit::Bool(b) => vec![BFieldElement::new(b as u64)],
                ast::ExprLit::U32(v) => vec![BFieldElement::new(v as u64)],
                ast::ExprLit::U64(v) => vec![
                    BFieldElement::new(v >> 32),
                    BFieldElement::new(v & u32::MAX as u64),
                ],
                ast::ExprLit::BFE(bfe) => vec![bfe],
                ast::ExprLit::XFE(xfe) => xfe.coefficients.to_vec(),
                ast::ExprLit::Digest(digest) => digest.values().to_vec(),
            };
            expected_final_stack.append(&mut bfe_sequence);
        }

        let init_stack_length = stack.len();
        let exec_result = tasm_lib::execute(
            &code,
            &mut stack,
            expected_final_stack.len() as isize - init_stack_length as isize,
            vec![],
            vec![],
            &mut HashMap::default(),
        );

        assert_eq!(
            expected_final_stack,
            exec_result.final_stack,
            "Code execution must produce expected stack `{}`. \n\nTVM:\n{}\nExpected:\n{}",
            function_name,
            exec_result
                .final_stack
                .iter()
                .map(|x| x.to_string())
                .collect_vec()
                .join(","),
            expected_final_stack
                .iter()
                .map(|x| x.to_string())
                .collect_vec()
                .join(","),
        );
    }

    #[test]
    fn nop_test() {
        graft_check_compile_prop(&nop_rast());
    }

    #[test]
    fn add_u32_test() {
        graft_check_compile_prop(&add_u32_rast());
    }

    #[test]
    fn add_u32_overwrite_rast_test() {
        graft_check_compile_prop(&add_u32_overwrite_rast());
    }

    #[test]
    fn add_bfe_test() {
        graft_check_compile_prop(&add_bfe_rast());
    }

    #[test]
    fn add_xfe_test() {
        graft_check_compile_prop(&add_xfe_rast());
    }

    #[test]
    fn add_u64_test() {
        graft_check_compile_prop(&add_u64_rast());
    }

    #[test]
    fn add_u64_run_test() {
        compile_execute_and_compare_prop(
            "add_u64_test",
            &add_u64_rast(),
            vec![
                ast::ExprLit::U64((1 << 33) + (1 << 16)),
                ast::ExprLit::U64((1 << 33) + (1 << 16)),
            ],
            vec![ast::ExprLit::U64((1 << 34) + (1 << 17))],
        );
        for _ in 0..10 {
            let lhs = rand::thread_rng().gen_range(0..u64::MAX / 2);
            let rhs = rand::thread_rng().gen_range(0..u64::MAX / 2);
            compile_execute_and_compare_prop(
                "add_u64_test",
                &add_u64_rast(),
                vec![ast::ExprLit::U64(lhs), ast::ExprLit::U64(rhs)],
                vec![ast::ExprLit::U64(lhs + rhs)],
            )
        }
    }

    #[test]
    fn and_bool_test() {
        graft_check_compile_prop(&and_bool_rast());
    }

    #[test]
    fn bitwise_and_u32_test() {
        graft_check_compile_prop(&bitwise_and_u32_rast());
    }

    #[test]
    fn bitwise_and_u64_test() {
        graft_check_compile_prop(&bitwise_and_u64_rast());
    }

    #[test]
    fn right_child_run_test() {
        compile_execute_and_compare_prop(
            "right_child",
            &right_child_rast(),
            vec![ast::ExprLit::U64(120)],
            vec![ast::ExprLit::U64(119)],
        );
        let mut rng = thread_rng();
        let rand = rng.next_u64();
        compile_execute_and_compare_prop(
            "right_child",
            &right_child_rast(),
            vec![ast::ExprLit::U64(rand)],
            vec![ast::ExprLit::U64(rand - 1)],
        );
    }

    #[test]
    fn right_child_test() {
        graft_check_compile_prop(&right_child_rast());
    }

    #[test]
    fn left_child_test() {
        graft_check_compile_prop(&left_child_rast());
    }
}
