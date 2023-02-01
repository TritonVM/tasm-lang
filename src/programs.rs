use syn::parse_quote;

use crate::graft::item_fn;

fn nop_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn nop_nop() {
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

fn sub_u32_rast_1() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs - rhs;
            return c;
        }
    })
}

fn sub_u32_rast_2() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u32(rhs: u32, lhs: u32) -> u32 {
            let c: u32 = lhs - rhs;
            return c;
        }
    })
}

fn sub_u64_rast_1() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u64_test(lhs: u64, rhs: u64) -> u64 {
            let c: u64 = lhs - rhs;
            return c;
        }
    })
}

fn sub_u64_rast_2() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u64_test(rhs: u64, lhs: u64) -> u64 {
            let c: u64 = lhs - rhs;
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
            // return node_index - pow2(height);
            // return node_index - 2u64.pow(height);
            return node_index - (1u64 << height);
            // return node_index - 2u32.pow(height);

        }
    })
}

fn right_lineage_length_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn right_lineage_length(node_index: u64) -> u32 {
            let bit_width: u32 = tasm::log_2_floor_u64(node_index) + 1u32;
            let npo2: u64 = 1u64 << bit_width;
            let dist: u64 = npo2 - node_index;

            // let bit_width_u64: u64 = bit_width.into();
            let bit_width_u64: u64 = bit_width as u64;
            let ret: u32 = if bit_width_u64 < dist {
                right_lineage_length(node_index - (npo2 / 2u64) + 1u64)
            } else {
                (dist - 1u64) as u32
            };

            return ret;
        }
    })
}

#[cfg(test)]
mod compile_and_typecheck_tests {
    use crate::shared_test::graft_check_compile_prop;

    use super::*;

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
    fn sub_u32_rast_1_test() {
        graft_check_compile_prop(&sub_u32_rast_1());
    }

    #[test]
    fn sub_u32_rast_2_test() {
        graft_check_compile_prop(&sub_u32_rast_2());
    }

    #[test]
    fn sub_u64_rast_1_test() {
        graft_check_compile_prop(&sub_u64_rast_1());
    }

    #[test]
    fn sub_u64_rast_2_test() {
        graft_check_compile_prop(&sub_u64_rast_2());
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
    fn right_child_test() {
        graft_check_compile_prop(&right_child_rast());
    }

    #[test]
    fn left_child_test() {
        graft_check_compile_prop(&left_child_rast());
    }

    #[test]
    fn right_lineage_length_test() {
        graft_check_compile_prop(&right_lineage_length_rast());
    }
}

#[cfg(test)]
mod compile_and_run_tests {
    use rand::{thread_rng, Rng, RngCore};

    use super::*;
    use crate::{ast, shared_test::compile_execute_and_compare_prop};

    #[test]
    fn add_u64_run_test() {
        compile_execute_and_compare_prop(
            &add_u64_rast(),
            vec![
                ast::ExprLit::U64((1 << 33) + (1 << 16)),
                ast::ExprLit::U64((1 << 33) + (1 << 16)),
            ],
            vec![ast::ExprLit::U64((1 << 34) + (1 << 17))],
        );
        for _ in 0..10 {
            let lhs = thread_rng().gen_range(0..u64::MAX / 2);
            let rhs = thread_rng().gen_range(0..u64::MAX / 2);
            compile_execute_and_compare_prop(
                &add_u64_rast(),
                vec![ast::ExprLit::U64(lhs), ast::ExprLit::U64(rhs)],
                vec![ast::ExprLit::U64(lhs + rhs)],
            )
        }
    }

    #[test]
    fn sub_u32_run_test() {
        let input_args_1 = vec![ast::ExprLit::U32(200), ast::ExprLit::U32(95)];
        let expected_outputs_1 = vec![ast::ExprLit::U32(105)];
        compile_execute_and_compare_prop(&sub_u32_rast_1(), input_args_1, expected_outputs_1);

        let input_args_2 = vec![ast::ExprLit::U32(95), ast::ExprLit::U32(200)];
        let expected_outputs_2 = vec![ast::ExprLit::U32(105)];
        compile_execute_and_compare_prop(&sub_u32_rast_2(), input_args_2, expected_outputs_2);
    }

    #[test]
    fn sub_u64_run_test() {
        let input_args_1 = vec![ast::ExprLit::U64(200), ast::ExprLit::U64(95)];
        let expected_outputs_1 = vec![ast::ExprLit::U64(105)];
        compile_execute_and_compare_prop(&sub_u64_rast_1(), input_args_1, expected_outputs_1);

        let input_args_2 = vec![ast::ExprLit::U64(95), ast::ExprLit::U64(200)];
        let expected_outputs_2 = vec![ast::ExprLit::U64(105)];
        compile_execute_and_compare_prop(&sub_u64_rast_2(), input_args_2, expected_outputs_2);

        let input_args_3 = vec![ast::ExprLit::U64(1), ast::ExprLit::U64(1 << 32)];
        let expected_outputs_3 = vec![ast::ExprLit::U64(u32::MAX as u64)];
        compile_execute_and_compare_prop(&sub_u64_rast_2(), input_args_3, expected_outputs_3);

        let lhs = thread_rng().gen_range(0..u64::MAX);
        let rhs = thread_rng().gen_range(0..=lhs);
        let input_args_4 = vec![ast::ExprLit::U64(rhs), ast::ExprLit::U64(lhs)];
        let expected_outputs_4 = vec![ast::ExprLit::U64(lhs - rhs)];
        compile_execute_and_compare_prop(&sub_u64_rast_2(), input_args_4, expected_outputs_4);
    }

    #[test]
    fn right_child_run_test() {
        compile_execute_and_compare_prop(
            &right_child_rast(),
            vec![ast::ExprLit::U64(120)],
            vec![ast::ExprLit::U64(119)],
        );
        let mut rng = thread_rng();
        let rand = rng.next_u64();
        compile_execute_and_compare_prop(
            &right_child_rast(),
            vec![ast::ExprLit::U64(rand)],
            vec![ast::ExprLit::U64(rand - 1)],
        );
    }

    #[test]
    fn left_child_run_test() {
        let inputs0 = vec![ast::ExprLit::U64(120), ast::ExprLit::U32(2)];
        let outputs0 = vec![ast::ExprLit::U64(116)];
        compile_execute_and_compare_prop(&left_child_rast(), inputs0, outputs0);

        let inputs1 = vec![ast::ExprLit::U64(31), ast::ExprLit::U32(4)];
        let outputs1 = vec![ast::ExprLit::U64(15)];
        compile_execute_and_compare_prop(&left_child_rast(), inputs1, outputs1);
    }
}
