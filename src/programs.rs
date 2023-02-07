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

fn right_lineage_length_stmt_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        pub fn right_lineage_length(node_index: u64) -> u32 {
            let bit_width: u32 = tasm::log_2_floor_u64(node_index) + 1u32;
            let npo2: u64 = 1u64 << bit_width;
            let dist: u64 = npo2 - node_index;

            let mut ret: u32 = 0u32;
            if (bit_width as u64) < dist {
                ret = right_lineage_length(node_index - (npo2 / 2u64) + 1u64);
            } else {
                ret = (dist - 1u64) as u32;
            }

            return ret;
        }
    })
}

fn right_lineage_length_expr_rast() -> syn::ItemFn {
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

fn simple_sub() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn simple_sub(a: u32, b: u32) -> u32 {
            return a - b;
        }
    })
}

fn operator_evaluation_ordering_with_div_u32() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_div() -> u32 {
            return 100u32 - 14u32 / 2u32 + 1u32;
        }
    })
}

fn operator_evaluation_ordering_with_div_u64() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_div() -> u64 {
            return 100u64 - 14u64 / 2u64 + 1u64;
        }
    })
}

fn operator_evaluation_ordering_with_mul() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_mul() -> u32 {
            return 380u32 - 14u32 * 2u32 * 10u32 + 1u32 - 41u32 * 1u32;
        }
    })
}

fn lt_u32() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn lt_for_u32_test_function() -> bool {
            let a: u32 = 14u32;
            let b: u32 = 20u32;
            let c: u32 = 10u32;
            let bool_0: bool = a < b;
            let bool_1: bool = b < c;
            let bool_2: bool = c < a;

            return bool_1 || bool_2;
        }
    })
}

fn simple_while_loop() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn simple_while_loop() -> u32 {
            let mut acc: u32 = 0u32;
            let mut i: u32 = 0u32;
            while i < 101u32 {
                acc = acc + i;
                i = i + 1u32;
            }

            return acc;
        }
    })
}

fn longer_while_loop() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn longer_while_loop(a: u32) -> u64 {
            // Should return `1641 + a`
            let mut ret: u64 = 600u64;
            let var0: u64 = 77u64;
            let var1: u64 = 10u64;
            let mut j: u64 = 23u64;
            let mut i: u32 = 0u32;
            while i < var1 as u32 {
                i = i + 1u32;
                ret = ret + var0;
                ret = ret + j;
                j = j - 1u64;
            }

            ret = 9u64 + ret + a as u64;

            return ret + var0;
        }
    })
}

fn while_loop_with_declarations() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn while_loop_with_declarations(a: u32) -> u64 {
            // Should return `1641 + a`
            let mut ret: u64 = 600u64;
            let var0: u64 = 77u64;
            let var1: u64 = 10u64;
            let mut j: u64 = 23u64;
            let mut i: u32 = 0u32;
            while i < var1 as u32 {
                let g: u32 = 10000u32;
                i = i + 1u32;
                ret = ret + var0;
                ret = ret + j;
                j = j - 1u64;
            }

            ret = 9u64 + ret + a as u64;

            return ret + var0;
        }
    })
}

fn code_block() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn code_block(a: u64) -> u32 {
            let b: u64 = a + 2u64;
            {
                let c: u32 = 0u32;
                let d: u64 = 1u64;
                let e: u64 = a + b + d;
            }

            return a as u32 + b as u32;
        }
    })
}

fn simple_list_support() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn make_short_list() -> (Vec<u64>, u32, u64, u64) {
            let mut a: Vec<u64> = Vec::<u64>::default();
            a.push(2000u64);
            a.push(3000u64);
            a.push(4000u64);
            let b: u64 = a.pop().unwrap();
            let len: u32 = a.len() as u32;

            a[1] = 5000u64;

            let d: u64 = a[0];

            return (a, len, b, d);
        }
    })
}

fn tuple_support() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn return_many() -> (bool, u32, u64) {
            let a: bool = true;
            let b: u32 = 42u32;
            let c: u64 = 100u64;

            return (a, b, c);
        }
    })
}

fn mut_list_argument() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn foo(values: &mut Vec<u64>) {
            let mut i: u64 = 0u64;
            while i < 10u64 {
                values.push(i);
                i += 1u64;
            }
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
        graft_check_compile_prop(&right_lineage_length_stmt_rast());
        graft_check_compile_prop(&right_lineage_length_expr_rast());
    }

    #[test]
    fn lt_u32_test() {
        graft_check_compile_prop(&lt_u32());
    }

    #[test]
    fn simple_while_loop_test() {
        graft_check_compile_prop(&simple_while_loop());
    }

    #[test]
    fn complicated_while_loop_test() {
        graft_check_compile_prop(&longer_while_loop());
    }

    #[test]
    fn while_loop_with_declarations_test() {
        graft_check_compile_prop(&while_loop_with_declarations());
    }

    #[test]
    fn code_block_test() {
        graft_check_compile_prop(&code_block());
    }

    #[test]
    fn simple_list_support_test() {
        graft_check_compile_prop(&simple_list_support());
    }

    #[test]
    fn tuple_support_test() {
        graft_check_compile_prop(&tuple_support());
    }

    // #[test]
    // fn mut_list_argument_test() {
    //     graft_check_compile_prop(&mut_list_argument());
    // }
}

#[cfg(test)]
mod compile_and_run_tests {
    use std::collections::HashMap;

    use num::Zero;
    use rand::{thread_rng, Rng, RngCore};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use super::*;
    use crate::{
        ast,
        shared_test::{compare_prop_with_stack, compare_prop_with_stack_and_memory},
    };

    #[test]
    fn simple_sub_test() {
        compare_prop_with_stack(
            &simple_sub(),
            vec![ast::ExprLit::U32(100), ast::ExprLit::U32(51)],
            vec![ast::ExprLit::U32(49)],
        );
    }

    #[test]
    fn operator_evaluation_ordering_test() {
        compare_prop_with_stack(
            &operator_evaluation_ordering_with_div_u32(),
            vec![],
            vec![ast::ExprLit::U32(94)],
        );

        compare_prop_with_stack(
            &operator_evaluation_ordering_with_div_u64(),
            vec![],
            vec![ast::ExprLit::U64(94)],
        );

        compare_prop_with_stack(
            &operator_evaluation_ordering_with_mul(),
            vec![],
            vec![ast::ExprLit::U32(60)],
        );
    }

    #[test]
    fn add_u64_run_test() {
        compare_prop_with_stack(
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
            compare_prop_with_stack(
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
        compare_prop_with_stack(&sub_u32_rast_1(), input_args_1, expected_outputs_1);

        let input_args_2 = vec![ast::ExprLit::U32(95), ast::ExprLit::U32(200)];
        let expected_outputs_2 = vec![ast::ExprLit::U32(105)];
        compare_prop_with_stack(&sub_u32_rast_2(), input_args_2, expected_outputs_2);
    }

    #[test]
    fn sub_u64_run_test() {
        let input_args_1 = vec![ast::ExprLit::U64(200), ast::ExprLit::U64(95)];
        let expected_outputs_1 = vec![ast::ExprLit::U64(105)];
        compare_prop_with_stack(&sub_u64_rast_1(), input_args_1, expected_outputs_1);

        let input_args_2 = vec![ast::ExprLit::U64(95), ast::ExprLit::U64(200)];
        let expected_outputs_2 = vec![ast::ExprLit::U64(105)];
        compare_prop_with_stack(&sub_u64_rast_2(), input_args_2, expected_outputs_2);

        let input_args_3 = vec![ast::ExprLit::U64(1), ast::ExprLit::U64(1 << 32)];
        let expected_outputs_3 = vec![ast::ExprLit::U64(u32::MAX as u64)];
        compare_prop_with_stack(&sub_u64_rast_2(), input_args_3, expected_outputs_3);

        let lhs = thread_rng().gen_range(0..u64::MAX);
        let rhs = thread_rng().gen_range(0..=lhs);
        let input_args_4 = vec![ast::ExprLit::U64(rhs), ast::ExprLit::U64(lhs)];
        let expected_outputs_4 = vec![ast::ExprLit::U64(lhs - rhs)];
        compare_prop_with_stack(&sub_u64_rast_2(), input_args_4, expected_outputs_4);
    }

    #[test]
    fn right_child_run_test() {
        compare_prop_with_stack(
            &right_child_rast(),
            vec![ast::ExprLit::U64(120)],
            vec![ast::ExprLit::U64(119)],
        );
        let mut rng = thread_rng();
        let rand = rng.next_u64();
        compare_prop_with_stack(
            &right_child_rast(),
            vec![ast::ExprLit::U64(rand)],
            vec![ast::ExprLit::U64(rand - 1)],
        );
    }

    #[test]
    fn left_child_run_test() {
        let inputs0 = vec![ast::ExprLit::U64(120), ast::ExprLit::U32(2)];
        let outputs0 = vec![ast::ExprLit::U64(116)];
        compare_prop_with_stack(&left_child_rast(), inputs0, outputs0);

        let inputs1 = vec![ast::ExprLit::U64(31), ast::ExprLit::U32(4)];
        let outputs1 = vec![ast::ExprLit::U64(15)];
        compare_prop_with_stack(&left_child_rast(), inputs1, outputs1);
    }

    // right_lineage_length_test
    #[test]
    fn right_lineage_length_run_test() {
        fn prop_right_lineage_length_run(node_index: u64, expected: u32) {
            let inputs = vec![ast::ExprLit::U64(node_index)];
            let outputs = vec![ast::ExprLit::U32(expected)];
            compare_prop_with_stack(
                &right_lineage_length_stmt_rast(),
                inputs.clone(),
                outputs.clone(),
            );
            compare_prop_with_stack(&right_lineage_length_expr_rast(), inputs, outputs);
        }

        prop_right_lineage_length_run(1, 0);
        prop_right_lineage_length_run(2, 1);
        prop_right_lineage_length_run(3, 0);
        prop_right_lineage_length_run(4, 0);
        prop_right_lineage_length_run(5, 2);
        prop_right_lineage_length_run(6, 1);
        prop_right_lineage_length_run(7, 0);
        prop_right_lineage_length_run(8, 0);
        prop_right_lineage_length_run(9, 1);
        prop_right_lineage_length_run(10, 0);
        prop_right_lineage_length_run(11, 0);
        prop_right_lineage_length_run(12, 3);
        prop_right_lineage_length_run(13, 2);
        prop_right_lineage_length_run(14, 1);
        prop_right_lineage_length_run(15, 0);
        prop_right_lineage_length_run(16, 0);
        prop_right_lineage_length_run(17, 1);
        prop_right_lineage_length_run(18, 0);
        prop_right_lineage_length_run(19, 0);
        prop_right_lineage_length_run(20, 2);
        prop_right_lineage_length_run(21, 1);
        prop_right_lineage_length_run(22, 0);
        prop_right_lineage_length_run(23, 0);
        prop_right_lineage_length_run(24, 1);
        prop_right_lineage_length_run(25, 0);
        prop_right_lineage_length_run(26, 0);
        prop_right_lineage_length_run(27, 4);
        prop_right_lineage_length_run(28, 3);
        prop_right_lineage_length_run(29, 2);
        prop_right_lineage_length_run(30, 1);
        prop_right_lineage_length_run(31, 0);
        prop_right_lineage_length_run(32, 0);
        prop_right_lineage_length_run(33, 1);
    }

    #[test]
    fn lt_u32_test() {
        compare_prop_with_stack(&lt_u32(), vec![], vec![ast::ExprLit::Bool(true)]);
    }

    #[test]
    fn simple_while_loop_run_test() {
        compare_prop_with_stack(&simple_while_loop(), vec![], vec![ast::ExprLit::U32(5050)]);
    }

    #[test]
    fn complicated_while_loop_test() {
        compare_prop_with_stack(
            &longer_while_loop(),
            vec![ast::ExprLit::U32(1000)],
            vec![ast::ExprLit::U64(2641)],
        );
        compare_prop_with_stack(
            &while_loop_with_declarations(),
            vec![ast::ExprLit::U32(2000)],
            vec![ast::ExprLit::U64(3641)],
        );
        compare_prop_with_stack(
            &while_loop_with_declarations(),
            vec![ast::ExprLit::U32(2001)],
            vec![ast::ExprLit::U64(3642)],
        );
    }

    #[test]
    fn code_block_run_test() {
        fn prop_code_block(input: u64) {
            let inputs = vec![ast::ExprLit::U64(input)];
            let outputs = vec![ast::ExprLit::U32(2 * (input as u32) + 2)];
            compare_prop_with_stack(&code_block(), inputs, outputs);
        }

        let mut rng = thread_rng();
        for _ in 0..10 {
            prop_code_block(rng.gen_range(0..(1u64 << 30)));
        }
    }

    #[test]
    fn simple_list_support_run_test() {
        let inputs = vec![];
        let outputs = vec![
            ast::ExprLit::BFE(BFieldElement::zero()),
            ast::ExprLit::U32(2),
            ast::ExprLit::U64(4000),
            ast::ExprLit::U64(2000),
        ];
        let input_memory = HashMap::default();
        let mut expected_final_memory = HashMap::default();
        tasm_lib::rust_shadowing_helper_functions::unsafe_list_new(
            BFieldElement::zero(),
            &mut expected_final_memory,
        );
        tasm_lib::rust_shadowing_helper_functions::unsafe_list_push(
            BFieldElement::zero(),
            [BFieldElement::new(2000), BFieldElement::new(0)],
            &mut expected_final_memory,
        );
        tasm_lib::rust_shadowing_helper_functions::unsafe_list_push(
            BFieldElement::zero(),
            [BFieldElement::new(5000), BFieldElement::new(0)],
            &mut expected_final_memory,
        );
        tasm_lib::rust_shadowing_helper_functions::unsafe_list_push(
            BFieldElement::zero(),
            [BFieldElement::new(4000), BFieldElement::new(0)],
            &mut expected_final_memory,
        );
        tasm_lib::rust_shadowing_helper_functions::unsafe_list_set_length(
            BFieldElement::zero(),
            2,
            &mut expected_final_memory,
        );
        compare_prop_with_stack_and_memory(
            &simple_list_support(),
            inputs,
            outputs,
            input_memory,
            expected_final_memory,
        );
    }

    #[test]
    fn tuple_support_run_test() {
        let outputs = vec![
            ast::ExprLit::Bool(true),
            ast::ExprLit::U32(42),
            ast::ExprLit::U64(100),
        ];

        compare_prop_with_stack(&tuple_support(), vec![], outputs);
    }

    // #[test]
    // fn simple_list_support_test() {
    //     graft_check_compile_prop(&simple_list_support());
    // }
}
