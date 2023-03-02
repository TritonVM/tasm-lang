use syn::parse_quote;

use crate::graft::item_fn;

#[allow(dead_code)]
pub fn left_child_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn left_child(node_index: u64, height: u32) -> u64 {
            return node_index - (1 << height);
        }
    })
}

#[allow(dead_code)]
pub fn right_child_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn right_child(node_index: u64) -> u64 {
            return node_index - 1;
        }
    })
}

#[allow(dead_code)]
pub fn leftmost_ancestor_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn leftmost_ancestor(node_index: u64) -> (u64, u32) {
            let h: u32 = tasm::tasm_arithmetic_u64_log_2_floor(node_index);
            let ret: u64 = (1 << (h + 1)) - 1;

            return (ret, h);
    }
    })
}

#[allow(dead_code)]
pub fn right_lineage_length_stmt_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        pub fn right_lineage_length(node_index: u64) -> u32 {
            let bit_width: u32 = tasm::tasm_arithmetic_u64_log_2_floor(node_index) + 1u32;
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

#[allow(dead_code)]
pub fn right_lineage_length_expr_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn right_lineage_length(node_index: u64) -> u32 {
            let bit_width: u32 = tasm::tasm_arithmetic_u64_log_2_floor(node_index) + 1u32;
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

#[allow(dead_code)]
pub fn right_lineage_length_and_own_height_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn right_lineage_length_and_own_height(node_index: u64) -> (u32, u32) {
            let mut candidate_and_candidate_height: (u64, u32) = tasm::tasm_mmr_leftmost_ancestor(node_index);

            let mut right_ancestor_count: u32 = 0;

            while candidate_and_candidate_height.0 != node_index {
                let left_child: u64 = candidate_and_candidate_height.0 - (1 << candidate_and_candidate_height.1);
                let candidate_is_right_child: bool = left_child < node_index;
                if candidate_is_right_child {
                    candidate_and_candidate_height.0 = candidate_and_candidate_height.0 - 1;
                    right_ancestor_count = right_ancestor_count + 1;
                } else {
                    candidate_and_candidate_height.0 = left_child;
                    right_ancestor_count = 0;
                }

                candidate_and_candidate_height.1 = candidate_and_candidate_height.1 - 1;
            }

            return (right_ancestor_count, candidate_and_candidate_height.1);
        }
    })
}

#[allow(dead_code)]
pub fn left_sibling_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn left_sibling(node_index: u64, height: u32) -> u64 {
            return node_index - (1 << (height + 1)) + 1;
        }
    })
}

#[allow(dead_code)]
pub fn right_sibling_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn right_sibling(node_index: u64, height: u32) -> u64 {
            return node_index + (1 << (height + 1)) - 1;
        }
    })
}
#[allow(dead_code)]
pub fn get_height_from_leaf_index_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn get_height_from_leaf_index(leaf_index: u64) -> u32 {
            return tasm::tasm_arithmetic_u64_log_2_floor(leaf_index + 1u64);
        }
    })
}

#[cfg(test)]
mod run_tests {
    use rand::{thread_rng, RngCore};

    use super::*;
    use crate::tests::shared_test::*;

    #[test]
    fn right_child_run_test() {
        compare_prop_with_stack(&right_child_rast(), vec![u64_lit(120)], vec![u64_lit(119)]);
        let mut rng = thread_rng();
        let rand = rng.next_u64();
        compare_prop_with_stack(
            &right_child_rast(),
            vec![u64_lit(rand)],
            vec![u64_lit(rand - 1)],
        );
    }

    #[test]
    fn left_child_run_test() {
        let inputs0 = vec![u64_lit(120), u32_lit(2)];
        let outputs0 = vec![u64_lit(116)];
        compare_prop_with_stack(&left_child_rast(), inputs0, outputs0);

        let inputs1 = vec![u64_lit(31), u32_lit(4)];
        let outputs1 = vec![u64_lit(15)];
        compare_prop_with_stack(&left_child_rast(), inputs1, outputs1);
    }

    // right_lineage_length_test
    #[test]
    fn right_lineage_length_run_test() {
        fn prop_right_lineage_length_run(node_index: u64, expected: u32) {
            let inputs = vec![u64_lit(node_index)];
            let outputs = vec![u32_lit(expected)];
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
    fn leftmost_ancestor_run_test() {
        let test_cases = vec![
            InputOutputTestCase::new(vec![u64_lit(1)], vec![u64_lit(1), u32_lit(0)]),
            InputOutputTestCase::new(vec![u64_lit(2)], vec![u64_lit(3), u32_lit(1)]),
            InputOutputTestCase::new(vec![u64_lit(3)], vec![u64_lit(3), u32_lit(1)]),
            InputOutputTestCase::new(vec![u64_lit(4)], vec![u64_lit(7), u32_lit(2)]),
            InputOutputTestCase::new(vec![u64_lit(5)], vec![u64_lit(7), u32_lit(2)]),
            InputOutputTestCase::new(vec![u64_lit(6)], vec![u64_lit(7), u32_lit(2)]),
            InputOutputTestCase::new(vec![u64_lit(7)], vec![u64_lit(7), u32_lit(2)]),
            InputOutputTestCase::new(vec![u64_lit(8)], vec![u64_lit(15), u32_lit(3)]),
            InputOutputTestCase::new(vec![u64_lit(9)], vec![u64_lit(15), u32_lit(3)]),
            InputOutputTestCase::new(vec![u64_lit(15)], vec![u64_lit(15), u32_lit(3)]),
            InputOutputTestCase::new(vec![u64_lit(16)], vec![u64_lit(31), u32_lit(4)]),
        ];

        for test_case in test_cases {
            compare_prop_with_stack(
                &leftmost_ancestor_rast(),
                test_case.input_args,
                test_case.expected_outputs,
            );
        }
    }

    #[test]
    fn right_lineage_length_and_own_height_test() {
        let test_cases = vec![
            InputOutputTestCase::new(vec![u64_lit(1)], vec![u32_lit(0), u32_lit(0)]),
            InputOutputTestCase::new(vec![u64_lit(2)], vec![u32_lit(1), u32_lit(0)]),
            InputOutputTestCase::new(vec![u64_lit(3)], vec![u32_lit(0), u32_lit(1)]),
            InputOutputTestCase::new(vec![u64_lit(4)], vec![u32_lit(0), u32_lit(0)]),
            InputOutputTestCase::new(vec![u64_lit(5)], vec![u32_lit(2), u32_lit(0)]),
            InputOutputTestCase::new(vec![u64_lit(6)], vec![u32_lit(1), u32_lit(1)]),
            InputOutputTestCase::new(vec![u64_lit(7)], vec![u32_lit(0), u32_lit(2)]),
            InputOutputTestCase::new(vec![u64_lit(8)], vec![u32_lit(0), u32_lit(0)]),
            InputOutputTestCase::new(vec![u64_lit(9)], vec![u32_lit(1), u32_lit(0)]),
            InputOutputTestCase::new(vec![u64_lit(10)], vec![u32_lit(0), u32_lit(1)]),
            InputOutputTestCase::new(vec![u64_lit(11)], vec![u32_lit(0), u32_lit(0)]),
            InputOutputTestCase::new(vec![u64_lit(12)], vec![u32_lit(3), u32_lit(0)]),
            InputOutputTestCase::new(vec![u64_lit(13)], vec![u32_lit(2), u32_lit(1)]),
            InputOutputTestCase::new(vec![u64_lit(14)], vec![u32_lit(1), u32_lit(2)]),
            InputOutputTestCase::new(vec![u64_lit(15)], vec![u32_lit(0), u32_lit(3)]),
            InputOutputTestCase::new(vec![u64_lit(16)], vec![u32_lit(0), u32_lit(0)]),
            InputOutputTestCase::new(vec![u64_lit(17)], vec![u32_lit(1), u32_lit(0)]),
            InputOutputTestCase::new(
                vec![u64_lit(u64::MAX / 2 - 2)],
                vec![u32_lit(2), u32_lit(60)],
            ),
        ];

        for test_case in test_cases {
            compare_prop_with_stack(
                &right_lineage_length_and_own_height_rast(),
                test_case.input_args,
                test_case.expected_outputs,
            );
        }
    }

    #[test]
    fn left_sibling_test() {
        fn prop(node_index: u64, height: u32, expected: u64) {
            compare_prop_with_stack(
                &left_sibling_rast(),
                vec![u64_lit(node_index), u32_lit(height)],
                vec![u64_lit(expected)],
            )
        }

        prop(6, 1, 3);
        prop(2, 0, 1);
        prop(5, 0, 4);
        prop(30, 3, 15);
        prop(29, 2, 22);
        prop(14, 2, 7);
    }

    #[test]
    fn right_sibling_test() {
        fn prop(node_index: u64, height: u32, expected: u64) {
            compare_prop_with_stack(
                &right_sibling_rast(),
                vec![u64_lit(node_index), u32_lit(height)],
                vec![u64_lit(expected)],
            )
        }

        prop(3, 1, 6);
        prop(1, 0, 2);
        prop(4, 0, 5);
        prop(15, 3, 30);
        prop(22, 2, 29);
        prop(7, 2, 14);
    }

    #[test]
    fn get_height_from_leaf_index_test() {
        fn prop(node_index: u64, expected: u32) {
            compare_prop_with_stack(
                &get_height_from_leaf_index_rast(),
                vec![u64_lit(node_index)],
                vec![u32_lit(expected)],
            )
        }

        prop(0, 0);
        prop(1, 1);
        prop(2, 1);
        prop(3, 2);
        prop(4, 2);
        prop(5, 2);
        prop(6, 2);
        prop(7, 3);
        prop(8, 3);
        prop(9, 3);
        prop((1 << 33) - 2, 32);
        prop((1 << 33) - 1, 33);
        prop(1 << 33, 33);
        prop((1 << 33) + 1, 33);
        prop((1 << 45) - 2, 44);
        prop((1 << 45) - 1, 45);
        prop((1 << 62) - 2, 61);
        prop((1 << 62) - 1, 62);
    }
}

#[cfg(test)]
mod compile_and_typecheck_tests {
    use super::*;
    use crate::tests::shared_test::graft_check_compile_prop;

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
    fn leftmost_ancestor_test() {
        graft_check_compile_prop(&leftmost_ancestor_rast());
    }

    #[test]
    fn right_lineage_length_and_own_height_test() {
        graft_check_compile_prop(&right_lineage_length_and_own_height_rast());
    }
}
