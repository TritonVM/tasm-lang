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
pub fn leaf_index_to_mt_index_and_peak_index_rast_loops_reduced() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn leaf_index_to_mt_index_and_peak_index(leaf_index: u64, leaf_count: u64) -> (u64, u32) {

            // a) Get the index as if this was a Merkle tree
            let local_mt_height_u32: u32 = tasm::tasm_arithmetic_u64_log_2_floor(leaf_index ^ leaf_count);
            let local_mt_leaf_count: u64 = tasm::tasm_arithmetic_u64_pow2(local_mt_height_u32);
            let remainder_bitmask: u64 = local_mt_leaf_count - 1;
            let mt_index: u64 = (remainder_bitmask & leaf_index) as u64 + local_mt_leaf_count;

            // b) Find the peak_index (in constant time)
            //let all_the_ones: u32 = leaf_count.count_ones();

            // TODO: Use .count_ones() / popcount when available
            // the following two loops can probably be merged.
            // .count_ones()
            let mut peak_index: u32 = 0u32;
            let mut tmp: u64 = leaf_count;
            while 0u64 < tmp {
                peak_index += (tmp & 1u64) as u32;
                tmp >>= 1u32;
            }

            //let ones_to_subtract: u32 = (leaf_count & remainder_bitmask).count_ones();
            tmp = leaf_count & remainder_bitmask;
            // .count_ones()
            while 0u64 < tmp {
                peak_index -= (tmp & 1u64) as u32;
                tmp >>= 1u32;
            }

            peak_index -= 1u32;
            return (mt_index, peak_index);
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
    fn leaf_index_to_mt_index_run_test() {
        fn check((out1, out2): (u64, u32), (leaf_count, leaf_index): (u64, u64)) {
            let inputs = vec![u64_lit(leaf_count), u64_lit(leaf_index)];
            let outputs = vec![u64_lit(out1), u32_lit(out2)];
            compare_prop_with_stack(
                &leaf_index_to_mt_index_and_peak_index_rast_loops_reduced(),
                inputs,
                outputs,
            );
        }

        // Leaf count = 1
        check((1, 0), (0, 1));

        // Leaf count = 2
        check((2, 0), (0, 2));
        check((2, 0), (0, 2));
        check((3, 0), (1, 2));

        // Leaf count = 3
        check((2, 0), (0, 3));
        check((3, 0), (1, 3));
        check((1, 1), (2, 3));

        // Leaf count = 4
        check((4, 0), (0, 4));
        check((5, 0), (1, 4));
        check((6, 0), (2, 4));
        check((7, 0), (3, 4));

        // Leaf count = 14
        check((8, 0), (0, 14));
        check((9, 0), (1, 14));
        check((10, 0), (2, 14));
        check((11, 0), (3, 14));
        check((12, 0), (4, 14));
        check((13, 0), (5, 14));
        check((14, 0), (6, 14));
        check((15, 0), (7, 14));
        check((4, 1), (8, 14));
        check((5, 1), (9, 14));
        check((6, 1), (10, 14));
        check((7, 1), (11, 14));
        check((7, 1), (11, 14));

        // Leaf count = 22
        check((16, 0), (0, 23));
        check((17, 0), (1, 23));
        check((18, 0), (2, 23));
        check((19, 0), (3, 23));
        check((30, 0), (14, 23));
        check((31, 0), (15, 23));
        check((4, 1), (16, 23));
        check((5, 1), (17, 23));
        check((6, 1), (18, 23));
        check((7, 1), (19, 23));
        check((2, 2), (20, 23));
        check((3, 2), (21, 23));
        check((1, 3), (22, 23));

        // Leaf count = 32
        for i in 0..32 {
            check((32 + i, 0), (i, 32));
        }

        // Leaf count = 33
        for i in 0..32 {
            check((32 + i, 0), (i, 33));
        }
        check((1, 1), (32, 33));

        // Leaf count = 34
        for i in 0..32 {
            check((32 + i, 0), (i, 34));
        }
        check((2, 1), (32, 34));
        check((3, 1), (33, 34));

        // Leaf count = 35
        for i in 0..32 {
            check((32 + i, 0), (i, 35));
        }
        check((2, 1), (32, 35));
        check((3, 1), (33, 35));
        check((1, 2), (34, 35));

        // Leaf count = 36
        for i in 0..32 {
            check((32 + i, 0), (i, 36));
        }
        check((4, 1), (32, 36));
        check((5, 1), (33, 36));
        check((6, 1), (34, 36));
        check((7, 1), (35, 36));

        // Leaf count = 37
        for i in 0..32 {
            check((32 + i, 0), (i, 37));
        }
        check((4, 1), (32, 37));
        check((5, 1), (33, 37));
        check((6, 1), (34, 37));
        check((7, 1), (35, 37));
        check((1, 2), (36, 37));

        for i in 10..63 {
            check((14 + (1 << i), 0), (14, 1 << i));
            check((3, 2), ((1 << i) + 9, (1 << i) + 11));
            check((1, 3), ((1 << i) + 10, (1 << i) + 11));
        }
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

    #[test]
    fn leaf_index_to_mt_index_and_peak_index_test() {
        graft_check_compile_prop(&leaf_index_to_mt_index_and_peak_index_rast_loops_reduced());
    }
}
