use syn::parse_quote;
use syn::ItemFn;

use crate::tests_and_benchmarks::test_helpers::shared_test::item_fn;

pub(crate) fn verify_authentication_path_with_local_function() -> ItemFn {
    item_fn(parse_quote! {
        fn verify_authentication_path(
            peaks: Vec<Digest>,
            leaf_count: u64,
            auth_path: Vec<Digest>,
            leaf_index: u64,
            leaf_hash: Digest,
        ) -> bool {
            fn leaf_index_to_mt_index_and_peak_index(leaf_index: u64, leaf_count: u64) -> (u64, u32) {
                assert!(leaf_index < leaf_count);

                // a) Get the index as if this was a Merkle tree
                let local_mt_height_u32: u32 =
                    tasm::tasmlib_arithmetic_u64_log_2_floor(leaf_index ^ leaf_count);
                let local_mt_leaf_count: u64 = tasm::tasmlib_arithmetic_u64_pow2(local_mt_height_u32);
                let remainder_bitmask: u64 = local_mt_leaf_count - 1;
                let mt_index: u64 = (remainder_bitmask & leaf_index) as u64 + local_mt_leaf_count;

                // b) Find the peak_index (in constant time)
                let all_the_ones: u32 = leaf_count.count_ones();
                let ones_to_subtract: u32 = (leaf_count & remainder_bitmask).count_ones();
                let peak_index: u32 = all_the_ones - ones_to_subtract - 1;

                return (mt_index, peak_index);
            }

            let mut mt_index_and_peak_index: (u64, u32) =
                leaf_index_to_mt_index_and_peak_index(leaf_index, leaf_count);

            let mut acc_hash: Digest = leaf_hash;
            let mut i: usize = 0;
            while mt_index_and_peak_index.0 != 1u64 {
                let ap_element: Digest = auth_path[i];
                if mt_index_and_peak_index.0 % 2u64 == 1u64 {
                    // Node with `acc_hash` is a right child
                    acc_hash = Tip5::hash_pair(ap_element, acc_hash);
                } else {
                    // Node with `acc_hash` is a left child
                    acc_hash = Tip5::hash_pair(acc_hash, ap_element);
                }

                mt_index_and_peak_index.0 /= 2;
                i = i + 1;
            }

            let expected_peak: Digest = peaks[mt_index_and_peak_index.1 as usize];
            return expected_peak == acc_hash;
        }
    })
}

#[cfg(test)]
mod run_tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use proptest::collection::vec;
    use proptest_arbitrary_interop::arb;
    use rand::random;
    use rand::RngCore;
    use tasm_lib::rust_shadowing_helper_functions::list::list_insert;
    use tasm_lib::triton_vm::prelude::*;
    use tasm_lib::twenty_first::math::other::random_elements;
    use tasm_lib::twenty_first::util_types::mmr;
    use tasm_lib::twenty_first::util_types::mmr::mmr_accumulator::util::mmra_with_mps;
    use tasm_lib::twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
    use tasm_lib::twenty_first::util_types::mmr::mmr_membership_proof::MmrMembershipProof;
    use tasm_lib::twenty_first::util_types::mmr::mmr_trait::Mmr;
    use tasm_lib::twenty_first::util_types::mmr::shared_basic::calculate_new_peaks_from_append;
    use tasm_lib::twenty_first::util_types::mmr::shared_basic::calculate_new_peaks_from_leaf_mutation;
    use test_strategy::proptest;

    use super::*;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn right_child_run_test() {
        fn right_child_rast() -> ItemFn {
            item_fn(parse_quote! {
                fn right_child(node_index: u64) -> u64 {
                    return node_index - 1;
                }
            })
        }

        compare_prop_with_stack_safe_lists(
            &right_child_rast(),
            vec![u64_lit(120)],
            vec![u64_lit(119)],
        );
        let mut rng = rand::rng();
        let rand = rng.next_u64();
        compare_prop_with_stack_safe_lists(
            &right_child_rast(),
            vec![u64_lit(rand)],
            vec![u64_lit(rand - 1)],
        );
    }

    #[test]
    fn left_child_run_test() {
        fn left_child_rast() -> ItemFn {
            item_fn(parse_quote! {
                fn left_child(node_index: u64, height: u32) -> u64 {
                    return node_index - (1 << height);
                }
            })
        }

        let inputs0 = vec![u64_lit(120), u32_lit(2)];
        let outputs0 = vec![u64_lit(116)];
        compare_prop_with_stack_safe_lists(&left_child_rast(), inputs0, outputs0);

        let inputs1 = vec![u64_lit(31), u32_lit(4)];
        let outputs1 = vec![u64_lit(15)];
        compare_prop_with_stack_safe_lists(&left_child_rast(), inputs1, outputs1);
    }

    // right_lineage_length_test
    #[test]
    fn right_lineage_length_run_test() {
        fn right_lineage_length_stmt_rast() -> ItemFn {
            item_fn(parse_quote! {
                 fn right_lineage_length(node_index: u64) -> u32 {
                    let bit_width: u32 = tasm::tasmlib_arithmetic_u64_log_2_floor(node_index) + 1u32;
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

        fn right_lineage_length_expr_rast() -> ItemFn {
            item_fn(parse_quote! {
                fn right_lineage_length(node_index: u64) -> u32 {
                    let bit_width: u32 = tasm::tasmlib_arithmetic_u64_log_2_floor(node_index) + 1u32;
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

        fn prop_right_lineage_length_run(node_index: u64, expected: u32) {
            let inputs = vec![u64_lit(node_index)];
            let outputs = vec![u32_lit(expected)];
            compare_prop_with_stack_safe_lists(
                &right_lineage_length_stmt_rast(),
                inputs.clone(),
                outputs.clone(),
            );
            compare_prop_with_stack_safe_lists(&right_lineage_length_expr_rast(), inputs, outputs);
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
        fn leftmost_ancestor_rast() -> ItemFn {
            item_fn(parse_quote! {
                fn leftmost_ancestor(node_index: u64) -> (u64, u32) {
                    let h: u32 = tasm::tasmlib_arithmetic_u64_log_2_floor(node_index);
                    let ret: u64 = (1 << (h + 1)) - 1;

                    return (ret, h);
            }
            })
        }

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

        multiple_compare_prop_with_stack_safe_lists(&leftmost_ancestor_rast(), test_cases);
    }

    #[test]
    fn right_lineage_length_and_own_height_test() {
        fn right_lineage_length_and_own_height_rast() -> ItemFn {
            item_fn(parse_quote! {
                // Obscure graph-traversel algorithm for transforming indices into other kinds of indices
                fn right_lineage_length_and_own_height(node_index: u64) -> (u32, u32) {
                    let leftmost_ancestor_height: u32 = tasm::tasmlib_arithmetic_u64_log_2_floor(node_index);
                    let leftmost_ancestor_node_index: u64 = (1 << (leftmost_ancestor_height + 1)) - 1;
                    let mut candidate_and_candidate_height: (u64, u32) = (leftmost_ancestor_node_index, leftmost_ancestor_height);

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

        multiple_compare_prop_with_stack_safe_lists(
            &right_lineage_length_and_own_height_rast(),
            test_cases,
        );
    }

    #[test]
    fn right_lineage_length_from_leaf_index_test() {
        fn right_lineage_length_from_leaf_index_rast() -> ItemFn {
            item_fn(parse_quote! {
             fn right_lineage_length_from_leaf_index(leaf_index: u64) -> u32 {
                // Identify the last (least significant) nonzero bit
                let pow2: u64 = (leaf_index + 1) & !leaf_index;

                // Get the index of that bit, counting from least significant bit
                return 64 - pow2.leading_zeros() - 1;
            }
            })
        }

        fn right_lineage_length_from_leaf_index_as_local_function() -> ItemFn {
            item_fn(parse_quote! {
                fn right_lineage_lenght_from_leaf_index_outer(leaf_index: u64) -> u32 {
                    fn right_lineage_length_from_leaf_index(leaf_index: u64) -> u32 {
                        // Identify the last (least significant) nonzero bit
                        let pow2: u64 = (leaf_index + 1) & !leaf_index;

                        // Get the index of that bit, counting from least significant bit
                        return 64 - pow2.leading_zeros() - 1;
                    }

                    return right_lineage_length_from_leaf_index(leaf_index);
                }
            })
        }

        let inputs: Vec<u64> = random_elements(40);
        let mut test_cases = inputs
            .into_iter()
            .map(|input| {
                InputOutputTestCase::new(
                    vec![u64_lit(input)],
                    vec![u32_lit(
                        mmr::shared_basic::right_lineage_length_from_leaf_index(input),
                    )],
                )
            })
            .collect_vec();
        test_cases.push(InputOutputTestCase::new(vec![u64_lit(0)], vec![u32_lit(0)]));
        test_cases.push(InputOutputTestCase::new(vec![u64_lit(1)], vec![u32_lit(1)]));
        test_cases.push(InputOutputTestCase::new(vec![u64_lit(2)], vec![u32_lit(0)]));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit((1u64 << 54) - 1)],
            vec![u32_lit(54)],
        ));
        multiple_compare_prop_with_stack_safe_lists(
            &right_lineage_length_from_leaf_index_rast(),
            test_cases.clone(),
        );
        multiple_compare_prop_with_stack_safe_lists(
            &right_lineage_length_from_leaf_index_as_local_function(),
            test_cases,
        );
    }

    #[test]
    fn leaf_index_to_mt_index_run_test() {
        fn leaf_index_to_mt_index_and_peak_index_rast() -> ItemFn {
            item_fn(parse_quote! {
                fn leaf_index_to_mt_index_and_peak_index(leaf_index: u64, leaf_count: u64) -> (u64, u32) {
                    assert!(leaf_index < leaf_count);

                    // a) Get the index as if this was a Merkle tree
                    let local_mt_height_u32: u32 = tasm::tasmlib_arithmetic_u64_log_2_floor(leaf_index ^ leaf_count);
                    let local_mt_leaf_count: u64 = tasm::tasmlib_arithmetic_u64_pow2(local_mt_height_u32);
                    let remainder_bitmask: u64 = local_mt_leaf_count - 1;
                    let mt_index: u64 = (remainder_bitmask & leaf_index) as u64 + local_mt_leaf_count;

                    // b) Find the peak_index (in constant time)
                    let all_the_ones: u32 = leaf_count.count_ones();
                    let ones_to_subtract: u32 = (leaf_count & remainder_bitmask).count_ones();
                    let peak_index: u32 = all_the_ones - ones_to_subtract - 1;

                    return (mt_index, peak_index);
                }
            })
        }

        fn old_leaf_index_to_mt_index_and_peak_index_rast_loops_reduced() -> ItemFn {
            item_fn(parse_quote! {
                fn old_leaf_index_to_mt_index_and_peak_index(leaf_index: u64, leaf_count: u64) -> (u64, u32) {

                    // a) Get the index as if this was a Merkle tree
                    let local_mt_height_u32: u32 = tasm::tasmlib_arithmetic_u64_log_2_floor(leaf_index ^ leaf_count);
                    let local_mt_leaf_count: u64 = tasm::tasmlib_arithmetic_u64_pow2(local_mt_height_u32);
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

        fn add_test(
            (out1, out2): (u64, u32),
            (leaf_count, leaf_index): (u64, u64),
            test_cases: &mut Vec<InputOutputTestCase>,
        ) {
            let inputs = vec![u64_lit(leaf_count), u64_lit(leaf_index)];
            let outputs = vec![u64_lit(out1), u32_lit(out2)];
            test_cases.push(InputOutputTestCase::new(inputs, outputs));
        }

        // Leaf count = 1
        let mut test_cases = vec![];
        add_test((1, 0), (0, 1), &mut test_cases);

        // Leaf count = 2
        add_test((2, 0), (0, 2), &mut test_cases);
        add_test((2, 0), (0, 2), &mut test_cases);
        add_test((3, 0), (1, 2), &mut test_cases);

        // Leaf count = 3
        add_test((2, 0), (0, 3), &mut test_cases);
        add_test((3, 0), (1, 3), &mut test_cases);
        add_test((1, 1), (2, 3), &mut test_cases);

        // Leaf count = 4
        add_test((4, 0), (0, 4), &mut test_cases);
        add_test((5, 0), (1, 4), &mut test_cases);
        add_test((6, 0), (2, 4), &mut test_cases);
        add_test((7, 0), (3, 4), &mut test_cases);

        // Leaf count = 14
        add_test((8, 0), (0, 14), &mut test_cases);
        add_test((9, 0), (1, 14), &mut test_cases);
        add_test((10, 0), (2, 14), &mut test_cases);
        add_test((11, 0), (3, 14), &mut test_cases);
        add_test((12, 0), (4, 14), &mut test_cases);
        add_test((13, 0), (5, 14), &mut test_cases);
        add_test((14, 0), (6, 14), &mut test_cases);
        add_test((15, 0), (7, 14), &mut test_cases);
        add_test((4, 1), (8, 14), &mut test_cases);
        add_test((5, 1), (9, 14), &mut test_cases);
        add_test((6, 1), (10, 14), &mut test_cases);
        add_test((7, 1), (11, 14), &mut test_cases);
        add_test((7, 1), (11, 14), &mut test_cases);

        // Leaf count = 22
        add_test((16, 0), (0, 23), &mut test_cases);
        add_test((17, 0), (1, 23), &mut test_cases);
        add_test((18, 0), (2, 23), &mut test_cases);
        add_test((19, 0), (3, 23), &mut test_cases);
        add_test((30, 0), (14, 23), &mut test_cases);
        add_test((31, 0), (15, 23), &mut test_cases);
        add_test((4, 1), (16, 23), &mut test_cases);
        add_test((5, 1), (17, 23), &mut test_cases);
        add_test((6, 1), (18, 23), &mut test_cases);
        add_test((7, 1), (19, 23), &mut test_cases);
        add_test((2, 2), (20, 23), &mut test_cases);
        add_test((3, 2), (21, 23), &mut test_cases);
        add_test((1, 3), (22, 23), &mut test_cases);

        // Leaf count = 32
        for i in 0..32 {
            add_test((32 + i, 0), (i, 32), &mut test_cases);
        }

        // Leaf count = 33
        for i in 0..32 {
            add_test((32 + i, 0), (i, 33), &mut test_cases);
        }
        add_test((1, 1), (32, 33), &mut test_cases);

        // Leaf count = 34
        for i in 0..32 {
            add_test((32 + i, 0), (i, 34), &mut test_cases);
        }
        add_test((2, 1), (32, 34), &mut test_cases);
        add_test((3, 1), (33, 34), &mut test_cases);

        // Leaf count = 35
        for i in 0..32 {
            add_test((32 + i, 0), (i, 35), &mut test_cases);
        }
        add_test((2, 1), (32, 35), &mut test_cases);
        add_test((3, 1), (33, 35), &mut test_cases);
        add_test((1, 2), (34, 35), &mut test_cases);

        // Leaf count = 36
        for i in 0..32 {
            add_test((32 + i, 0), (i, 36), &mut test_cases);
        }
        add_test((4, 1), (32, 36), &mut test_cases);
        add_test((5, 1), (33, 36), &mut test_cases);
        add_test((6, 1), (34, 36), &mut test_cases);
        add_test((7, 1), (35, 36), &mut test_cases);

        // Leaf count = 37
        for i in 0..32 {
            add_test((32 + i, 0), (i, 37), &mut test_cases);
        }
        add_test((4, 1), (32, 37), &mut test_cases);
        add_test((5, 1), (33, 37), &mut test_cases);
        add_test((6, 1), (34, 37), &mut test_cases);
        add_test((7, 1), (35, 37), &mut test_cases);
        add_test((1, 2), (36, 37), &mut test_cases);

        for i in 10..63 {
            add_test((14 + (1 << i), 0), (14, 1 << i), &mut test_cases);
            add_test((3, 2), ((1 << i) + 9, (1 << i) + 11), &mut test_cases);
            add_test((1, 3), ((1 << i) + 10, (1 << i) + 11), &mut test_cases);
        }

        multiple_compare_prop_with_stack_safe_lists(
            &old_leaf_index_to_mt_index_and_peak_index_rast_loops_reduced(),
            test_cases.clone(),
        );
        multiple_compare_prop_with_stack_safe_lists(
            &leaf_index_to_mt_index_and_peak_index_rast(),
            test_cases,
        );
    }

    fn calculate_new_peaks_from_append_with_local_function_rast() -> ItemFn {
        item_fn(parse_quote! {
            fn calculate_new_peaks_from_append(
                old_leaf_count: u64,
                old_peaks: Vec<Digest>,
                new_leaf: Digest,
            ) -> (Vec<Digest>, Vec<Digest>) {
                fn right_lineage_length_from_leaf_index(leaf_index: u64) -> u32 {
                    // Identify the last (least significant) nonzero bit
                    let pow2: u64 = (leaf_index + 1) & !leaf_index;

                    // Get the index of that bit, counting from least significant bit
                    return 64 - pow2.leading_zeros() - 1;
                }

                // 64 = MAX_MMR_HEIGHT
                let mut peaks: Vec<Digest> = old_peaks;
                peaks.push(new_leaf);
                let mut auth_path: Vec<Digest> = Vec::<Digest>::default();
                let mut right_lineage_count: u32 = right_lineage_length_from_leaf_index(old_leaf_count);
                while right_lineage_count != 0u32 {
                    let new_hash: Digest = peaks.pop().unwrap();
                    let previous_peak: Digest = peaks.pop().unwrap();
                    auth_path.push(previous_peak);
                    peaks.push(Tip5::hash_pair(previous_peak, new_hash));
                    right_lineage_count -= 1;
                }

                return (peaks, auth_path);
            }
        })
    }

    fn calculate_new_peaks_from_append_inlined_rast() -> ItemFn {
        item_fn(parse_quote! {
            fn calculate_new_peaks_from_append(
                old_leaf_count: u64,
                old_peaks: Vec<Digest>,
                new_leaf: Digest,
            ) -> (Vec<Digest>, Vec<Digest>) {
                let mut peaks: Vec<Digest> = old_peaks;
                peaks.push(new_leaf);
                let pow2: u64 = (old_leaf_count + 1) & !old_leaf_count;
                let mut right_lineage_count: u32 = 64 - pow2.leading_zeros() - 1;

                // 64 = MAX_MMR_HEIGHT
                let mut auth_path: Vec<Digest> = Vec::<Digest>::default();
                while right_lineage_count != 0u32 {
                    let new_hash: Digest = peaks.pop().unwrap();
                    let previous_peak: Digest = peaks.pop().unwrap();
                    auth_path.push(previous_peak);
                    peaks.push(Tip5::hash_pair(previous_peak, new_hash));
                    right_lineage_count -= 1;
                }

                return (peaks, auth_path);
            }
        })
    }

    #[proptest(cases = 10)]
    fn calculate_new_peaks_from_append_with_local_function_test(
        #[strategy(vec(arb(), 0..20))] digests: Vec<Digest>,
    ) {
        test_new_peak_calculation_equivalence(
            &calculate_new_peaks_from_append_with_local_function_rast(),
            digests,
        );
    }

    #[proptest(cases = 10)]
    fn calculate_new_peaks_from_append_inlined_test(
        #[strategy(vec(arb(), 0..20))] digests: Vec<Digest>,
    ) {
        test_new_peak_calculation_equivalence(
            &calculate_new_peaks_from_append_inlined_rast(),
            digests,
        );
    }

    fn test_new_peak_calculation_equivalence(src: &ItemFn, digests: Vec<Digest>) {
        let msa = MmrAccumulator::new_from_leafs(digests);
        let mut init_memory = HashMap::default();
        let list_pointer = 10_000u64.into();
        list_insert(list_pointer, msa.peaks(), &mut init_memory);
        let new_leaf: Digest = random();
        let inputs = vec![
            u64_lit(msa.num_leafs()),
            bfe_lit(list_pointer),
            digest_lit(new_leaf),
        ];

        let std_in = vec![];
        let non_determinism = NonDeterminism::default().with_ram(init_memory);
        let state = execute_with_stack_and_ins_safe_lists(src, inputs, std_in, non_determinism, -6)
            .unwrap();

        let (new_peaks, mp) =
            calculate_new_peaks_from_append(msa.num_leafs(), msa.peaks(), new_leaf);

        // Verify that the new peaks calculated in the VM match those calculated in Rust
        assert_list_equal(
            new_peaks.iter().map(|x| digest_lit(*x)).collect_vec(),
            list_pointer,
            &state.ram,
        );

        // Verify that the authentication path calculated in the VM match that calculated in Rust
        let auth_path_pointer = *state.op_stack.stack.last().unwrap();
        let auth_path_list = mp
            .authentication_path
            .iter()
            .map(|&x| digest_lit(x))
            .collect_vec();
        assert_list_equal(auth_path_list, auth_path_pointer, &state.ram);
    }

    fn calculate_new_peaks_from_leaf_mutation_inlined_rast() -> ItemFn {
        item_fn(parse_quote! {
        fn calculate_new_peaks_from_leaf_mutation(
            old_peaks: Vec<Digest>,
            new_leaf: Digest,
            leaf_count: u64,
            authentication_path: Vec<Digest>,
            leaf_index: u64,
        ) -> Vec<Digest> {

            let discrepancies: u64 = leaf_index ^ leaf_count;
            let local_mt_height: u32 = tasm::tasmlib_arithmetic_u64_log_2_floor(discrepancies);
            let local_mt_leaf_count: u64 = tasm::tasmlib_arithmetic_u64_pow2(local_mt_height);
            let remainder_bitmask: u64 = local_mt_leaf_count - 1u64;
            let local_leaf_index: u64 = remainder_bitmask & leaf_index;
            let mut mt_index: u64 = local_leaf_index + local_mt_leaf_count;

            // b) Find the peak_index (in constant time)
            let all_the_ones: u32 = leaf_count.count_ones();
            let ones_to_subtract: u32 = (leaf_count & remainder_bitmask).count_ones();
            let peak_index: u32 = all_the_ones - ones_to_subtract - 1;

            let mut acc_hash: Digest = new_leaf;
            let mut i: usize = 0;
            while mt_index != 1u64 {
                let ap_element: Digest = authentication_path[i];
                if mt_index % 2u64 == 1u64 {
                    // Node with `acc_hash` is a right child
                    acc_hash = Tip5::hash_pair(ap_element, acc_hash);
                } else {
                    // Node with `acc_hash` is a left child
                    acc_hash = Tip5::hash_pair(acc_hash, ap_element);
                }

                mt_index /= 2;
                i = i + 1;
            }

            let mut calculated_peaks: Vec<Digest> = old_peaks;
            calculated_peaks[peak_index as usize] = acc_hash;

            return calculated_peaks;
        }
        })
    }

    fn calculate_new_peaks_from_leaf_mutation_with_local_function_rast() -> ItemFn {
        item_fn(parse_quote! {
            fn calculate_new_peaks_from_leaf_mutation(
                old_peaks: Vec<Digest>,
                new_leaf: Digest,
                leaf_count: u64,
                authentication_path: Vec<Digest>,
                leaf_index: u64,
            ) -> Vec<Digest> {
                fn leaf_index_to_mt_index_and_peak_index(leaf_index: u64, leaf_count: u64) -> (u64, u32) {
                    assert!(leaf_index < leaf_count);

                    // a) Get the index as if this was a Merkle tree
                    let local_mt_height_u32: u32 =
                        tasm::tasmlib_arithmetic_u64_log_2_floor(leaf_index ^ leaf_count);
                    let local_mt_leaf_count: u64 = tasm::tasmlib_arithmetic_u64_pow2(local_mt_height_u32);
                    let remainder_bitmask: u64 = local_mt_leaf_count - 1;
                    let mt_index: u64 = (remainder_bitmask & leaf_index) as u64 + local_mt_leaf_count;

                    // b) Find the peak_index (in constant time)
                    let all_the_ones: u32 = leaf_count.count_ones();
                    let ones_to_subtract: u32 = (leaf_count & remainder_bitmask).count_ones();
                    let peak_index: u32 = all_the_ones - ones_to_subtract - 1;

                    return (mt_index, peak_index);
                }

                let mut mt_index_and_peak_index: (u64, u32) =
                    leaf_index_to_mt_index_and_peak_index(leaf_index, leaf_count);
                let mut acc_hash: Digest = new_leaf;
                let mut i: usize = 0;
                while mt_index_and_peak_index.0 != 1u64 {
                    let ap_element: Digest = authentication_path[i];
                    if mt_index_and_peak_index.0 % 2u64 == 1u64 {
                        // Node with `acc_hash` is a right child
                        acc_hash = Tip5::hash_pair(ap_element, acc_hash);
                    } else {
                        // Node with `acc_hash` is a left child
                        acc_hash = Tip5::hash_pair(acc_hash, ap_element);
                    }

                    mt_index_and_peak_index.0 /= 2;
                    i = i + 1;
                }

                let mut calculated_peaks: Vec<Digest> = old_peaks;
                calculated_peaks[mt_index_and_peak_index.1 as usize] = acc_hash;

                return calculated_peaks;
            }
        })
    }

    #[proptest(cases = 20)]
    fn calculate_new_peaks_from_leaf_mutatation_inlined_test(
        #[strategy(vec(arb(), 1..45))] digests: Vec<Digest>,
        #[strategy(0..# digests.len() as u64)] leaf_index: u64,
        #[strategy(arb())] new_leaf: Digest,
    ) {
        let (code, _) =
            compile_for_run_test(&calculate_new_peaks_from_leaf_mutation_inlined_rast());
        test_mmr_leaf_mutation_equivalence(&code, digests, leaf_index, new_leaf);
    }

    #[proptest(cases = 20)]
    fn calculate_new_peaks_from_leaf_mutatation_with_local_function_test(
        #[strategy(vec(arb(), 1..45))] digests: Vec<Digest>,
        #[strategy(0..# digests.len() as u64)] leaf_index: u64,
        #[strategy(arb())] new_leaf: Digest,
    ) {
        let (code, _) = compile_for_run_test(
            &calculate_new_peaks_from_leaf_mutation_with_local_function_rast(),
        );
        test_mmr_leaf_mutation_equivalence(&code, digests, leaf_index, new_leaf);
    }

    fn test_mmr_leaf_mutation_equivalence(
        code: &[LabelledInstruction],
        digests: Vec<Digest>,
        index_of_leaf_to_change: u64,
        new_leaf: Digest,
    ) {
        let (mmra, mps) = mmra_with_mps(
            digests.len() as u64,
            digests
                .into_iter()
                .enumerate()
                .map(|(i, x)| (i as u64, x))
                .collect_vec(),
        );

        let mut init_memory = HashMap::default();
        let old_peaks_pointer: BFieldElement = 10000u64.into();
        let old_peaks = mmra.peaks();
        list_insert(old_peaks_pointer, old_peaks.clone(), &mut init_memory);
        let ap_pointer: BFieldElement = 20000u64.into();
        let old_mp = mps[index_of_leaf_to_change as usize].clone();
        list_insert(
            ap_pointer,
            old_mp.authentication_path.clone(),
            &mut init_memory,
        );
        let inputs = vec![
            bfe_lit(old_peaks_pointer),
            digest_lit(new_leaf),
            u64_lit(mmra.num_leafs()),
            bfe_lit(ap_pointer),
            u64_lit(index_of_leaf_to_change),
        ];
        let non_determinism = NonDeterminism::default().with_ram(init_memory);
        let expected_stack_diff = -10;
        let vm_state = execute_compiled_with_stack_and_ins_for_test(
            code,
            inputs,
            vec![],
            non_determinism,
            expected_stack_diff,
        )
        .unwrap();

        let expected_new_peaks = calculate_new_peaks_from_leaf_mutation(
            &old_peaks,
            mmra.num_leafs(),
            new_leaf,
            index_of_leaf_to_change,
            &old_mp,
        );

        // Verify that the new peaks calculated in the VM match those calculated in Rust
        assert_list_equal(
            expected_new_peaks
                .iter()
                .map(|x| digest_lit(*x))
                .collect_vec(),
            old_peaks_pointer,
            &vm_state.ram,
        );

        // Sanity check
        assert_ne!(
            expected_new_peaks, old_peaks,
            "Mutating a leaf must mutate peaks list"
        );
    }

    #[test]
    fn verify_authentication_path_test() {
        let (code_inlined_function, _fn_name) =
            compile_for_run_test(&verify_authentication_path_inlined());
        let (code_local_function, _fn_name) =
            compile_for_run_test(&verify_authentication_path_with_local_function());
        for code in [code_inlined_function, code_local_function] {
            for size in 1..19 {
                let digests: Vec<Digest> = random_elements(size);
                let (mmra, mps) = mmra_with_mps(
                    digests.len() as u64,
                    digests
                        .clone()
                        .into_iter()
                        .enumerate()
                        .map(|(i, x)| (i as u64, x))
                        .collect_vec(),
                );
                let leaf_index = random::<u64>() % size as u64;

                let mut init_memory = HashMap::default();
                let peaks_pointer: BFieldElement = 10000u64.into();
                let peaks = mmra.peaks();
                list_insert(peaks_pointer, peaks.clone(), &mut init_memory);
                let ap_pointer: BFieldElement = 20_000u64.into();
                let mp: MmrMembershipProof = mps[leaf_index as usize].clone();
                list_insert(ap_pointer, mp.authentication_path.clone(), &mut init_memory);

                let own_leaf = digests[leaf_index as usize];
                let good_inputs = vec![
                    bfe_lit(peaks_pointer),
                    u64_lit(mmra.num_leafs()),
                    bfe_lit(ap_pointer),
                    u64_lit(leaf_index),
                    digest_lit(own_leaf),
                ];

                // Positive test
                let std_in = vec![];
                let non_determinism = NonDeterminism::default().with_ram(init_memory);
                let vm_res = match execute_compiled_with_stack_and_ins_for_test(
                    &code,
                    good_inputs,
                    std_in.clone(),
                    non_determinism.clone(),
                    -10,
                ) {
                    Ok(vm_res) => vm_res,
                    Err(err) => panic!("VM execution must succeed. Got: {err}"),
                };

                let expected_result = mp.verify(leaf_index, own_leaf, &peaks, mmra.num_leafs());
                let vm_result: bool = vm_res.op_stack.stack.last().unwrap().value() == 1;
                assert_eq!(
                    expected_result, vm_result,
                    "VM result must agree with Rust result"
                );
                assert!(vm_result, "VM verdict must be positive in positive test");

                // Negative test
                let bad_leaf: Digest = random();
                let bad_inputs = vec![
                    bfe_lit(peaks_pointer),
                    u64_lit(mmra.num_leafs()),
                    bfe_lit(ap_pointer),
                    u64_lit(leaf_index),
                    digest_lit(bad_leaf),
                ];
                let vm_res_negative = match execute_compiled_with_stack_and_ins_for_test(
                    &code,
                    bad_inputs,
                    std_in,
                    non_determinism,
                    -10,
                ) {
                    Ok(vm_res) => vm_res,
                    Err(err) => panic!("VM execution must succeed. Got: {err}"),
                };
                let expected_result_neg = mp.verify(leaf_index, bad_leaf, &peaks, mmra.num_leafs());
                let vm_result_neg: bool =
                    vm_res_negative.op_stack.stack.last().unwrap().value() == 1;
                assert_eq!(
                    expected_result_neg, vm_result_neg,
                    "VM result must agree with Rust result"
                );
                assert!(
                    !vm_result_neg,
                    "VM verdict in negative test must be negative"
                );
            }
        }

        fn verify_authentication_path_inlined() -> ItemFn {
            item_fn(parse_quote! {
                fn verify_authentication_path(
                    peaks: Vec<Digest>,
                    leaf_count: u64,
                    auth_path: Vec<Digest>,
                    leaf_index: u64,
                    leaf_hash: Digest,
                ) -> bool {
                    // Find the mt_index in constant time
                    let discrepancies: u64 = leaf_index ^ leaf_count;
                    let local_mt_height: u32 = tasm::tasmlib_arithmetic_u64_log_2_floor(discrepancies);
                    let local_mt_leaf_count: u64 = tasm::tasmlib_arithmetic_u64_pow2(local_mt_height);
                    let remainder_bitmask: u64 = local_mt_leaf_count - 1u64;
                    let local_leaf_index: u64 = remainder_bitmask & leaf_index;
                    let mut mt_index: u64 = local_leaf_index + local_mt_leaf_count;

                    // b) Find the peak_index (in constant time)
                    let all_the_ones: u32 = leaf_count.count_ones();
                    let ones_to_subtract: u32 = (leaf_count & remainder_bitmask).count_ones();
                    let peak_index: u32 = all_the_ones - ones_to_subtract - 1;

                    let mut acc_hash: Digest = leaf_hash;
                    let mut i: usize = 0;
                    while mt_index != 1u64 {
                        let ap_element: Digest = auth_path[i];
                        if mt_index % 2u64 == 1u64 {
                            // Node with `acc_hash` is a right child
                            acc_hash = Tip5::hash_pair(ap_element, acc_hash);
                        } else {
                            // Node with `acc_hash` is a left child
                            acc_hash = Tip5::hash_pair(acc_hash, ap_element);
                        }

                        mt_index /= 2;
                        i = i + 1;
                    }

                    let expected_peak: Digest = peaks[peak_index as usize];
                    return expected_peak == acc_hash;
                }
            })
        }
    }
}
