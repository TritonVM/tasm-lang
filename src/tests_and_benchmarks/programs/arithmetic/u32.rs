use syn::parse_quote;

use crate::graft::item_fn;

#[cfg(test)]
mod run_tests {
    use itertools::Itertools;
    use rand::{random, thread_rng, RngCore};
    use twenty_first::shared_math::other::random_elements;

    use super::*;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn simple_sub_test() {
        compare_prop_with_stack_safe_lists(
            &simple_sub_u32(),
            vec![u32_lit(100), u32_lit(51)],
            vec![u32_lit(49)],
        );

        fn simple_sub_u32() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn simple_sub(a: u32, b: u32) -> u32 {
                    return a - b;
                }
            })
        }
    }

    #[test]
    fn declare_u32_test() {
        compare_prop_with_stack_safe_lists(
            &declare_u32s_as_lits_rast(),
            vec![],
            vec![
                u32_lit(123_321u32),
                u32_lit(123456789u32),
                u32_lit(0xcafe_babeu32),
            ],
        );

        fn declare_u32s_as_lits_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn declare_u32s_as_lits() -> (u32, u32, u32) {
                    let a: u32 = 123_321u32;
                    let b: u32 = 123456789u32;
                    let c: u32 = 0xcafe_babeu32;
                    return (a, b, c);
                }
            })
        }
    }

    #[test]
    fn declare_u32_max_test() {
        compare_prop_with_stack_safe_lists(
            &declare_u32_max_rast(),
            vec![],
            vec![u32_lit(0xffff_ffff)],
        );

        fn declare_u32_max_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn declare_u32_max() -> u32 {
                    let a: u32 = u32::MAX;
                    return a;
                }
            })
        }
    }

    #[test]
    fn add_u32_test() {
        fn add_u32_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn add_u32(lhs: u32, rhs: u32) -> u32 {
                    let c: u32 = lhs + rhs;
                    return c;
                }
            })
        }
        compare_prop_with_stack_safe_lists(
            &add_u32_rast(),
            vec![u32_lit(123_321u32), u32_lit(432_234u32)],
            vec![u32_lit(555_555u32)],
        );
    }

    #[test]
    fn add_u32_overwrite_test() {
        fn add_u32_overwrite_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn add_u32_overwrite(lhs: u32, rhs: u32) -> u32 {
                    let mut c: u32 = lhs + rhs;
                    c = c + rhs;
                    return c;
                }
            })
        }
        compare_prop_with_stack_safe_lists(
            &add_u32_overwrite_rast(),
            vec![u32_lit(555_555u32), u32_lit(111_111u32)],
            vec![u32_lit(777_777u32)],
        );
    }

    #[test]
    fn sub_u32_run_test() {
        let input_args_1 = vec![u32_lit(200), u32_lit(95)];
        let expected_outputs_1 = vec![u32_lit(105)];
        compare_prop_with_stack_safe_lists(&sub_u32_rast_1(), input_args_1, expected_outputs_1);

        let input_args_2 = vec![u32_lit(95), u32_lit(200)];
        let expected_outputs_2 = vec![u32_lit(105)];
        compare_prop_with_stack_safe_lists(&sub_u32_rast_2(), input_args_2, expected_outputs_2);

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
                    return lhs - rhs;
                }
            })
        }
    }

    #[test]
    fn mul_u32_run_test() {
        let input_args_1 = vec![u32_lit(200), u32_lit(100)];
        let expected_outputs_1 = vec![u32_lit(20_000)];
        compare_prop_with_stack_safe_lists(&mul_u32_rast(), input_args_1, expected_outputs_1);

        fn mul_u32_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn mul_u32(rhs: u32, lhs: u32) -> u32 {
                    return rhs * lhs;
                }
            })
        }
    }

    #[test]
    fn div_rem_u32_run_test() {
        let mut rng = thread_rng();
        for _ in 0..100 {
            let numerator = rng.next_u32();
            let divisor = rng.next_u32();
            let input_args_1 = vec![u32_lit(numerator), u32_lit(divisor)];
            compare_prop_with_stack_safe_lists(
                &rem_u32_rast(),
                input_args_1.clone(),
                vec![u32_lit(numerator % divisor)],
            );
            compare_prop_with_stack_safe_lists(
                &div_u32_rast(),
                input_args_1,
                vec![u32_lit(numerator / divisor)],
            );
        }

        fn rem_u32_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn rem_u32(rhs: u32, lhs: u32) -> u32 {
                    return rhs % lhs;
                }
            })
        }

        fn div_u32_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn rem_u32(rhs: u32, lhs: u32) -> u32 {
                    return rhs / lhs;
                }
            })
        }
    }

    #[test]
    fn bitwise_and_u32_test() {
        let lhs: u32 = random();
        let rhs: u32 = random();
        compare_prop_with_stack_safe_lists(
            &bitwise_and_u32_rast(),
            vec![u32_lit(lhs), u32_lit(rhs)],
            vec![u32_lit(lhs & rhs)],
        );

        fn bitwise_and_u32_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn bitwise_and_u32(lhs: u32, rhs: u32) -> u32 {
                    let c: u32 = lhs & rhs;
                    return c;
                }
            })
        }
    }

    #[test]
    fn cmp_u32_dynamic_test() {
        let mut rng = thread_rng();
        for _ in 0..10 {
            let lhs = rng.next_u32();
            let rhs = rng.next_u32();
            compare_prop_with_stack_safe_lists(
                &lt_u32_dynamic_rast(),
                vec![u32_lit(lhs), u32_lit(rhs)],
                vec![bool_lit(lhs < rhs)],
            );

            compare_prop_with_stack_safe_lists(
                &lt_u32_dynamic_rast(),
                vec![u32_lit(lhs), u32_lit(lhs)],
                vec![bool_lit(false)],
            );

            compare_prop_with_stack_safe_lists(
                &gt_u32_dynamic_rast(),
                vec![u32_lit(lhs), u32_lit(rhs)],
                vec![bool_lit(lhs > rhs)],
            );

            compare_prop_with_stack_safe_lists(
                &gt_u32_dynamic_rast(),
                vec![u32_lit(lhs), u32_lit(lhs)],
                vec![bool_lit(false)],
            );
        }

        // 0 vs 0
        compare_prop_with_stack_safe_lists(
            &lt_u32_dynamic_rast(),
            vec![u32_lit(0), u32_lit(0)],
            vec![bool_lit(false)],
        );
        compare_prop_with_stack_safe_lists(
            &gt_u32_dynamic_rast(),
            vec![u32_lit(0), u32_lit(0)],
            vec![bool_lit(false)],
        );

        // 0 vs 1
        compare_prop_with_stack_safe_lists(
            &lt_u32_dynamic_rast(),
            vec![u32_lit(0), u32_lit(1)],
            vec![bool_lit(true)],
        );
        compare_prop_with_stack_safe_lists(
            &gt_u32_dynamic_rast(),
            vec![u32_lit(0), u32_lit(1)],
            vec![bool_lit(false)],
        );

        // 1 vs 0
        compare_prop_with_stack_safe_lists(
            &lt_u32_dynamic_rast(),
            vec![u32_lit(1), u32_lit(0)],
            vec![bool_lit(false)],
        );
        compare_prop_with_stack_safe_lists(
            &gt_u32_dynamic_rast(),
            vec![u32_lit(1), u32_lit(0)],
            vec![bool_lit(true)],
        );

        // 1 vs 1
        compare_prop_with_stack_safe_lists(
            &lt_u32_dynamic_rast(),
            vec![u32_lit(1), u32_lit(1)],
            vec![bool_lit(false)],
        );
        compare_prop_with_stack_safe_lists(
            &gt_u32_dynamic_rast(),
            vec![u32_lit(1), u32_lit(1)],
            vec![bool_lit(false)],
        );

        fn lt_u32_dynamic_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn lt_u32_dynamic(lhs: u32, rhs: u32) -> bool {
                    return lhs < rhs;
                }
            })
        }

        fn gt_u32_dynamic_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn gt_u32_dynamic(lhs: u32, rhs: u32) -> bool {
                    return lhs > rhs;
                }
            })
        }
    }

    #[test]
    fn lt_u32_static_test() {
        compare_prop_with_stack_safe_lists(&lt_u32_static(), vec![], vec![bool_lit(true)]);

        fn lt_u32_static() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn lt_u32_static() -> bool {
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
    }

    #[test]
    fn leftshift_u32_run_test() {
        let input = vec![u32_lit(0b101010101010101u32), u32_lit(16u32)];
        let expected_output = vec![u32_lit(1431633920)];
        compare_prop_with_stack_safe_lists(&leftshift_u32_rast(), input, expected_output);

        fn leftshift_u32_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn leftshift_u32(lhs: u32, rhs: u32) -> u32 {
                    let c: u32 = lhs << rhs;
                    return c;
                }
            })
        }
    }

    #[test]
    fn rightshift_u32_run_test() {
        let input = vec![u32_lit(0b101010101010101u32), u32_lit(3u32)];
        let expected_output = vec![u32_lit(2730)];
        compare_prop_with_stack_safe_lists(&rightshift_u32_rast(), input, expected_output);

        fn rightshift_u32_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn rightshift_u32(lhs: u32, rhs: u32) -> u32 {
                    let c: u32 = lhs >> rhs;
                    return c;
                }
            })
        }
    }

    #[test]
    fn operator_evaluation_ordering_test() {
        compare_prop_with_stack_safe_lists(
            &operator_evaluation_ordering_with_div_u32(),
            vec![],
            vec![u32_lit(94)],
        );

        compare_prop_with_stack_safe_lists(
            &operator_evaluation_ordering_with_mul(),
            vec![],
            vec![u32_lit(60)],
        );

        fn operator_evaluation_ordering_with_div_u32() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn complicated_expression_with_div() -> u32 {
                    return 100u32 - 14u32 / 2u32 + 1u32;
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
    }

    #[test]
    fn usize_as_alias_for_u32_test() {
        compare_prop_with_stack_safe_lists(
            &usize_as_alias_for_u32_rast(),
            vec![u32_lit(1 << 25), u32_lit(1 << 27)],
            vec![u32_lit((1 << 25) + (1 << 27))],
        );

        fn usize_as_alias_for_u32_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn usize_as_alias_for_u32(lhs: usize, rhs: u32) -> usize {
                    let c: u32 = lhs + rhs;
                    return c;
                }
            })
        }
    }

    #[test]
    fn leading_zeros_u32_test() {
        let values: Vec<u32> = random_elements(20);
        let mut test_cases = values
            .iter()
            .map(|value| {
                InputOutputTestCase::new(
                    vec![u32_lit(*value)],
                    vec![u32_lit(value.leading_zeros())],
                )
            })
            .collect_vec();
        test_cases.push(InputOutputTestCase::new(
            vec![u32_lit(0)],
            vec![u32_lit(32)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u32_lit(1)],
            vec![u32_lit(31)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u32_lit(2)],
            vec![u32_lit(30)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u32_lit(3)],
            vec![u32_lit(30)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u32_lit(u32::MAX)],
            vec![u32_lit(0)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u32_lit(1u32 << 31)],
            vec![u32_lit(0)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u32_lit((1u32 << 31) + 1)],
            vec![u32_lit(0)],
        ));
        multiple_compare_prop_with_stack_safe_lists(&leading_zeros_u32_rast(), test_cases);

        fn leading_zeros_u32_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn leading_zeros_u32(value: u32) -> u32 {
                    return value.leading_zeros();
                }
            })
        }
    }

    #[test]
    fn count_ones_u32_test() {
        let values: Vec<u32> = random_elements(40);
        let mut test_cases = values
            .iter()
            .map(|value| {
                InputOutputTestCase::new(vec![u32_lit(*value)], vec![u32_lit(value.count_ones())])
            })
            .collect_vec();
        test_cases.push(InputOutputTestCase::new(vec![u32_lit(0)], vec![u32_lit(0)]));
        test_cases.push(InputOutputTestCase::new(
            vec![u32_lit(u32::MAX)],
            vec![u32_lit(32)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u32_lit(u32::MAX - 1)],
            vec![u32_lit(31)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u32_lit((1u32 << 31) - 1)],
            vec![u32_lit(31)],
        ));
        for j in 0..32 {
            test_cases.push(InputOutputTestCase::new(
                vec![u32_lit((u32::MAX) << j)],
                vec![u32_lit(32 - j)],
            ));
            test_cases.push(InputOutputTestCase::new(
                vec![u32_lit((u32::MAX) >> j)],
                vec![u32_lit(32 - j)],
            ));
        }
        multiple_compare_prop_with_stack_safe_lists(&count_ones_u32_rast(), test_cases);

        fn count_ones_u32_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn count_ones_u32(value: u32) -> u32 {
                    return value.count_ones();
                }
            })
        }
    }

    #[test]
    fn bitwise_not_u32_test() {
        let values: Vec<u32> = random_elements(10);
        let mut test_cases = values
            .iter()
            .map(|value| InputOutputTestCase::new(vec![u32_lit(*value)], vec![u32_lit(!value)]))
            .collect_vec();
        test_cases.push(InputOutputTestCase::new(
            vec![u32_lit(0)],
            vec![u32_lit(u32::MAX)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u32_lit(u32::MAX)],
            vec![u32_lit(0)],
        ));
        multiple_compare_prop_with_stack_safe_lists(&bitwise_not_return_rast(), test_cases.clone());
        multiple_compare_prop_with_stack_safe_lists(&bitwise_not_assign_rast(), test_cases);

        fn bitwise_not_return_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn bitwise_not(value: u32) -> u32 {
                    return !value;
                }
            })
        }

        fn bitwise_not_assign_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn bitwise_not(value: u32) -> u32 {
                    let ret: u32 = !value;
                    return ret;
                }
            })
        }
    }
}
