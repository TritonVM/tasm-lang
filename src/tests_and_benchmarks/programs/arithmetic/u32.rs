use syn::parse_quote;

use crate::graft::item_fn;

#[allow(dead_code)]
fn declare_u32_max_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn declare_u32_max() {
            let a: u32 = u32::MAX;
            return;
        }
    })
}

#[allow(dead_code)]
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

#[allow(dead_code)]
fn add_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
fn add_u32_overwrite_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_u32_overwrite(lhs: u32, rhs: u32) -> u32 {
            let mut c: u32 = lhs + rhs;
            c = c + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
fn simple_sub_u32() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn simple_sub(a: u32, b: u32) -> u32 {
            return a - b;
        }
    })
}

#[allow(dead_code)]
fn sub_u32_rast_1() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
fn sub_u32_rast_2() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u32(rhs: u32, lhs: u32) -> u32 {
            let c: u32 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
fn mul_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn mul_u32(rhs: u32, lhs: u32) -> u32 {
            return rhs * lhs;
        }
    })
}

#[allow(dead_code)]
fn rem_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn rem_u32(rhs: u32, lhs: u32) -> u32 {
            return rhs % lhs;
        }
    })
}

#[allow(dead_code)]
fn div_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn rem_u32(rhs: u32, lhs: u32) -> u32 {
            return rhs / lhs;
        }
    })
}

#[allow(dead_code)]
fn bitwise_and_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn bitwise_and_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs & rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
fn operator_evaluation_ordering_with_div_u32() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_div() -> u32 {
            return 100u32 - 14u32 / 2u32 + 1u32;
        }
    })
}

#[allow(dead_code)]
fn operator_evaluation_ordering_with_mul() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_mul() -> u32 {
            return 380u32 - 14u32 * 2u32 * 10u32 + 1u32 - 41u32 * 1u32;
        }
    })
}

#[allow(dead_code)]
fn lt_u32_dynamic_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn lt_u32_dynamic(lhs: u32, rhs: u32) -> bool {
            return lhs < rhs;
        }
    })
}

#[allow(dead_code)]
fn gt_u32_dynamic_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn gt_u32_dynamic(lhs: u32, rhs: u32) -> bool {
            return lhs > rhs;
        }
    })
}

#[allow(dead_code)]
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

#[allow(dead_code)]
fn leftshift_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn leftshift_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs << rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
fn rightshift_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn rightshift_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs >> rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
fn usize_as_alias_for_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn usize_as_alias_for_u32(lhs: usize, rhs: u32) -> usize {
            let c: u32 = lhs + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
fn leading_zeros_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn leading_zeros_u32(value: u32) -> u32 {
            return value.leading_zeros();
        }
    })
}

#[allow(dead_code)]
fn count_ones_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn count_ones_u32(value: u32) -> u32 {
            return value.count_ones();
        }
    })
}

#[allow(dead_code)]
fn bitwise_not_return_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn bitwise_not(value: u32) -> u32 {
            return !value;
        }
    })
}

#[allow(dead_code)]
fn bitwise_not_assign_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn bitwise_not(value: u32) -> u32 {
            let ret: u32 = !value;
            return ret;
        }
    })
}

#[cfg(test)]
mod run_tests {
    use itertools::Itertools;
    use rand::{thread_rng, RngCore};
    use twenty_first::shared_math::other::random_elements;

    use super::*;
    use crate::tests_and_benchmarks::shared_test::*;

    #[test]
    fn simple_sub_test() {
        compare_prop_with_stack(
            &simple_sub_u32(),
            vec![u32_lit(100), u32_lit(51)],
            vec![u32_lit(49)],
        );
    }

    #[test]
    fn declare_u32_test() {
        compare_prop_with_stack(
            &declare_u32s_as_lits_rast(),
            vec![],
            vec![
                u32_lit(123_321u32),
                u32_lit(123456789u32),
                u32_lit(0xcafe_babeu32),
            ],
        );
    }

    #[test]
    fn sub_u32_run_test() {
        let input_args_1 = vec![u32_lit(200), u32_lit(95)];
        let expected_outputs_1 = vec![u32_lit(105)];
        compare_prop_with_stack(&sub_u32_rast_1(), input_args_1, expected_outputs_1);

        let input_args_2 = vec![u32_lit(95), u32_lit(200)];
        let expected_outputs_2 = vec![u32_lit(105)];
        compare_prop_with_stack(&sub_u32_rast_2(), input_args_2, expected_outputs_2);
    }

    #[test]
    fn mul_u32_run_test() {
        let input_args_1 = vec![u32_lit(200), u32_lit(100)];
        let expected_outputs_1 = vec![u32_lit(20_000)];
        compare_prop_with_stack(&mul_u32_rast(), input_args_1, expected_outputs_1);
    }

    #[test]
    fn div_rem_u32_run_test() {
        let mut rng = thread_rng();
        for _ in 0..100 {
            let numerator = rng.next_u32();
            let divisor = rng.next_u32();
            let input_args_1 = vec![u32_lit(numerator), u32_lit(divisor)];
            compare_prop_with_stack(
                &rem_u32_rast(),
                input_args_1.clone(),
                vec![u32_lit(numerator % divisor)],
            );
            compare_prop_with_stack(
                &div_u32_rast(),
                input_args_1,
                vec![u32_lit(numerator / divisor)],
            );
        }
    }

    #[test]
    fn cmp_u32_dynamic_test() {
        let mut rng = thread_rng();
        for _ in 0..10 {
            let lhs = rng.next_u32();
            let rhs = rng.next_u32();
            compare_prop_with_stack(
                &lt_u32_dynamic_rast(),
                vec![u32_lit(lhs), u32_lit(rhs)],
                vec![bool_lit(lhs < rhs)],
            );

            compare_prop_with_stack(
                &lt_u32_dynamic_rast(),
                vec![u32_lit(lhs), u32_lit(lhs)],
                vec![bool_lit(false)],
            );

            compare_prop_with_stack(
                &gt_u32_dynamic_rast(),
                vec![u32_lit(lhs), u32_lit(rhs)],
                vec![bool_lit(lhs > rhs)],
            );

            compare_prop_with_stack(
                &gt_u32_dynamic_rast(),
                vec![u32_lit(lhs), u32_lit(lhs)],
                vec![bool_lit(false)],
            );
        }

        // 0 vs 0
        compare_prop_with_stack(
            &lt_u32_dynamic_rast(),
            vec![u32_lit(0), u32_lit(0)],
            vec![bool_lit(false)],
        );
        compare_prop_with_stack(
            &gt_u32_dynamic_rast(),
            vec![u32_lit(0), u32_lit(0)],
            vec![bool_lit(false)],
        );

        // 0 vs 1
        compare_prop_with_stack(
            &lt_u32_dynamic_rast(),
            vec![u32_lit(0), u32_lit(1)],
            vec![bool_lit(true)],
        );
        compare_prop_with_stack(
            &gt_u32_dynamic_rast(),
            vec![u32_lit(0), u32_lit(1)],
            vec![bool_lit(false)],
        );

        // 1 vs 0
        compare_prop_with_stack(
            &lt_u32_dynamic_rast(),
            vec![u32_lit(1), u32_lit(0)],
            vec![bool_lit(false)],
        );
        compare_prop_with_stack(
            &gt_u32_dynamic_rast(),
            vec![u32_lit(1), u32_lit(0)],
            vec![bool_lit(true)],
        );

        // 1 vs 1
        compare_prop_with_stack(
            &lt_u32_dynamic_rast(),
            vec![u32_lit(1), u32_lit(1)],
            vec![bool_lit(false)],
        );
        compare_prop_with_stack(
            &gt_u32_dynamic_rast(),
            vec![u32_lit(1), u32_lit(1)],
            vec![bool_lit(false)],
        );
    }

    #[test]
    fn lt_u32_static_test() {
        compare_prop_with_stack(&lt_u32_static(), vec![], vec![bool_lit(true)]);
    }

    #[test]
    fn leftshift_u32_run_test() {
        let input = vec![u32_lit(0b101010101010101u32), u32_lit(16u32)];
        let expected_output = vec![u32_lit(1431633920)];
        compare_prop_with_stack(&leftshift_u32_rast(), input, expected_output);
    }

    #[test]
    fn rightshift_u32_run_test() {
        let input = vec![u32_lit(0b101010101010101u32), u32_lit(3u32)];
        let expected_output = vec![u32_lit(2730)];
        compare_prop_with_stack(&rightshift_u32_rast(), input, expected_output);
    }

    #[test]
    fn operator_evaluation_ordering_test() {
        compare_prop_with_stack(
            &operator_evaluation_ordering_with_div_u32(),
            vec![],
            vec![u32_lit(94)],
        );

        compare_prop_with_stack(
            &operator_evaluation_ordering_with_mul(),
            vec![],
            vec![u32_lit(60)],
        );
    }

    #[test]
    fn usize_as_alias_for_u32_test() {
        compare_prop_with_stack(
            &usize_as_alias_for_u32_rast(),
            vec![u32_lit(1 << 25), u32_lit(1 << 27)],
            vec![u32_lit((1 << 25) + (1 << 27))],
        );
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
        multiple_compare_prop_with_stack(&leading_zeros_u32_rast(), test_cases);
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
        multiple_compare_prop_with_stack(&count_ones_u32_rast(), test_cases);
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
        multiple_compare_prop_with_stack(&bitwise_not_return_rast(), test_cases.clone());
        multiple_compare_prop_with_stack(&bitwise_not_assign_rast(), test_cases);
    }
}

#[cfg(test)]
mod compile_and_typecheck_tests {
    use crate::tests_and_benchmarks::shared_test::graft_check_compile_prop;

    use super::*;

    #[test]
    fn declare_u32_max_test() {
        graft_check_compile_prop(&declare_u32_max_rast());
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
    fn bitwise_and_u32_test() {
        graft_check_compile_prop(&bitwise_and_u32_rast());
    }

    #[test]
    fn lt_u32_test() {
        graft_check_compile_prop(&lt_u32_static());
    }

    #[test]
    fn leftshift_u32_test() {
        graft_check_compile_prop(&leftshift_u32_rast());
    }

    #[test]
    fn rightshift_u32_test() {
        graft_check_compile_prop(&rightshift_u32_rast());
    }

    #[test]
    fn usize_as_alias_for_u32_test() {
        graft_check_compile_prop(&usize_as_alias_for_u32_rast());
    }
}
