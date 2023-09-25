#[cfg(test)]
mod run_tests {
    use itertools::Itertools;
    use rand::{thread_rng, Rng};
    use syn::parse_quote;
    use twenty_first::shared_math::other::random_elements;

    use crate::{graft::item_fn, tests_and_benchmarks::test_helpers::shared_test::*};

    #[test]
    fn declare_u128_max_and_bits_test() {
        compare_prop_with_stack_safe_lists(
            &declare_u128_max_and_bits_rast(),
            vec![],
            vec![
                u128_lit(0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff),
                u32_lit(0x0000_0080),
            ],
        );

        fn declare_u128_max_and_bits_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn declare_u128_max_and_bits() -> (u128, u32) {
                    let a: u128 = u128::MAX;
                    let b: u32 = u128::BITS;
                    return (a, b);
                }
            })
        }
    }

    #[test]
    fn u128_add_test() {
        let values_lhs: Vec<u128> = random_elements::<u128>(10)
            .into_iter()
            .map(|x| x / 2)
            .collect_vec();
        let values_rhs: Vec<u128> = random_elements::<u128>(10)
            .into_iter()
            .map(|x| x / 2)
            .collect_vec();
        let mut test_cases = values_lhs
            .into_iter()
            .zip(values_rhs)
            .map(|(lhs, rhs)| {
                InputOutputTestCase::new(
                    vec![u128_lit(lhs), u128_lit(rhs)],
                    vec![u128_lit(lhs + rhs)],
                )
            })
            .collect_vec();
        test_cases.push(InputOutputTestCase::new(
            vec![u128_lit(0), u128_lit(0)],
            vec![u128_lit(0)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u128_lit(u128::MAX), u128_lit(0)],
            vec![u128_lit(u128::MAX)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u128_lit(u128::MAX - 1), u128_lit(1)],
            vec![u128_lit(u128::MAX)],
        ));
        multiple_compare_prop_with_stack_safe_lists(&u128_add_rast(), test_cases);

        fn u128_add_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn u128_add_test_fn(lhs: u128, rhs: u128) -> u128 {
                    return lhs + rhs;
                }
            })
        }
    }

    #[test]
    fn mul_two_u64s_run_test() {
        let mut test_cases = vec![
            InputOutputTestCase::new(vec![u64_lit(0), u64_lit(0)], vec![u128_lit(0)]),
            InputOutputTestCase::new(vec![u64_lit(1), u64_lit(0)], vec![u128_lit(0)]),
            InputOutputTestCase::new(vec![u64_lit(0), u64_lit(1)], vec![u128_lit(0)]),
            InputOutputTestCase::new(vec![u64_lit(1), u64_lit(1)], vec![u128_lit(1)]),
            InputOutputTestCase::new(
                vec![u64_lit(1), u64_lit(u64::MAX)],
                vec![u128_lit(u64::MAX as u128)],
            ),
            InputOutputTestCase::new(
                vec![u64_lit(u64::MAX), u64_lit(1)],
                vec![u128_lit(u64::MAX as u128)],
            ),
            InputOutputTestCase::new(
                vec![u64_lit(u64::MAX), u64_lit(u64::MAX)],
                vec![u128_lit(u64::MAX as u128 * u64::MAX as u128)],
            ),
            InputOutputTestCase::new(
                vec![u64_lit(u32::MAX as u64), u64_lit(u64::MAX)],
                vec![u128_lit(u32::MAX as u128 * u64::MAX as u128)],
            ),
            InputOutputTestCase::new(
                vec![u64_lit(u64::MAX), u64_lit(u32::MAX as u64)],
                vec![u128_lit(u32::MAX as u128 * u64::MAX as u128)],
            ),
        ];
        for _ in 0..10 {
            let lhs = thread_rng().gen_range(0..u64::MAX);
            let rhs = thread_rng().gen_range(0..u64::MAX);
            let expected = u128_lit(lhs as u128 * rhs as u128);
            test_cases.push(InputOutputTestCase::new(
                vec![u64_lit(lhs), u64_lit(rhs)],
                vec![expected.clone()],
            ));
            test_cases.push(InputOutputTestCase::new(
                vec![u64_lit(rhs), u64_lit(lhs)],
                vec![expected],
            ));
        }
        multiple_compare_prop_with_stack_safe_lists(&mul_two_u64s_rast(), test_cases);

        fn mul_two_u64s_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn mul_u128_test(lhs: u64, rhs: u64) -> u128 {
                    let a: u128 = tasm::tasm_arithmetic_u64_mul_two_u64s_to_u128_u64(lhs, rhs);
                    let b: u128 = lhs as u128 * rhs as u128;
                    assert!(a == b);
                    assert!(a != b + 1);
                    assert!(a + 1 != b);
                    return a;
                }
            })
        }
    }
}
