#[cfg(test)]
mod run_tests {
    use rand::{thread_rng, Rng};
    use syn::parse_quote;

    use crate::{graft::item_fn, tests_and_benchmarks::test_helpers::shared_test::*};

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
