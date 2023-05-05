use syn::parse_quote;

use crate::graft::item_fn;

#[allow(dead_code)]
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

#[allow(dead_code)]
fn sub_u64_rast_1() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u64_test(lhs: u64, rhs: u64) -> u64 {
            let c: u64 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
fn sub_u64_rast_2() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u64_test(rhs: u64, lhs: u64) -> u64 {
            let c: u64 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
fn mul_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn mul_u64(rhs: u64, lhs: u64) -> u64 {
            return rhs * lhs;
        }
    })
}

#[allow(dead_code)]
fn div_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn div_u64(numerator: u64, divisor: u64) -> u64 {
            return numerator / divisor;
        }
    })
}

#[allow(dead_code)]
fn rem_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn rem_u64(numerator: u64, divisor: u64) -> u64 {
            return numerator % divisor;
        }
    })
}

#[allow(dead_code)]
fn lt_u64_dynamic_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn lt_u64_dynamic(lhs: u64, rhs: u64) -> bool {
            return lhs < rhs;
        }
    })
}

#[allow(dead_code)]
fn gt_u64_dynamic_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn gt_u64_dynamic(lhs: u64, rhs: u64) -> bool {
            return lhs > rhs;
        }
    })
}

#[allow(dead_code)]
fn leading_zeros_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn leading_zeros_u64(value: u64) -> u32 {
            return value.leading_zeros();
        }
    })
}

#[allow(dead_code)]
fn count_ones_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn count_ones_u64(value: u64) -> u32 {
            return value.count_ones();
        }
    })
}

#[allow(dead_code)]
fn divmoddi4_u64_rast() -> syn::ItemFn {
    // This code shows how to do u64 div-mod using only u32 div-mod primitives.
    // So the TASM code that this function compiles to can be used for the u64
    // div-mod snippet for this compiler.
    item_fn(parse_quote! {
        fn divmoddi4_tasm_lang_friendly(numerator_input: u64, divisor: u64) -> (u64, u64) {
            let num_hi: u32 = (numerator_input >> 32) as u32;
            let num_lo: u32 = (numerator_input & u32::MAX as u64) as u32;
            let div_hi: u32 = (divisor >> 32) as u32;
            let div_lo: u32 = (divisor & u32::MAX as u64) as u32;
            let mut ret: (u64, u64) = (0u64, 0u64);
            let mut numerator: u64 = numerator_input;


            if divisor > numerator {
                ret = (0u64, numerator);
            } else {
                if div_hi == 0u32 && divisor == 1u64 || div_hi == 0u32 && num_hi == 0u32 {
                    if divisor == 1u64 {
                        // return (numerator, 0);
                        ret = (numerator, 0u64);
                    } else {
                        if num_hi == 0u32 {
                            // return ((num_lo / div_lo) as u64, (num_lo % div_lo) as u64);
                            ret = ((num_lo / div_lo) as u64, (num_lo % div_lo) as u64);
                        }
                    }
                } else {
                    // assert!(0u64 != divisor, "Cannot divide by zero");
                    assert!(0u64 != divisor);
                    let mut bits: u32 = divisor.leading_zeros() - numerator.leading_zeros() + 1u32;
                    let mut rem: u64 = numerator >> bits;
                    numerator = numerator << 64 - bits;
                    let mut wrap: u64 = 0u64;
                    while bits > 0u32 {
                        rem = (rem << 1) | (numerator >> 63);
                        numerator = (numerator << 1) | (wrap & 1u64);
                        wrap = if divisor > rem { 0u64 } else { u64::MAX };
                        rem = rem - (divisor & wrap);

                        bits = bits - 1u32;
                    }

                    ret = (numerator << 1 | wrap & 1u64, rem);
                }
            }

            return ret;
        }
    })
}

#[allow(dead_code)]
fn bitwise_and_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn bitwise_and_u64(lhs: u64, rhs: u64) -> u64 {
            let c: u64 = lhs & rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
fn operator_evaluation_ordering_with_div_u64() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_div() -> u64 {
            return 100u64 - 14u64 / 2u64 + 1u64;
        }
    })
}

#[allow(dead_code)]
fn leftshift_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn leftshift_u64(lhs: u64, rhs: u32) -> u64 {
            let c: u64 = lhs << rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
fn rightshift_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn rightshift_u64(lhs: u64, rhs: u32) -> u64 {
            let c: u64 = lhs >> rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
fn bitwise_not_return_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn bitwise_not(value: u64) -> u64 {
            return !value;
        }
    })
}

#[allow(dead_code)]
fn bitwise_not_assign_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn bitwise_not(value: u64) -> u64 {
            let ret: u64 = !value;
            return ret;
        }
    })
}

#[cfg(test)]
mod run_tests {
    use itertools::Itertools;
    use rand::{thread_rng, Rng, RngCore};
    use twenty_first::shared_math::other::random_elements;

    use super::*;
    use crate::tests_and_benchmarks::shared_test::*;

    #[test]
    fn add_u64_run_test() {
        compare_prop_with_stack(
            &add_u64_rast(),
            vec![
                u64_lit((1 << 33) + (1 << 16)),
                u64_lit((1 << 33) + (1 << 16)),
            ],
            vec![u64_lit((1 << 34) + (1 << 17))],
        );
        for _ in 0..10 {
            let lhs = thread_rng().gen_range(0..u64::MAX / 2);
            let rhs = thread_rng().gen_range(0..u64::MAX / 2);
            compare_prop_with_stack(
                &add_u64_rast(),
                vec![u64_lit(lhs), u64_lit(rhs)],
                vec![u64_lit(lhs + rhs)],
            )
        }
    }

    #[test]
    fn sub_u64_run_test() {
        let input_args_1 = vec![u64_lit(200), u64_lit(95)];
        let expected_outputs_1 = vec![u64_lit(105)];
        compare_prop_with_stack(&sub_u64_rast_1(), input_args_1, expected_outputs_1);

        let input_args_2 = vec![u64_lit(95), u64_lit(200)];
        let expected_outputs_2 = vec![u64_lit(105)];
        compare_prop_with_stack(&sub_u64_rast_2(), input_args_2, expected_outputs_2);

        let input_args_3 = vec![u64_lit(1), u64_lit(1 << 32)];
        let expected_outputs_3 = vec![u64_lit(u32::MAX as u64)];
        compare_prop_with_stack(&sub_u64_rast_2(), input_args_3, expected_outputs_3);

        let lhs = thread_rng().gen_range(0..u64::MAX);
        let rhs = thread_rng().gen_range(0..=lhs);
        let input_args_4 = vec![u64_lit(rhs), u64_lit(lhs)];
        let expected_outputs_4 = vec![u64_lit(lhs - rhs)];
        compare_prop_with_stack(&sub_u64_rast_2(), input_args_4, expected_outputs_4);
    }

    #[test]
    fn mul_u64_run_test() {
        let input_args_1 = vec![u64_lit(1u64 << 46), u64_lit(1u64 << 4)];
        let expected_outputs_1 = vec![u64_lit(1u64 << 50)];
        compare_prop_with_stack(&mul_u64_rast(), input_args_1, expected_outputs_1);
    }

    #[test]
    fn div_u64_test() {
        compare_prop_with_stack(
            &div_u64_rast(),
            vec![u64_lit(9075814844808036352), u64_lit(1675951742761566208)],
            vec![u64_lit(5)],
        );
        compare_prop_with_stack(
            &div_u64_rast(),
            vec![u64_lit(u64::MAX), u64_lit(2)],
            vec![u64_lit((1u64 << 63) - 1)],
        );

        // Test with small divisors
        let mut rng = thread_rng();
        for _ in 0..4 {
            let numerator: u64 = rng.next_u64();
            let divisor: u64 = rng.gen_range(1..(1 << 12));
            compare_prop_with_stack(
                &div_u64_rast(),
                vec![u64_lit(numerator), u64_lit(divisor)],
                vec![u64_lit(numerator / divisor)],
            );
        }

        // Test with big divisor
        for j in 0..33 {
            for _ in 0..2 {
                let numerator: u64 = rng.next_u64();
                let divisor: u64 = rng.next_u32() as u64 + (1u64 << (31 + j));
                compare_prop_with_stack(
                    &div_u64_rast(),
                    vec![u64_lit(numerator), u64_lit(divisor)],
                    vec![u64_lit(numerator / divisor)],
                );
            }
        }
    }

    #[test]
    fn rem_u64_test() {
        compare_prop_with_stack(
            &rem_u64_rast(),
            vec![u64_lit(9075814844808036352), u64_lit(1675951742761566208)],
            vec![u64_lit(696056131000205312)],
        );
        compare_prop_with_stack(
            &rem_u64_rast(),
            vec![u64_lit(u64::MAX), u64_lit(2)],
            vec![u64_lit(1)],
        );

        // Test with small divisors
        let mut rng = thread_rng();
        for _ in 0..4 {
            let numerator: u64 = rng.next_u64();
            let divisor: u64 = rng.gen_range(1..(1 << 12));
            compare_prop_with_stack(
                &rem_u64_rast(),
                vec![u64_lit(numerator), u64_lit(divisor)],
                vec![u64_lit(numerator % divisor)],
            );
        }

        // Test with big divisor
        for j in 0..33 {
            for _ in 0..2 {
                let numerator: u64 = rng.next_u64();
                let divisor: u64 = rng.next_u32() as u64 + (1u64 << (31 + j));
                compare_prop_with_stack(
                    &rem_u64_rast(),
                    vec![u64_lit(numerator), u64_lit(divisor)],
                    vec![u64_lit(numerator % divisor)],
                );
            }
        }
    }

    // TODO: Fix the two below tests when a better execution wrapper
    // exists!
    #[should_panic]
    #[test]
    fn div_u64_zero_divisor_small_numerator_test() {
        compare_prop_with_stack(
            &divmoddi4_u64_rast(),
            vec![u64_lit(51), u64_lit(0)],
            vec![u64_lit(0), u64_lit(0)],
        );
    }

    #[should_panic]
    #[test]
    fn div_u64_zero_divisor_big_numerator_test() {
        compare_prop_with_stack(
            &divmoddi4_u64_rast(),
            vec![u64_lit((1u64 << 32) + 100), u64_lit(0)],
            vec![u64_lit(0), u64_lit(0)],
        );
    }

    #[test]
    fn divmoddi4_u64_run_test() {
        fn add_test(
            (quotient, remainder): (u64, u64),
            (numerator, divisor): (u64, u64),
            test_cases: &mut Vec<InputOutputTestCase>,
        ) {
            let inputs = vec![u64_lit(numerator), u64_lit(divisor)];
            let outputs = vec![u64_lit(quotient), u64_lit(remainder)];
            test_cases.push(InputOutputTestCase::new(inputs, outputs));
        }

        let mut test_cases = vec![];
        add_test((7, 2), (51, 7), &mut test_cases);
        add_test((7, 0), (14, 2), &mut test_cases);
        add_test((10, 0), (100, 10), &mut test_cases);
        add_test((33, 1), (100, 3), &mut test_cases);
        add_test((1u64 << 42, 0), (1u64 << 46, 1u64 << 4), &mut test_cases);
        add_test(
            (5, 696056131000205312),
            (9075814844808036352, 1675951742761566208),
            &mut test_cases,
        );

        // Test with small divisors
        let mut rng = thread_rng();
        for _ in 0..4 {
            let numerator: u64 = rng.next_u64();
            let divisor: u64 = rng.gen_range(0..(1 << 12));
            add_test(
                (numerator / divisor, numerator % divisor),
                (numerator, divisor),
                &mut test_cases,
            )
        }

        // Test with biggest divisors
        for j in 0..33 {
            for _ in 0..2 {
                let numerator: u64 = rng.next_u64();
                let divisor: u64 = rng.next_u32() as u64 + (1u64 << (31 + j));
                add_test(
                    (numerator / divisor, numerator % divisor),
                    (numerator, divisor),
                    &mut test_cases,
                )
            }
        }

        multiple_compare_prop_with_stack(&divmoddi4_u64_rast(), test_cases);
    }

    #[test]
    fn lt_u64_dynamic_test() {
        let mut rng = thread_rng();
        for _ in 0..10 {
            let lhs = rng.next_u64();
            let rhs = rng.next_u64();
            compare_prop_with_stack(
                &lt_u64_dynamic_rast(),
                vec![u64_lit(lhs), u64_lit(rhs)],
                vec![bool_lit(lhs < rhs)],
            );

            compare_prop_with_stack(
                &lt_u64_dynamic_rast(),
                vec![u64_lit(lhs), u64_lit(lhs)],
                vec![bool_lit(false)],
            );

            compare_prop_with_stack(
                &gt_u64_dynamic_rast(),
                vec![u64_lit(lhs), u64_lit(rhs)],
                vec![bool_lit(lhs > rhs)],
            );

            compare_prop_with_stack(
                &gt_u64_dynamic_rast(),
                vec![u64_lit(lhs), u64_lit(lhs)],
                vec![bool_lit(false)],
            );
        }

        // 0 vs 0
        compare_prop_with_stack(
            &lt_u64_dynamic_rast(),
            vec![u64_lit(0), u64_lit(0)],
            vec![bool_lit(false)],
        );
        compare_prop_with_stack(
            &gt_u64_dynamic_rast(),
            vec![u64_lit(0), u64_lit(0)],
            vec![bool_lit(false)],
        );

        // 0 vs 1
        compare_prop_with_stack(
            &lt_u64_dynamic_rast(),
            vec![u64_lit(0), u64_lit(1)],
            vec![bool_lit(true)],
        );
        compare_prop_with_stack(
            &gt_u64_dynamic_rast(),
            vec![u64_lit(0), u64_lit(1)],
            vec![bool_lit(false)],
        );

        // 1 vs 0
        compare_prop_with_stack(
            &lt_u64_dynamic_rast(),
            vec![u64_lit(1), u64_lit(0)],
            vec![bool_lit(false)],
        );
        compare_prop_with_stack(
            &gt_u64_dynamic_rast(),
            vec![u64_lit(1), u64_lit(0)],
            vec![bool_lit(true)],
        );

        // 1 vs 1
        compare_prop_with_stack(
            &lt_u64_dynamic_rast(),
            vec![u64_lit(1), u64_lit(1)],
            vec![bool_lit(false)],
        );
        compare_prop_with_stack(
            &gt_u64_dynamic_rast(),
            vec![u64_lit(1), u64_lit(1)],
            vec![bool_lit(false)],
        );

        // 1^32 vs 1
        compare_prop_with_stack(
            &lt_u64_dynamic_rast(),
            vec![u64_lit(1u64 << 32), u64_lit(1)],
            vec![bool_lit(false)],
        );
        compare_prop_with_stack(
            &gt_u64_dynamic_rast(),
            vec![u64_lit(1u64 << 32), u64_lit(1)],
            vec![bool_lit(true)],
        );
    }

    #[test]
    fn leading_zeros_u64_test() {
        let values: Vec<u64> = random_elements(40);
        let mut test_cases = values
            .iter()
            .map(|value| {
                InputOutputTestCase::new(
                    vec![u64_lit(*value)],
                    vec![u32_lit(value.leading_zeros())],
                )
            })
            .collect_vec();
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit(0)],
            vec![u32_lit(64)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit(1)],
            vec![u32_lit(63)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit(2)],
            vec![u32_lit(62)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit(3)],
            vec![u32_lit(62)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit(u64::MAX)],
            vec![u32_lit(0)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit(1u64 << 63)],
            vec![u32_lit(0)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit((1u64 << 63) + 1)],
            vec![u32_lit(0)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit((1u64 << 63) - 1)],
            vec![u32_lit(1)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit(1u64 << 31)],
            vec![u32_lit(32)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit((1u64 << 31) + 1)],
            vec![u32_lit(32)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit((1u64 << 31) - 1)],
            vec![u32_lit(33)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit((1u64 << 32) - 1)],
            vec![u32_lit(32)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit(1u64 << 32)],
            vec![u32_lit(31)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit((1u64 << 32) + 1)],
            vec![u32_lit(31)],
        ));
        multiple_compare_prop_with_stack(&leading_zeros_u64_rast(), test_cases);
    }

    #[test]
    fn count_ones_u64_test() {
        let values: Vec<u64> = random_elements(40);
        let mut test_cases = values
            .iter()
            .map(|value| {
                InputOutputTestCase::new(vec![u64_lit(*value)], vec![u32_lit(value.count_ones())])
            })
            .collect_vec();
        test_cases.push(InputOutputTestCase::new(vec![u64_lit(0)], vec![u32_lit(0)]));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit(u64::MAX)],
            vec![u32_lit(64)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit(u64::MAX - 1)],
            vec![u32_lit(63)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit((1u64 << 63) - 1)],
            vec![u32_lit(63)],
        ));
        for j in 0..33 {
            test_cases.push(InputOutputTestCase::new(
                vec![u64_lit((u32::MAX as u64) << j)],
                vec![u32_lit(32)],
            ));
        }
        multiple_compare_prop_with_stack(&count_ones_u64_rast(), test_cases);
    }

    #[test]
    fn leftshift_u64_run_test() {
        let input = vec![u64_lit(0b10101010101010101010101010101u64), u32_lit(32u32)];
        let expected_output = vec![u64_lit(1537228671377473536)];
        compare_prop_with_stack(&leftshift_u64_rast(), input, expected_output);
    }

    #[test]
    fn rightshift_u64_run_test() {
        let input = vec![u64_lit(0b10101010101010101010101010101u64), u32_lit(3u32)];
        let expected_output = vec![u64_lit(44739242)];
        compare_prop_with_stack(&rightshift_u64_rast(), input, expected_output);
    }

    #[test]
    fn operator_evaluation_ordering_test() {
        compare_prop_with_stack(
            &operator_evaluation_ordering_with_div_u64(),
            vec![],
            vec![u64_lit(94)],
        );
    }

    #[test]
    fn bitwise_not_u64_test() {
        let values: Vec<u64> = random_elements(10);
        let mut test_cases = values
            .iter()
            .map(|value| InputOutputTestCase::new(vec![u64_lit(*value)], vec![u64_lit(!value)]))
            .collect_vec();
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit(0)],
            vec![u64_lit(u64::MAX)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit(u64::MAX)],
            vec![u64_lit(0)],
        ));
        test_cases.push(InputOutputTestCase::new(
            vec![u64_lit(u32::MAX as u64)],
            vec![u64_lit(u64::MAX ^ u32::MAX as u64)],
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
    fn sub_u64_rast_1_test() {
        graft_check_compile_prop(&sub_u64_rast_1());
    }

    #[test]
    fn sub_u64_rast_2_test() {
        graft_check_compile_prop(&sub_u64_rast_2());
    }

    #[test]
    fn add_u64_test() {
        graft_check_compile_prop(&add_u64_rast());
    }

    #[test]
    fn bitwise_and_u64_test() {
        graft_check_compile_prop(&bitwise_and_u64_rast());
    }

    #[test]
    fn leftshift_u64_test() {
        graft_check_compile_prop(&leftshift_u64_rast());
    }

    #[test]
    fn rightshift_u64_test() {
        graft_check_compile_prop(&rightshift_u64_rast());
    }

    #[test]
    fn u64_div_mod_test() {
        graft_check_compile_prop(&divmoddi4_u64_rast());
    }
}