use syn::parse_quote;

use crate::graft::item_fn;

#[allow(dead_code)]
pub fn add_u64_rast() -> syn::ItemFn {
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
pub fn sub_u64_rast_1() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u64_test(lhs: u64, rhs: u64) -> u64 {
            let c: u64 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn sub_u64_rast_2() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u64_test(rhs: u64, lhs: u64) -> u64 {
            let c: u64 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn mul_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn mul_u64(rhs: u64, lhs: u64) -> u64 {
            return rhs * lhs;
        }
    })
}

#[allow(dead_code)]
pub fn div_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn divmoddi4_tasm_lang_friendly(numerator_input: u64, divisor: u64) -> (u64, u64) {
            let num_hi: u32 = (numerator_input >> 32) as u32;
            let num_lo: u32 = (numerator_input & u32::MAX as u64) as u32;
            let div_hi: u32 = (divisor >> 32) as u32;
            let div_lo: u32 = (divisor & u32::MAX as u64) as u32;
            let mut ret: (u64, u64) = (0u64, 0u64);
            // assert_ne!(0, divisor, "Cannot divide by zero");

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
                    let mut bits: u32 = divisor.leading_zeros() - numerator.leading_zeros() + 1u32;
                    let mut rem: u64 = numerator >> bits;
                    numerator = numerator << 64 - bits;
                    let mut wrap: u64 = 0u64;
                    while bits > 0u32 {
                        rem = (rem << 1) | (numerator >> 63);
                        numerator = (numerator << 1) | (wrap & 1u64);
                        wrap = if divisor > rem { 0u64 } else { u64::MAX };
                        rem = rem - divisor & wrap;

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
pub fn bitwise_and_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn bitwise_and_u64(lhs: u64, rhs: u64) -> u64 {
            let c: u64 = lhs & rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn operator_evaluation_ordering_with_div_u64() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_div() -> u64 {
            return 100u64 - 14u64 / 2u64 + 1u64;
        }
    })
}

#[allow(dead_code)]
pub fn leftshift_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn leftshift_u64(lhs: u64, rhs: u32) -> u64 {
            let c: u64 = lhs << rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn rightshift_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn rightshift_u64(lhs: u64, rhs: u32) -> u64 {
            let c: u64 = lhs >> rhs;
            return c;
        }
    })
}

#[cfg(test)]
mod run_tests {
    use rand::{thread_rng, Rng};

    use super::*;
    use crate::tests::shared_test::*;

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
    fn div_u64_run_test() {
        compare_prop_with_stack(
            &div_u64_rast(),
            vec![u64_lit(51), u64_lit(7)],
            vec![u64_lit(7), u64_lit(2)],
        );
        compare_prop_with_stack(
            &div_u64_rast(),
            vec![u64_lit(14), u64_lit(2)],
            vec![u64_lit(7), u64_lit(0)],
        );
        compare_prop_with_stack(
            &div_u64_rast(),
            vec![u64_lit(100), u64_lit(10)],
            vec![u64_lit(10), u64_lit(0)],
        );
        compare_prop_with_stack(
            &div_u64_rast(),
            vec![u64_lit(100), u64_lit(3)],
            vec![u64_lit(33), u64_lit(1)],
        );
        compare_prop_with_stack(
            &div_u64_rast(),
            vec![u64_lit(1u64 << 46), u64_lit(1u64 << 4)],
            vec![u64_lit(1u64 << 42), u64_lit(0)],
        );
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
}

#[cfg(test)]
mod compile_and_typecheck_tests {
    use crate::tests::shared_test::graft_check_compile_prop;

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
}
