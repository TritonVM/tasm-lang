use syn::parse_quote;

use crate::graft::item_fn;

#[allow(dead_code)]
pub fn add_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn add_u32_overwrite_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_u32_overwrite(lhs: u32, rhs: u32) -> u32 {
            let mut c: u32 = lhs + rhs;
            c = c + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn sub_u32_rast_1() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn sub_u32_rast_2() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u32(rhs: u32, lhs: u32) -> u32 {
            let c: u32 = lhs - rhs;
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
pub fn add_bfe_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_bfe(lhs: BFieldElement, rhs: BFieldElement) -> BFieldElement {
            let c: BFieldElement = lhs + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn add_xfe_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_xfe(lhs: XFieldElement, rhs: XFieldElement) -> XFieldElement {
            let c: XFieldElement = lhs + rhs;
            return c;
        }
    })
}

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
pub fn and_bool_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn boolean_and_bool(lhs: bool, rhs: bool) -> bool {
            let c: bool = lhs && rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn bitwise_and_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn bitwise_and_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs & rhs;
            return c;
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
pub fn simple_sub() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn simple_sub(a: u32, b: u32) -> u32 {
            return a - b;
        }
    })
}

#[allow(dead_code)]
pub fn operator_evaluation_ordering_with_div_u32() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_div() -> u32 {
            return 100u32 - 14u32 / 2u32 + 1u32;
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
pub fn operator_evaluation_ordering_with_mul() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_mul() -> u32 {
            return 380u32 - 14u32 * 2u32 * 10u32 + 1u32 - 41u32 * 1u32;
        }
    })
}

#[allow(dead_code)]
pub fn lt_u32() -> syn::ItemFn {
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

#[allow(dead_code)]
pub fn powers_of_two_with_bit_shifting() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn powers_of_two_with_bit_shifting() -> u64 {
            let a: u64 = 1 << 40;
            let b: u64 = 1 << 60;
            let c: u32 = 1000;
            let d: u32 = 2000;
            let e: (u32, u64) = (2300u32, 4000u64);
            return a + b;
        }
    })
}

#[allow(dead_code)]
pub fn leftshift_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn leftshift_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs << rhs;
            return c;
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
pub fn rightshift_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn rightshift_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs >> rhs;
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
    fn simple_sub_test() {
        compare_prop_with_stack(
            &simple_sub(),
            vec![u32_lit(100), u32_lit(51)],
            vec![u32_lit(49)],
        );
    }

    #[test]
    fn operator_evaluation_ordering_test() {
        compare_prop_with_stack(
            &operator_evaluation_ordering_with_div_u32(),
            vec![],
            vec![u32_lit(94)],
        );

        compare_prop_with_stack(
            &operator_evaluation_ordering_with_div_u64(),
            vec![],
            vec![u64_lit(94)],
        );

        compare_prop_with_stack(
            &operator_evaluation_ordering_with_mul(),
            vec![],
            vec![u32_lit(60)],
        );
    }

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
    fn sub_u32_run_test() {
        let input_args_1 = vec![u32_lit(200), u32_lit(95)];
        let expected_outputs_1 = vec![u32_lit(105)];
        compare_prop_with_stack(&sub_u32_rast_1(), input_args_1, expected_outputs_1);

        let input_args_2 = vec![u32_lit(95), u32_lit(200)];
        let expected_outputs_2 = vec![u32_lit(105)];
        compare_prop_with_stack(&sub_u32_rast_2(), input_args_2, expected_outputs_2);
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
    fn lt_u32_test() {
        compare_prop_with_stack(&lt_u32(), vec![], vec![bool_lit(true)]);
    }

    #[test]
    fn powers_of_two_with_bit_shifting_test() {
        compare_prop_with_stack(
            &powers_of_two_with_bit_shifting(),
            vec![],
            vec![u64_lit((1u64 << 40) + (1u64 << 60))],
        );
    }

    #[test]
    fn leftshift_u32_run_test() {
        let input = vec![u32_lit(0b101010101010101u32), u32_lit(16u32)];
        let expected_output = vec![u32_lit(1431633920)];
        compare_prop_with_stack(&leftshift_u32_rast(), input, expected_output);
    }

    #[test]
    fn leftshift_u64_run_test() {
        let input = vec![u64_lit(0b10101010101010101010101010101u64), u32_lit(32u32)];
        let expected_output = vec![u64_lit(1537228671377473536)];
        compare_prop_with_stack(&leftshift_u64_rast(), input, expected_output);
    }

    #[test]
    fn rightshift_u32_run_test() {
        let input = vec![u32_lit(0b101010101010101u32), u32_lit(3u32)];
        let expected_output = vec![u32_lit(2730)];
        compare_prop_with_stack(&rightshift_u32_rast(), input, expected_output);
    }

    #[test]
    fn rightshift_u64_run_test() {
        let input = vec![u64_lit(0b10101010101010101010101010101u64), u32_lit(3u32)];
        let expected_output = vec![u64_lit(44739242)];
        compare_prop_with_stack(&rightshift_u64_rast(), input, expected_output);
    }
}

#[cfg(test)]
mod compile_and_typecheck_tests {
    use crate::tests::shared_test::graft_check_compile_prop;

    use super::*;

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
    fn lt_u32_test() {
        graft_check_compile_prop(&lt_u32());
    }

    #[test]
    fn leftshift_u32_test() {
        graft_check_compile_prop(&leftshift_u32_rast());
    }

    #[test]
    fn leftshift_u64_test() {
        graft_check_compile_prop(&leftshift_u64_rast());
    }

    #[test]
    fn rightshift_u32_test() {
        graft_check_compile_prop(&rightshift_u32_rast());
    }

    #[test]
    fn rightshift_u64_test() {
        graft_check_compile_prop(&rightshift_u64_rast());
    }
}
