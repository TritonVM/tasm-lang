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
pub fn simple_sub_u32() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn simple_sub(a: u32, b: u32) -> u32 {
            return a - b;
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
pub fn mul_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn mul_u32(rhs: u32, lhs: u32) -> u32 {
            return rhs * lhs;
        }
    })
}

#[allow(dead_code)]
pub fn div_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn div_u32(rhs: u32, lhs: u32) -> u32 {
            return rhs / lhs;
        }
    })
}

#[allow(dead_code)]
pub fn rem_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn rem_u32(rhs: u32, lhs: u32) -> u32 {
            return rhs % lhs;
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
pub fn operator_evaluation_ordering_with_div_u32() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_div() -> u32 {
            return 100u32 - 14u32 / 2u32 + 1u32;
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
pub fn leftshift_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn leftshift_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs << rhs;
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

#[cfg(test)]
mod run_tests {
    use super::*;
    use crate::tests::shared_test::*;

    #[test]
    fn simple_sub_test() {
        compare_prop_with_stack(
            &simple_sub_u32(),
            vec![u32_lit(100), u32_lit(51)],
            vec![u32_lit(49)],
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
    fn div_u32_run_test() {
        let input_args_1 = vec![u32_lit(56), u32_lit(7)];
        let expected_outputs_1 = vec![u32_lit(8)];
        compare_prop_with_stack(&div_u32_rast(), input_args_1, expected_outputs_1);
    }

    #[test]
    fn rem_u32_run_test() {
        let input_args_1 = vec![u32_lit(17), u32_lit(6)];
        let expected_outputs_1 = vec![u32_lit(5)];
        compare_prop_with_stack(&rem_u32_rast(), input_args_1, expected_outputs_1);
    }

    #[test]
    fn lt_u32_test() {
        compare_prop_with_stack(&lt_u32(), vec![], vec![bool_lit(true)]);
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
    fn mul_u32_rast_test() {
        graft_check_compile_prop(&mul_u32_rast());
    }

    #[test]
    fn div_u32_rast_test() {
        graft_check_compile_prop(&div_u32_rast());
    }

    #[test]
    fn rem_u32_rast_test() {
        graft_check_compile_prop(&rem_u32_rast());
    }

    #[test]
    fn bitwise_and_u32_test() {
        graft_check_compile_prop(&bitwise_and_u32_rast());
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
    fn rightshift_u32_test() {
        graft_check_compile_prop(&rightshift_u32_rast());
    }
}
