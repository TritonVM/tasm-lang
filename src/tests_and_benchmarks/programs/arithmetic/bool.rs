use syn::parse_quote;

use crate::graft::item_fn;

#[cfg(test)]
mod run_tests {
    use super::*;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn and_bool_test() {
        for (lhs, rhs) in [(false, false), (false, true), (true, false), (true, true)] {
            compare_prop_with_stack_safe_lists(
                &and_bool_rast(),
                vec![bool_lit(lhs), bool_lit(rhs)],
                vec![bool_lit(lhs && rhs)],
            );
        }

        fn and_bool_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn boolean_and_bool(lhs: bool, rhs: bool) -> bool {
                    let c: bool = lhs && rhs;
                    return c;
                }
            })
        }
    }

    #[test]
    fn or_bool_test() {
        for (lhs, rhs) in [(false, false), (false, true), (true, false), (true, true)] {
            compare_prop_with_stack_safe_lists(
                &or_bool_rast(),
                vec![bool_lit(lhs), bool_lit(rhs)],
                vec![bool_lit(lhs || rhs)],
            );
        }

        fn or_bool_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn boolean_and_bool(lhs: bool, rhs: bool) -> bool {
                    let c: bool = lhs || rhs;
                    return c;
                }
            })
        }
    }

    #[test]
    fn not_bool_test() {
        let test_cases = vec![
            InputOutputTestCase::new(vec![bool_lit(true)], vec![bool_lit(false)]),
            InputOutputTestCase::new(vec![bool_lit(false)], vec![bool_lit(true)]),
        ];
        multiple_compare_prop_with_stack_safe_lists(&not_bool_assign_rast(), test_cases.clone());
        multiple_compare_prop_with_stack_safe_lists(&not_bool_return_rast(), test_cases);

        fn not_bool_assign_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn boolean_not(value: bool) -> bool {
                    let ret: bool = !value;
                    return ret;
                }
            })
        }

        fn not_bool_return_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn boolean_not(value: bool) -> bool {
                    return !value;
                }
            })
        }
    }

    #[test]
    fn not_bool_expr_test() {
        let test_cases = vec![
            InputOutputTestCase::new(vec![bool_lit(false), bool_lit(false)], vec![bool_lit(true)]),
            InputOutputTestCase::new(vec![bool_lit(false), bool_lit(true)], vec![bool_lit(false)]),
            InputOutputTestCase::new(vec![bool_lit(true), bool_lit(false)], vec![bool_lit(true)]),
            InputOutputTestCase::new(vec![bool_lit(true), bool_lit(true)], vec![bool_lit(true)]),
        ];
        multiple_compare_prop_with_stack_safe_lists(&not_bool_in_expression_rast(), test_cases);

        fn not_bool_in_expression_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn boolean_not(a: bool, b: bool) -> bool {
                    return (a || !b);
                }
            })
        }
    }

    #[test]
    fn assert_bool_test() {
        // TODO: Include `false` test here when we have a wrapper that doesn't panic on VM execution errors
        // for bool in [false, true] {
        compare_prop_with_stack_safe_lists(&assert_bool_rast(), vec![bool_lit(true)], vec![]);

        fn assert_bool_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn boolean_and_bool(lhs: bool) {
                    assert!(lhs);
                    return;
                }
            })
        }
    }

    #[should_panic]
    #[test]
    fn assert_bool_failure_test() {
        // TODO: Remove this test when we have a VM wrapper that doesn't panic on execution errors
        compare_prop_with_stack_safe_lists(&assert_bool_rast(), vec![bool_lit(false)], vec![]);

        fn assert_bool_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn boolean_and_bool(lhs: bool) {
                    assert!(lhs);
                    return;
                }
            })
        }
    }
}
