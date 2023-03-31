use syn::parse_quote;

use crate::graft::item_fn;

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
pub fn or_bool_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn boolean_and_bool(lhs: bool, rhs: bool) -> bool {
            let c: bool = lhs || rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn assert_bool_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn boolean_and_bool(lhs: bool) {
            assert!(lhs);
            return;
        }
    })
}

#[cfg(test)]
mod compile_and_typecheck_tests {
    use super::*;
    use crate::tests::shared_test::graft_check_compile_prop;

    #[test]
    fn and_bool_test() {
        graft_check_compile_prop(&and_bool_rast());
    }
}

#[cfg(test)]
mod run_tests {
    use super::*;
    use crate::tests::shared_test::*;

    #[test]
    fn and_bool_test() {
        for (lhs, rhs) in [(false, false), (false, true), (true, false), (true, true)] {
            compare_prop_with_stack(
                &and_bool_rast(),
                vec![bool_lit(lhs), bool_lit(rhs)],
                vec![bool_lit(lhs && rhs)],
            );
        }
    }

    #[test]
    fn or_bool_test() {
        for (lhs, rhs) in [(false, false), (false, true), (true, false), (true, true)] {
            compare_prop_with_stack(
                &or_bool_rast(),
                vec![bool_lit(lhs), bool_lit(rhs)],
                vec![bool_lit(lhs || rhs)],
            );
        }
    }

    #[test]
    fn assert_bool_test() {
        // TODO: Include `false` test here when we have a wrapper that doesn't panic on VM execution errors
        // for bool in [false, true] {
        compare_prop_with_stack(&assert_bool_rast(), vec![bool_lit(true)], vec![]);
    }

    #[should_panic]
    #[test]
    fn assert_bool_failure_test() {
        // TODO: Remove this test when we have a VM wrapper that doesn't panic on execution errors
        compare_prop_with_stack(&assert_bool_rast(), vec![bool_lit(false)], vec![]);
    }
}
