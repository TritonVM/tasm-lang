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
        compare_prop_with_stack(
            &and_bool_rast(),
            vec![bool_lit(false), bool_lit(false)],
            vec![bool_lit(false)],
        );

        compare_prop_with_stack(
            &and_bool_rast(),
            vec![bool_lit(true), bool_lit(false)],
            vec![bool_lit(false)],
        );

        compare_prop_with_stack(
            &and_bool_rast(),
            vec![bool_lit(false), bool_lit(true)],
            vec![bool_lit(false)],
        );

        compare_prop_with_stack(
            &and_bool_rast(),
            vec![bool_lit(true), bool_lit(true)],
            vec![bool_lit(true)],
        );
    }
}
