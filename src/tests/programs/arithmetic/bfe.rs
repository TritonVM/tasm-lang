use syn::parse_quote;

use crate::graft::item_fn;

#[allow(dead_code)]
fn add_bfe_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_bfe(lhs: BFieldElement, rhs: BFieldElement) -> BFieldElement {
            let c: BFieldElement = lhs + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
fn mul_bfe_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_bfe(lhs: BFieldElement, rhs: BFieldElement) -> BFieldElement {
            return lhs * rhs;
        }
    })
}

#[allow(dead_code)]
fn cast_from_bool_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn cast_from_bool(input: bool) -> BFieldElement {
            return input as BFieldElement;
        }
    })
}

#[cfg(test)]
mod compile_and_typecheck_tests {
    use super::*;
    use crate::tests::shared_test::graft_check_compile_prop;

    #[test]
    fn add_bfe_test() {
        graft_check_compile_prop(&add_bfe_rast());
    }
}

#[cfg(test)]
mod run_tests {
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use super::*;
    use crate::tests::shared_test::*;

    #[test]
    fn add_bfe_test() {
        compare_prop_with_stack(
            &add_bfe_rast(),
            vec![bfe_lit(1000u64.into()), bfe_lit(BFieldElement::MAX.into())],
            vec![bfe_lit(999u64.into())],
        );
    }

    #[test]
    fn mul_bfe_test() {
        compare_prop_with_stack(
            &mul_bfe_rast(),
            vec![
                bfe_lit(10_000_000_000u64.into()),
                bfe_lit(10_000_000_000u64.into()),
            ],
            vec![bfe_lit(7766279652927078395u64.into())],
        );
    }

    #[test]
    fn cast_from_bool_test() {
        compare_prop_with_stack(
            &cast_from_bool_rast(),
            vec![bool_lit(false)],
            vec![bfe_lit(0u64.into())],
        );
        compare_prop_with_stack(
            &cast_from_bool_rast(),
            vec![bool_lit(true)],
            vec![bfe_lit(1u64.into())],
        );
    }
}
