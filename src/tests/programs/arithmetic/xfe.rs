use syn::parse_quote;

use crate::graft::item_fn;

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
pub fn div_xfe_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn div_xfe(rhs: XFieldElement, lhs: XFieldElement) ->  XFieldElement {
            return rhs / lhs;
        }
    })
}

#[cfg(test)]
mod compile_and_typecheck_tests {
    use crate::tests::shared_test::graft_check_compile_prop;

    use super::*;

    #[test]
    fn add_xfe_test() {
        graft_check_compile_prop(&add_xfe_rast());
    }
}

#[cfg(test)]
mod run_tests {
    use twenty_first::shared_math::x_field_element::XFieldElement;

    use super::*;
    use crate::tests::shared_test::*;

    #[test]
    fn add_xfe_run_test() {
        compare_prop_with_stack(
            &add_xfe_rast(),
            vec![
                xfe_lit(XFieldElement::new([
                    100u64.into(),
                    200u64.into(),
                    300u64.into(),
                ])),
                xfe_lit(XFieldElement::new([1u64.into(), 2u64.into(), 3u64.into()])),
            ],
            vec![xfe_lit(XFieldElement::new([
                101u64.into(),
                202u64.into(),
                303u64.into(),
            ]))],
        );

        compare_prop_with_stack(
            &add_xfe_rast(),
            vec![
                xfe_lit(XFieldElement::new([
                    (1u64 << 63).into(),
                    ((1u64 << 63) + 10).into(),
                    (1u64 << 63).into(),
                ])),
                xfe_lit(XFieldElement::new([
                    (1u64 << 63).into(),
                    (1u64 << 63).into(),
                    ((1u64 << 63) + 12).into(),
                ])),
            ],
            vec![xfe_lit(XFieldElement::new([
                (u32::MAX as u64).into(),
                (u32::MAX as u64 + 10).into(),
                (u32::MAX as u64 + 12).into(),
            ]))],
        );
    }
}
