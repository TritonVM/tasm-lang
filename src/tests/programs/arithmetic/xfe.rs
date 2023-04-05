use syn::parse_quote;

use crate::graft::item_fn;

#[allow(dead_code)]
fn instantiate_xfe_with_literal_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn instantiate_cfe() -> (XFieldElement, XFieldElement, XFieldElement, XFieldElement) {
            let one: XFieldElement = XFieldElement::new([
                BFieldElement::new(1),
                BFieldElement::new(0),
                BFieldElement::new(0),
            ]);
            let x: XFieldElement = XFieldElement::new([
                BFieldElement::new(0),
                BFieldElement::new(1),
                BFieldElement::new(0),
            ]);
            let x_squared: XFieldElement = XFieldElement::new([
                BFieldElement::new(0),
                BFieldElement::new(0),
                BFieldElement::new(1),
            ]);
            let big_numbers: XFieldElement = XFieldElement::new([
                BFieldElement::new(12345678901234567890u64),
                BFieldElement::new(12345678901234567892u64),
                BFieldElement::new(12345678901234567894u64),
            ]);
            return (one, x, x_squared, big_numbers);
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
pub fn mul_xfe_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn mul_xfe(lhs: XFieldElement, rhs: XFieldElement) ->  XFieldElement {
            return lhs * rhs;
        }
    })
}

#[allow(dead_code)]
pub fn div_xfe_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn div_xfe(numerator: XFieldElement, denominator: XFieldElement) ->  XFieldElement {
            return numerator / denominator;
        }
    })
}

#[cfg(test)]
mod compile_and_typecheck_tests {
    use crate::tests::shared_test::graft_check_compile_prop;

    use super::*;

    #[test]
    fn instantiate_xfe_with_literal_test() {
        graft_check_compile_prop(&instantiate_xfe_with_literal_rast());
    }

    #[test]
    fn add_xfe_test() {
        graft_check_compile_prop(&add_xfe_rast());
    }
}

#[cfg(test)]
mod run_tests {
    use twenty_first::shared_math::{other::random_elements, x_field_element::XFieldElement};

    use super::*;
    use crate::tests::shared_test::*;

    #[test]
    fn instantiate_xfe_with_literal_test() {
        compare_prop_with_stack(
            &instantiate_xfe_with_literal_rast(),
            vec![],
            vec![
                xfe_lit(XFieldElement::new([1u64.into(), 0u64.into(), 0u64.into()])),
                xfe_lit(XFieldElement::new([0u64.into(), 1u64.into(), 0u64.into()])),
                xfe_lit(XFieldElement::new([0u64.into(), 0u64.into(), 1u64.into()])),
                xfe_lit(XFieldElement::new([
                    12345678901234567890u64.into(),
                    12345678901234567892u64.into(),
                    12345678901234567894u64.into(),
                ])),
            ],
        );
    }

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

    #[test]
    fn mul_xfe_test() {
        let test_iterations = 5;
        let lhs: Vec<XFieldElement> = random_elements(test_iterations);
        let rhs: Vec<XFieldElement> = random_elements(test_iterations);
        for i in 0..test_iterations {
            let expected = xfe_lit(lhs[i] * rhs[i]);
            compare_prop_with_stack(
                &mul_xfe_rast(),
                vec![xfe_lit(lhs[i]), xfe_lit(rhs[i])],
                vec![expected],
            );
        }
    }

    #[test]
    fn div_xfe_run_test() {
        compare_prop_with_stack(
            &div_xfe_rast(),
            vec![
                xfe_lit(XFieldElement::new([
                    14058438416488592029u64.into(),
                    9323205069984740887u64.into(),
                    17067945378368492892u64.into(),
                ])),
                xfe_lit(XFieldElement::new([
                    1830538383633518907u64.into(),
                    6059860057029841626u64.into(),
                    16015800434434079444u64.into(),
                ])),
            ],
            vec![xfe_lit(XFieldElement::new([
                14483313154228911360u64.into(),
                11258517663240461652u64.into(),
                15927246224920701489u64.into(),
            ]))],
        );
    }
}
