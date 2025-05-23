use syn::parse_quote;

#[cfg(test)]
mod run_tests {
    use itertools::Itertools;
    use num::One;
    use num::Zero;
    use rand::random;
    use tasm_lib::triton_vm::prelude::*;
    use tasm_lib::twenty_first::math::other::random_elements;
    use tasm_lib::twenty_first::math::x_field_element::EXTENSION_DEGREE;

    use super::*;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn instantiate_xfe_with_num_test() {
        compare_prop_with_stack_safe_lists(
            &instantiate_xfe_with_num_traits_rast(),
            vec![],
            vec![
                xfe_lit(XFieldElement::one()),
                xfe_lit(XFieldElement::zero()),
            ],
        );

        fn instantiate_xfe_with_num_traits_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn zeros_and_one() -> (XFieldElement, XFieldElement) {
                    let a: XFieldElement = XFieldElement::one();
                    let b: XFieldElement = XFieldElement::zero();

                    return (a, b);
                }
            })
        }
    }

    #[test]
    fn instantiate_xfe_with_literal_test() {
        compare_prop_with_stack_safe_lists(
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
    }

    #[test]
    fn add_xfe_run_test() {
        compare_prop_with_stack_safe_lists(
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

        compare_prop_with_stack_safe_lists(
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

        fn add_xfe_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn add_xfe(lhs: XFieldElement, rhs: XFieldElement) -> XFieldElement {
                    let c: XFieldElement = lhs + rhs;
                    return c;
                }
            })
        }
    }

    #[test]
    fn sub_xfe_test() {
        // TODO: Increase test iterations when we can compare multiple test cases without
        // compiling multiply times
        let test_iterations = 1;
        let lhs: Vec<XFieldElement> = random_elements(test_iterations);
        let rhs: Vec<XFieldElement> = random_elements(test_iterations);
        for i in 0..test_iterations {
            let expected = xfe_lit(lhs[i] - rhs[i]);
            compare_prop_with_stack_safe_lists(
                &sub_xfe_rast(),
                vec![xfe_lit(lhs[i]), xfe_lit(rhs[i])],
                vec![expected],
            );
        }

        fn sub_xfe_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn sub_xfe(lhs: XFieldElement, rhs: XFieldElement) -> XFieldElement {
                    let c: XFieldElement = lhs - rhs;
                    return c;
                }
            })
        }
    }

    #[test]
    fn negate_xfe_test() {
        let test_iterations = 1;
        let values: Vec<XFieldElement> = random_elements(test_iterations);
        for value in values {
            compare_prop_with_stack_safe_lists(
                &negate_xfe_rast(),
                vec![xfe_lit(value)],
                vec![xfe_lit(-value)],
            );
        }

        fn negate_xfe_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn negate_xfe(value: XFieldElement) -> XFieldElement {
                    return -value;
                }
            })
        }
    }

    #[test]
    fn mul_xfe_test() {
        let test_iterations = 5;
        let lhs: Vec<XFieldElement> = random_elements(test_iterations);
        let rhs: Vec<XFieldElement> = random_elements(test_iterations);
        for i in 0..test_iterations {
            let expected = xfe_lit(lhs[i] * rhs[i]);
            compare_prop_with_stack_safe_lists(
                &mul_xfe_rast(),
                vec![xfe_lit(lhs[i]), xfe_lit(rhs[i])],
                vec![expected],
            );
        }

        fn mul_xfe_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn mul_xfe(lhs: XFieldElement, rhs: XFieldElement) ->  XFieldElement {
                    return lhs * rhs;
                }
            })
        }
    }

    #[test]
    fn mul_lifted_xfes_test() {
        let test_iterations = 20;
        let lhss: Vec<BFieldElement> = random_elements(test_iterations);
        let rhss: Vec<BFieldElement> = random_elements(test_iterations);
        let test_cases = lhss
            .into_iter()
            .zip_eq(rhss)
            .map(|(lhs, rhs)| {
                InputOutputTestCase::new(
                    vec![bfe_lit(lhs), bfe_lit(rhs)],
                    vec![xfe_lit((lhs * rhs).lift())],
                )
            })
            .collect_vec();
        multiple_compare_prop_with_stack_safe_lists(&mul_lifted_xfes_rast(), test_cases);

        fn mul_lifted_xfes_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn mul_lifted_xfes(lhs: BFieldElement, rhs: BFieldElement) -> XFieldElement {
                    return lhs.lift() * rhs.lift();
                }
            })
        }
    }

    #[test]
    fn div_xfe_run_test() {
        compare_prop_with_stack_safe_lists(
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

        fn div_xfe_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn div_xfe(numerator: XFieldElement, denominator: XFieldElement) ->  XFieldElement {
                    return numerator / denominator;
                }
            })
        }
    }

    #[test]
    fn eq_xfe_test() {
        let test_iterations = 1;
        let lhs: Vec<XFieldElement> = random_elements(test_iterations);
        let rhs: Vec<XFieldElement> = random_elements(test_iterations);
        for i in 0..test_iterations {
            let expected = vec![bool_lit(lhs == rhs), bool_lit(lhs != rhs)];
            compare_prop_with_stack_safe_lists(
                &eq_xfe_rast(),
                vec![xfe_lit(lhs[i]), xfe_lit(rhs[i])],
                expected.clone(),
            );
            compare_prop_with_stack_safe_lists(
                &eq_xfe_with_assignment_rast(),
                vec![xfe_lit(lhs[i]), xfe_lit(rhs[i])],
                expected,
            );

            compare_prop_with_stack_safe_lists(
                &eq_xfe_rast(),
                vec![xfe_lit(lhs[i]), xfe_lit(lhs[i])],
                vec![bool_lit(true), bool_lit(false)],
            );
            compare_prop_with_stack_safe_lists(
                &eq_xfe_with_assignment_rast(),
                vec![xfe_lit(lhs[i]), xfe_lit(lhs[i])],
                vec![bool_lit(true), bool_lit(false)],
            );

            // Verify that all coefficients in the XFE are tested
            for j in 0..EXTENSION_DEGREE {
                let mut new_lhs = lhs[i];
                new_lhs.increment(j);
                compare_prop_with_stack_safe_lists(
                    &eq_xfe_rast(),
                    vec![xfe_lit(new_lhs), xfe_lit(lhs[i])],
                    vec![bool_lit(false), bool_lit(true)],
                );
                compare_prop_with_stack_safe_lists(
                    &eq_xfe_with_assignment_rast(),
                    vec![xfe_lit(new_lhs), xfe_lit(lhs[i])],
                    vec![bool_lit(false), bool_lit(true)],
                );
                compare_prop_with_stack_safe_lists(
                    &eq_xfe_rast(),
                    vec![xfe_lit(lhs[i]), xfe_lit(new_lhs)],
                    vec![bool_lit(false), bool_lit(true)],
                );
                compare_prop_with_stack_safe_lists(
                    &eq_xfe_with_assignment_rast(),
                    vec![xfe_lit(lhs[i]), xfe_lit(new_lhs)],
                    vec![bool_lit(false), bool_lit(true)],
                );
            }
        }

        compare_prop_with_stack_safe_lists(
            &eq_xfe_rast(),
            vec![
                xfe_lit(XFieldElement::zero()),
                xfe_lit(XFieldElement::zero()),
            ],
            vec![bool_lit(true), bool_lit(false)],
        );
        compare_prop_with_stack_safe_lists(
            &eq_xfe_with_assignment_rast(),
            vec![
                xfe_lit(XFieldElement::zero()),
                xfe_lit(XFieldElement::zero()),
            ],
            vec![bool_lit(true), bool_lit(false)],
        );

        compare_prop_with_stack_safe_lists(
            &eq_xfe_rast(),
            vec![xfe_lit(XFieldElement::one()), xfe_lit(XFieldElement::one())],
            vec![bool_lit(true), bool_lit(false)],
        );
        compare_prop_with_stack_safe_lists(
            &eq_xfe_with_assignment_rast(),
            vec![xfe_lit(XFieldElement::one()), xfe_lit(XFieldElement::one())],
            vec![bool_lit(true), bool_lit(false)],
        );

        compare_prop_with_stack_safe_lists(
            &eq_xfe_rast(),
            vec![
                xfe_lit(XFieldElement::zero()),
                xfe_lit(XFieldElement::one()),
            ],
            vec![bool_lit(false), bool_lit(true)],
        );
        compare_prop_with_stack_safe_lists(
            &eq_xfe_with_assignment_rast(),
            vec![
                xfe_lit(XFieldElement::zero()),
                xfe_lit(XFieldElement::one()),
            ],
            vec![bool_lit(false), bool_lit(true)],
        );

        compare_prop_with_stack_safe_lists(
            &eq_xfe_rast(),
            vec![
                xfe_lit(XFieldElement::one()),
                xfe_lit(XFieldElement::zero()),
            ],
            vec![bool_lit(false), bool_lit(true)],
        );
        compare_prop_with_stack_safe_lists(
            &eq_xfe_with_assignment_rast(),
            vec![
                xfe_lit(XFieldElement::one()),
                xfe_lit(XFieldElement::zero()),
            ],
            vec![bool_lit(false), bool_lit(true)],
        );

        fn eq_xfe_with_assignment_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn eq_xfe(lhs: XFieldElement, rhs: XFieldElement) -> (bool, bool) {
                    let equal: bool = lhs == rhs;
                    let not_equal: bool = lhs != rhs;
                    return (equal, not_equal);
                }
            })
        }

        fn eq_xfe_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn eq_xfe(lhs: XFieldElement, rhs: XFieldElement) -> (bool, bool) {
                    return (lhs == rhs, lhs != rhs);
                }
            })
        }
    }

    #[test]
    fn unlift_xfe_test() {
        let random_bfe: BFieldElement = random();
        let lifted_to_xfe = random_bfe.lift();

        multiple_compare_prop_with_stack_safe_lists(
            &unlift_xfe_rast(),
            vec![InputOutputTestCase::new(
                vec![xfe_lit(lifted_to_xfe)],
                vec![bfe_lit(random_bfe)],
            )],
        );

        fn unlift_xfe_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn unlift_xfe(input: XFieldElement) -> BFieldElement {
                    return input.unlift().unwrap();
                }
            })
        }
    }

    #[test]
    fn long_expression_1_test() {
        let test_iterations = 5;
        let inputs_0: Vec<XFieldElement> = random_elements(test_iterations);
        let inputs_1: Vec<XFieldElement> = random_elements(test_iterations);
        let inputs_2: Vec<XFieldElement> = random_elements(test_iterations);
        for i in 0..test_iterations {
            let expected = vec![
                xfe_lit(XFieldElement::zero()),
                xfe_lit(XFieldElement::new([
                    BFieldElement::new(6),
                    BFieldElement::new(7),
                    BFieldElement::new(9),
                ])),
                xfe_lit(inputs_1[i]),
            ];
            compare_prop_with_stack_safe_lists(
                &long_expression_1_rast(),
                vec![
                    xfe_lit(inputs_0[i]),
                    xfe_lit(inputs_1[i]),
                    xfe_lit(inputs_2[i]),
                ],
                expected,
            );
        }

        /// Always returns (0, [6,7,9], b)
        fn long_expression_1_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn long_expression_1(
                    a: XFieldElement,
                    b: XFieldElement,
                    c: XFieldElement,
                ) -> (XFieldElement, XFieldElement, XFieldElement) {
                    let val0: XFieldElement = XFieldElement::new([
                        BFieldElement::new(0),
                        BFieldElement::new(2),
                        BFieldElement::new(4),
                    ]);
                    let val1: XFieldElement = XFieldElement::new([
                        BFieldElement::new(1),
                        BFieldElement::new(3),
                        BFieldElement::new(5),
                    ]);
                    let val2: XFieldElement = XFieldElement::new([
                        BFieldElement::new(2),
                        BFieldElement::new(4),
                        BFieldElement::new(6),
                    ]);
                    let val3: XFieldElement = XFieldElement::new([
                        BFieldElement::new(3),
                        BFieldElement::new(5),
                        BFieldElement::new(7),
                    ]);
                    let val4: XFieldElement = XFieldElement::new([
                        BFieldElement::new(4),
                        BFieldElement::new(6),
                        BFieldElement::new(8),
                    ]);
                    let val5: XFieldElement = XFieldElement::new([
                        BFieldElement::new(5),
                        BFieldElement::new(7),
                        BFieldElement::new(9),
                    ]);

                    let res0: XFieldElement = a + val5 + b + c / c + c - a - b - c - val5 - val4 / val4;
                    let res1: XFieldElement = a * a + val5 + b * b + c * c - a * a - b * b - c * c + val2 / val2;

                    return (res0, res1, b);
                }
            })
        }
    }

    #[test]
    fn long_expression_2_test() {
        let test_iterations = 2;
        let inputs_0: Vec<XFieldElement> = random_elements(test_iterations);
        let inputs_1: Vec<XFieldElement> = random_elements(test_iterations);
        let inputs_2: Vec<XFieldElement> = random_elements(test_iterations);
        let inputs_3: Vec<XFieldElement> = random_elements(test_iterations);

        for i in 0..test_iterations {
            let expected = long_expression_2(inputs_0[i], inputs_1[i], inputs_2[i], inputs_3[i]);
            let expected = vec![
                xfe_lit(expected.0),
                xfe_lit(expected.1),
                xfe_lit(expected.2),
            ];
            compare_prop_with_stack_safe_lists(
                &long_expression_2_rast(),
                vec![
                    xfe_lit(inputs_0[i]),
                    xfe_lit(inputs_1[i]),
                    xfe_lit(inputs_2[i]),
                    xfe_lit(inputs_3[i]),
                ],
                expected,
            );
        }

        #[allow(clippy::needless_return)]
        fn long_expression_2(
            a: XFieldElement,
            b: XFieldElement,
            c: XFieldElement,
            d: XFieldElement,
        ) -> (XFieldElement, XFieldElement, XFieldElement) {
            let val0: XFieldElement = XFieldElement::new([
                BFieldElement::new(0),
                BFieldElement::new(2),
                BFieldElement::new(4),
            ]);
            let val1: XFieldElement = XFieldElement::new([
                BFieldElement::new(1),
                BFieldElement::new(3),
                BFieldElement::new(5),
            ]);
            let val2: XFieldElement = XFieldElement::new([
                BFieldElement::new(2),
                BFieldElement::new(4),
                BFieldElement::new(6),
            ]);

            let res0: XFieldElement =
                val1 + (a + b + c + d) * a - d / a + d / b + (a * b * c) * (a + b) - val0 / a;
            let res1: XFieldElement =
                (-d / a + d / b + (a * b * c) * (a + b) - val0 / a) * ((c - d) * (a + d) * (a + b));
            let res2: XFieldElement = (a * (b + c) / d) + (c * d - a * b) * (a + b - c) / (d * d)
                - (a / b * c + d * (b - a)) * (c / d + a)
                + (b * (a + c) - d) / (a * b)
                + (c * (d - b) / a) * (b * c / d)
                + val2
                    * XFieldElement::new([
                        BFieldElement::new(1),
                        BFieldElement::new(3),
                        BFieldElement::new(5),
                    ])
                - a * (b / c - d * a)
                + (d * (c - a) * b) / (c * a);

            return (res0, res1, res2);
        }

        fn long_expression_2_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn long_expression_2(
                    a: XFieldElement,
                    b: XFieldElement,
                    c: XFieldElement,
                    d: XFieldElement,
                ) -> (XFieldElement, XFieldElement, XFieldElement) {
                    let val0: XFieldElement = XFieldElement::new([
                        BFieldElement::new(0),
                        BFieldElement::new(2),
                        BFieldElement::new(4),
                    ]);
                    let val1: XFieldElement = XFieldElement::new([
                        BFieldElement::new(1),
                        BFieldElement::new(3),
                        BFieldElement::new(5),
                    ]);
                    let val2: XFieldElement = XFieldElement::new([
                        BFieldElement::new(2),
                        BFieldElement::new(4),
                        BFieldElement::new(6),
                    ]);

                    let res0: XFieldElement =
                        val1 + (a + b + c + d) * a - d / a + d / b + (a * b * c) * (a + b) - val0 / a;
                    let res1: XFieldElement =
                        (-d / a + d / b + (a * b * c) * (a + b) - val0 / a) * ((c - d) * (a + d) * (a + b));
                    let res2: XFieldElement = (a * (b + c) / d) + (c * d - a * b) * (a + b - c) / (d * d)
                        - (a / b * c + d * (b - a)) * (c / d + a)
                        + (b * (a + c) - d) / (a * b)
                        + (c * (d - b) / a) * (b * c / d)
                        + val2
                            * XFieldElement::new([
                                BFieldElement::new(1),
                                BFieldElement::new(3),
                                BFieldElement::new(5),
                            ])
                        - a * (b / c - d * a)
                        + (d * (c - a) * b) / (c * a);

                    return (res0, res1, res2);
                }
            })
        }
    }

    #[test]
    fn long_expression_3_test() {
        #[allow(clippy::needless_return)]
        fn long_expression_3(
            a: XFieldElement,
            b: XFieldElement,
            c: XFieldElement,
            d: XFieldElement,
        ) -> (XFieldElement, XFieldElement, XFieldElement, XFieldElement) {
            let val0: XFieldElement = XFieldElement::new([
                BFieldElement::new(0),
                BFieldElement::new(2),
                BFieldElement::new(4),
            ]);
            let val1: XFieldElement = XFieldElement::new([
                BFieldElement::new(1),
                BFieldElement::new(3),
                BFieldElement::new(5),
            ]);
            let val2: XFieldElement = XFieldElement::new([
                BFieldElement::new(2),
                BFieldElement::new(4),
                BFieldElement::new(6),
            ]);

            let res0: XFieldElement =
                val1 + (a + b + c + d) * a - d / a + d / b + (a * b * c) * (a + b) - val0 / a;
            let res1: XFieldElement =
                (-d / a + d / b + (a * b * c) * (a + b) - val0 / a) * ((c - d) * (a + d) * (a + b));
            let res2: XFieldElement = (a * (b + c) / d) + (c * d - a * b) * (a + b - c) / (d * d)
                - (a / b * c + d * (b - a)) * (c / d + a)
                + (b * (a + c) - d) / (a * b)
                + (c * (d - b) / a) * (b * c / d)
                + val2
                    * XFieldElement::new([
                        BFieldElement::new(1),
                        BFieldElement::new(3),
                        BFieldElement::new(5),
                    ])
                - a * (b / c - d * a)
                + (d * (c - a) * b) / (c * a);

            return (
                a + val0 + res0 + res1 + res0,
                b + val0 + res2 * res1,
                c + val0 + val0,
                d - val0 + val1,
            );
        }

        fn long_expression_3_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn long_expression_3(
                    a: XFieldElement,
                    b: XFieldElement,
                    c: XFieldElement,
                    d: XFieldElement,
                ) -> (XFieldElement, XFieldElement, XFieldElement, XFieldElement) {
                    let val0: XFieldElement = XFieldElement::new([
                        BFieldElement::new(0),
                        BFieldElement::new(2),
                        BFieldElement::new(4),
                    ]);
                    let val1: XFieldElement = XFieldElement::new([
                        BFieldElement::new(1),
                        BFieldElement::new(3),
                        BFieldElement::new(5),
                    ]);
                    let val2: XFieldElement = XFieldElement::new([
                        BFieldElement::new(2),
                        BFieldElement::new(4),
                        BFieldElement::new(6),
                    ]);

                    let res0: XFieldElement =
                        val1 + (a + b + c + d) * a - d / a + d / b + (a * b * c) * (a + b) - val0 / a;
                    let res1: XFieldElement =
                        (-d / a + d / b + (a * b * c) * (a + b) - val0 / a) * ((c - d) * (a + d) * (a + b));
                    let res2: XFieldElement = (a * (b + c) / d) + (c * d - a * b) * (a + b - c) / (d * d)
                        - (a / b * c + d * (b - a)) * (c / d + a)
                        + (b * (a + c) - d) / (a * b)
                        + (c * (d - b) / a) * (b * c / d)
                        + val2
                            * XFieldElement::new([
                                BFieldElement::new(1),
                                BFieldElement::new(3),
                                BFieldElement::new(5),
                            ])
                        - a * (b / c - d * a)
                        + (d * (c - a) * b) / (c * a);

                    return (a + val0 + res0 + res1 + res0, b + val0 + res2 * res1, c + val0 + val0, d - val0 + val1);
                }
            })
        }

        let test_iterations = 2;
        let inputs_0: Vec<XFieldElement> = random_elements(test_iterations);
        let inputs_1: Vec<XFieldElement> = random_elements(test_iterations);
        let inputs_2: Vec<XFieldElement> = random_elements(test_iterations);
        let inputs_3: Vec<XFieldElement> = random_elements(test_iterations);

        for i in 0..test_iterations {
            let expected = long_expression_3(inputs_0[i], inputs_1[i], inputs_2[i], inputs_3[i]);
            let expected = vec![
                xfe_lit(expected.0),
                xfe_lit(expected.1),
                xfe_lit(expected.2),
                xfe_lit(expected.3),
            ];
            compare_prop_with_stack_safe_lists(
                &long_expression_3_rast(),
                vec![
                    xfe_lit(inputs_0[i]),
                    xfe_lit(inputs_1[i]),
                    xfe_lit(inputs_2[i]),
                    xfe_lit(inputs_3[i]),
                ],
                expected,
            );
        }
    }
}
