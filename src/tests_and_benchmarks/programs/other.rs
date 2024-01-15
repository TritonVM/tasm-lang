use syn::parse_quote;

use crate::graft::item_fn;

#[cfg(test)]
mod compile_and_typecheck_tests {
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn inferred_literals_test() {
        graft_check_compile_prop(&inferred_literals(), crate::ast_types::ListType::Safe);

        fn inferred_literals() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn main() {
                    // infer from let context
                    let a: u32 = 0;
                    let b: u64 = 1;

                    // infer as lhs and rhs
                    let add_1: u32 = a + 1;
                    let add_2: u32 = 2 + a;
                    let add_3: u64 = b + 3;
                    let add_4: u64 = 4 + b;

                    let sub_1: u32 = a - 5;
                    let sub_2: u32 = 6 - a;
                    let sub_3: u64 = b - 7;
                    let sub_4: u64 = 8 - b;

                    let bit_and_1: u32 = a & 9;
                    let bit_and_2: u32 = 10 & a;
                    let bit_and_3: u64 = b & 11;
                    let bit_and_4: u64 = 12 & b;

                    let bit_xor_1: u32 = a ^ 13;
                    let bit_xor_2: u32 = 14 ^ a;
                    let bit_xor_3: u64 = b ^ 15;
                    let bit_xor_4: u64 = 16 ^ b;

                    let div_1: u32 = a / 2;
                    let div_2: u32 = 18 / a;
                    let div_3: u64 = b / 2;
                    let div_4: u64 = 20 / b;

                    let mul_1: u32 = a * 21;
                    let mul_2: u32 = 22 * a;
                    let mul_3: u64 = b * 23;
                    let mul_4: u64 = 24 * b;

                    let rem_1: u32 = a % 25;
                    let rem_2: u32 = 26 % a;
                    let rem_3: u64 = b % 27;
                    let rem_4: u64 = 28 % b;

                    let bit_shl_1: u32 = 1 << 3;
                    let bit_shl_2: u32 = 1 << a;
                    let bit_shl_3: u32 = 2 << 1;
                    let bit_shl_4: u32 = a << 1;
                    let bit_shl_5: u64 = b << 5;
                    let bit_shl_6: u64 = 1 << 5;
                    let bit_shl_7: u64 = 1 << a;
                    let bit_shl_8: u64 = 6 << 7;
                    let bit_shl_9: u64 = 8 << a;

                    let bit_shr_6: u64 = 1 >> 5;
                    let bit_shr_7: u64 = 1 >> a;

                    let three: u64 = tasm::tasm_arithmetic_u64_add(1, 2);

                    let mut arr: Vec<u64> = Vec::<u64>::with_capacity(16u32);
                    arr[0] = b;
                    arr[a] = b + 1;
                    arr[2 * a + 3] = 1 << (4 / a + 5);

                    arr.push(4);

                    return;
                }
            })
        }
    }

    #[test]
    fn nop_test() {
        graft_check_compile_prop(&nop_rast(), crate::ast_types::ListType::Safe);

        fn nop_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn nop_nop() {
                    return;
                }
            })
        }
    }

    #[should_panic]
    #[test]
    fn missing_mut_keyword_test() {
        graft_check_compile_prop(&missing_mut_keyword(), crate::ast_types::ListType::Safe);

        fn missing_mut_keyword() -> syn::ItemFn {
            item_fn(parse_quote!(
                fn missing_mut() {
                    let a = 5000u64;
                    a = a + 1;
                }
            ))
        }
    }
}

#[cfg(test)]
mod run_tests {
    use rand::thread_rng;
    use rand::Rng;

    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn tasm_argument_evaluation_order_test() {
        fn tasm_argument_evaluation_order_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn tasm_argument_evaluation_order() -> u64 {
                    let five: u64 = tasm::tasm_arithmetic_u64_sub(2, 7);
                    return five;
                }
            })
        }

        multiple_compare_prop_with_stack_safe_lists(
            &tasm_argument_evaluation_order_rast(),
            vec![InputOutputTestCase::new(vec![], vec![u64_lit(5)])],
        );
    }

    #[test]
    fn nested_if_expressions_test() {
        fn nested_if_expressions_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn if_expressions(input0: bool, input1: bool) -> u32 {
                    let ret: u32 = if input0 {
                        if input1 {
                            3u32
                        } else {
                            2u32
                        }
                    } else {
                        if input1 {
                            1u32
                        } else {
                            0u32
                        }
                    };

                    return ret;
                }
            })
        }

        multiple_compare_prop_with_stack_safe_lists(
            &nested_if_expressions_rast(),
            vec![
                InputOutputTestCase::new(vec![bool_lit(false), bool_lit(false)], vec![u32_lit(0)]),
                InputOutputTestCase::new(vec![bool_lit(false), bool_lit(true)], vec![u32_lit(1)]),
                InputOutputTestCase::new(vec![bool_lit(true), bool_lit(false)], vec![u32_lit(2)]),
                InputOutputTestCase::new(vec![bool_lit(true), bool_lit(true)], vec![u32_lit(3)]),
            ],
        );
    }

    #[test]
    fn simple_recursive_pow_test() {
        fn simple_recursive_pow_rast() -> syn::ItemFn {
            // A simple, recursive function that is *not* symmetric in it's input arguments.
            // In other words: it gives a difference result if the input arguments are flipped.
            // This test is added to ensure consistency in the order of input arguments.
            item_fn(parse_quote! {
                fn simple_recursive_pow(a: u64, b: u64) -> u64 {
                    return if b == 0u64 {
                        1u64
                    } else {
                        a * simple_recursive_pow(a, b - 1u64)
                    };
                }
            })
        }

        multiple_compare_prop_with_stack_safe_lists(
            &simple_recursive_pow_rast(),
            vec![
                // There's a significant amount of explosives in the trash receptacle next to you
                InputOutputTestCase::new(vec![u64_lit(7), u64_lit(1)], vec![u64_lit(7)]),
                InputOutputTestCase::new(vec![u64_lit(7), u64_lit(2)], vec![u64_lit(49)]),
                // Shut up, McClane, I'm good at this
                InputOutputTestCase::new(vec![u64_lit(7), u64_lit(3)], vec![u64_lit(343)]),
                InputOutputTestCase::new(vec![u64_lit(7), u64_lit(4)], vec![u64_lit(2401)]),
                InputOutputTestCase::new(vec![u64_lit(1), u64_lit(1)], vec![u64_lit(1)]),
                InputOutputTestCase::new(
                    vec![u64_lit(1000), u64_lit(6)],
                    vec![u64_lit(1_000_000_000_000_000_000)],
                ),
                InputOutputTestCase::new(vec![u64_lit(2), u64_lit(3)], vec![u64_lit(8)]),
                InputOutputTestCase::new(vec![u64_lit(3), u64_lit(2)], vec![u64_lit(9)]),
            ],
        );
    }

    #[test]
    fn simple_while_loop_run_test() {
        fn simple_while_loop() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn simple_while_loop() -> u32 {
                    let mut acc: u32 = 0u32;
                    let mut i: u32 = 0u32;
                    while i < 101u32 {
                        acc = acc + i;
                        i = i + 1u32;
                    }

                    return acc;
                }
            })
        }

        compare_prop_with_stack_safe_lists(&simple_while_loop(), vec![], vec![u32_lit(5050)]);
    }

    #[test]
    fn complicated_while_loop_test() {
        compare_prop_with_stack_safe_lists(
            &longer_while_loop(),
            vec![u32_lit(1000)],
            vec![u64_lit(2641)],
        );
        compare_prop_with_stack_safe_lists(
            &while_loop_with_declarations(),
            vec![u32_lit(2000)],
            vec![u64_lit(3641)],
        );
        compare_prop_with_stack_safe_lists(
            &while_loop_with_declarations(),
            vec![u32_lit(2001)],
            vec![u64_lit(3642)],
        );

        fn longer_while_loop() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn longer_while_loop(a: u32) -> u64 {
                    // Should return `1641 + a`
                    let mut ret: u64 = 600u64;
                    let var0: u64 = 77u64;
                    let var1: u64 = 10u64;
                    let mut j: u64 = 23u64;
                    let mut i: u32 = 0u32;
                    while i < var1 as u32 {
                        i = i + 1u32;
                        ret = ret + var0;
                        ret = ret + j;
                        j = j - 1u64;
                    }

                    ret = 9u64 + ret + a as u64;

                    return ret + var0;
                }
            })
        }

        fn while_loop_with_declarations() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn while_loop_with_declarations(a: u32) -> u64 {
                    // Should return `1641 + a`
                    let mut ret: u64 = 600u64;
                    let var0: u64 = 77u64;
                    let var1: u64 = 10u64;
                    let mut j: u64 = 23u64;
                    let mut i: u32 = 0u32;
                    while i < var1 as u32 {
                        let g: u32 = 10000u32;
                        i = i + 1u32;
                        ret = ret + var0;
                        ret = ret + j;
                        j = j - 1u64;
                    }

                    ret = 9u64 + ret + a as u64;

                    return ret + var0;
                }
            })
        }
    }

    #[test]
    fn code_block_run_test() {
        fn prop_code_block(input: u64) {
            let inputs = vec![u64_lit(input)];
            let outputs = vec![u32_lit(2 * (input as u32) + 2)];
            compare_prop_with_stack_safe_lists(&code_block(), inputs, outputs);
        }

        let mut rng = thread_rng();
        for _ in 0..10 {
            prop_code_block(rng.gen_range(0..(1u64 << 30)));
        }

        fn code_block() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn code_block(a: u64) -> u32 {
                    let b: u64 = a + 2u64;
                    {
                        let c: u32 = 0u32;
                        let d: u64 = 1u64;
                        let e: u64 = a + b + d;
                    }

                    return a as u32 + b as u32;
                }
            })
        }
    }

    #[test]
    fn tuple_support_run_test() {
        let outputs = vec![bool_lit(true), u32_lit(42), u64_lit(100)];

        compare_prop_with_stack_safe_lists(&tuple_support(), vec![], outputs);

        fn tuple_support() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn return_many() -> (bool, u32, u64) {
                    let a: bool = true;
                    let b: u32 = 42u32;
                    let c: u64 = 100u64;

                    return (a, b, c);
                }
            })
        }
    }

    #[test]
    fn return_tuple_element() {
        // let tuple: (bool, u32, u64, u64, u32) = (true, 42u32, 10000000000u64, 60000000000u64, 20000u32);
        compare_prop_with_stack_safe_lists(&return_tuple_element_0(), vec![], vec![bool_lit(true)]);
        compare_prop_with_stack_safe_lists(&return_tuple_element_1(), vec![], vec![u32_lit(42)]);
        compare_prop_with_stack_safe_lists(
            &return_tuple_element_2(),
            vec![],
            vec![u64_lit(10_000_000_000)],
        );
        compare_prop_with_stack_safe_lists(
            &return_tuple_element_3(),
            vec![],
            vec![u64_lit(60_000_000_000)],
        );
        compare_prop_with_stack_safe_lists(&return_tuple_element_4(), vec![], vec![u32_lit(20000)]);

        fn return_tuple_element_0() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn return_tuple_element() -> bool {
                    let tuple: (bool, u32, u64, u64, u32) = (true, 42u32, 10000000000u64, 60000000000u64, 20000u32);
                    return tuple.0;
                }
            })
        }

        fn return_tuple_element_1() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn return_tuple_element() -> u32 {
                    let tuple: (bool, u32, u64, u64, u32) = (true, 42u32, 10000000000u64, 60000000000u64, 20000u32);
                    return tuple.1;
                }
            })
        }

        fn return_tuple_element_2() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn return_tuple_element() -> u64 {
                    let tuple: (bool, u32, u64, u64, u32) = (true, 42u32, 10000000000u64, 60000000000u64, 20000u32);
                    return tuple.2;
                }
            })
        }

        fn return_tuple_element_3() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn return_tuple_element() -> u64 {
                    let tuple: (bool, u32, u64, u64, u32) = (true, 42u32, 10000000000u64, 60000000000u64, 20000u32);
                    return tuple.3;
                }
            })
        }

        fn return_tuple_element_4() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn return_tuple_element() -> u32 {
                    let tuple: (bool, u32, u64, u64, u32) = (true, 42u32, 10000000000u64, 60000000000u64, 20000u32);
                    return tuple.4;
                }
            })
        }
    }

    #[test]
    fn allow_mutable_tuple_test() {
        compare_prop_with_stack_safe_lists(
            &allow_mutable_tuple_rast(),
            vec![],
            vec![u64_lit(3), u64_lit(4)],
        );

        fn allow_mutable_tuple_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn allow_mutable_tuple() -> (u64, u64) {
                    let mut tuple: (u64, u64) = (1u64, 2u64);
                    tuple.0 = 3u64;
                    let b: u32 = 100;
                    let c: u32 = 101;
                    tuple.1 = 4u64;
                    let d: (u64, u32) = (1000u64, 2000u32);

                    return tuple;
                }
            })
        }
    }

    #[test]
    fn allow_mutable_tuple_complicated_test() {
        compare_prop_with_stack_safe_lists(
            &allow_mutable_tuple_complicated_rast(),
            vec![],
            vec![u64_lit(3), u64_lit(4)],
        );

        fn allow_mutable_tuple_complicated_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn allow_mutable_tuple_complicated() -> (u64, u64) {
                    let a: u64 = 1u64;
                    let mut tuple: (u64, u64) = (1u64, 2u64);
                    tuple.0 = 3u64;
                    let b: u32 = 100;
                    let c: u32 = 101;
                    tuple.1 = 4u64;
                    let d: (u64, u32) = (1000u64, 2000u32);

                    return tuple;
                }
            })
        }
    }

    #[test]
    fn allow_mutable_triplet_test() {
        compare_prop_with_stack_safe_lists(
            &allow_mutable_triplet_rast(),
            vec![],
            vec![u64_lit(4), u64_lit(5), u32_lit(6)],
        );

        fn allow_mutable_triplet_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn allow_mutable_triplet() -> (u64, u64, u32) {
                    let mut tuple: (u64, u64, u32) = (1u64, 2u64, 3u32);
                    tuple.0 = 4u64;
                    tuple.1 = 5u64;
                    tuple.2 = 6u32;

                    return tuple;
                }
            })
        }
    }

    #[test]
    fn overwrite_values_test() {
        compare_prop_with_stack_safe_lists(
            &overwrite_values_rast(),
            vec![],
            vec![u32_lit(1200), u32_lit(1300)],
        );
    }

    fn overwrite_values_rast() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn overwrite_values() -> (u32, u32) {
                let mut a: u32 = 100;
                a = 200;
                a = 300;
                a = a + 300;
                a = a + a;

                return (a, a + 100u32);
            }
        })
    }
}
