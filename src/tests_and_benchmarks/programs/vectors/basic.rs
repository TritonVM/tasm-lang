pub(crate) mod run_tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use num::One;
    use rand::random;
    use syn::parse_quote;
    use tasm_lib::memory::dyn_malloc;
    use tasm_lib::triton_vm::prelude::*;

    use crate::ast_types;
    use crate::ast_types::DataType;

    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn very_simple_list_support_test() {
        fn very_simple_list_support() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn make_very_short_list() -> u32 {
                    let mut a: Vec<u32> = Vec::<u32>::with_capacity(17);
                    a.push(2000u32);

                    return a[0];
                }
            })
        }

        let inputs = vec![];
        let expected_outputs = vec![u32_lit(2000)];
        compare_prop_with_stack_safe_lists(&very_simple_list_support(), inputs, expected_outputs);
    }

    #[test]
    fn clear_support_test() {
        fn clear_list_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn clear_list() -> usize {
                    let mut a: Vec<u32> = Vec::<u32>::with_capacity(17);
                    a.push(2001u32);
                    a.push(2002u32);
                    a.push(2003u32);
                    a.push(2004u32);
                    a.push(2005u32);
                    a.push(2006u32);
                    a.push(2007u32);
                    a.push(2008u32);

                    // Length is 8
                    a.clear();

                    a.push(2009u32);
                    a.push(2010u32);
                    a.push(2011u32);

                    // Length is 3
                    return a.len();
                }
            })
        }

        compare_prop_with_stack_safe_lists(&clear_list_rast(), vec![], vec![u32_lit(3)]);
        compare_prop_with_stack_unsafe_lists(&clear_list_rast(), vec![], vec![u32_lit(3)]);
    }

    #[test]
    fn simple_digest_list_support_test() {
        let random_digest: Digest = random();
        let inputs = vec![digest_lit(random_digest)];
        let expected_outputs = vec![digest_lit(random_digest)];
        let rust_ast = item_fn(parse_quote! {
            fn make_short_digest_list(digest: Digest) -> Digest {
                let mut a: Vec<Digest> = Vec::<Digest>::with_capacity(17);
                a.push(digest);
                return a[0];
            }
        });
        compare_prop_with_stack_safe_lists(&rust_ast, inputs, expected_outputs);
    }

    #[test]
    fn build_digest_list_with_indexing_test() {
        let random_digest0: Digest = random();
        let random_digest1: Digest = random();
        let random_digest2: Digest = random();
        let inputs = vec![
            digest_lit(random_digest1),
            digest_lit(random_digest0),
            digest_lit(random_digest2),
        ];
        let expected_outputs = vec![
            digest_lit(random_digest0),
            digest_lit(random_digest1),
            digest_lit(random_digest2),
        ];
        let rust_ast = item_fn(parse_quote! {
            fn make_short_digest_list(digest1: Digest, digest0: Digest, digest2: Digest) -> (Digest, Digest, Digest) {
                let mut a: Vec<Digest> = Vec::<Digest>::with_capacity(17);
                a.push(digest1);
                a.push(digest0);
                a.push(digest2);
                // a = [d1, d0, d2]

                // Reorganize list
                let temp: Digest = a[0];
                a[0] = a[1];
                a[1] = temp;
                a[2] = a[2];
                // a = [d0, d1, d2]

                return (a[0], a[1], a[2]);
            }
        });
        compare_prop_with_stack_safe_lists(&rust_ast, inputs, expected_outputs);
    }

    #[test]
    fn simple_list_support_run_test() {
        use tasm_lib::rust_shadowing_helper_functions::list::list_new;
        use tasm_lib::rust_shadowing_helper_functions::list::list_pop;
        use tasm_lib::rust_shadowing_helper_functions::list::list_push;

        let rust_ast = item_fn(parse_quote! {
            fn make_short_list() -> (Vec<u64>, u32, u64, u64) {
                let mut a: Vec<u64> = Vec::<u64>::with_capacity(17);
                a.push(2000u64);
                a.push(3000u64);
                a.push(4000u64);
                let b: u64 = a.pop().unwrap();
                let len: u32 = a.len() as u32;

                a[1] = 5000u64;

                let d: u64 = a[0];

                return (a, len, b, d);
            }
        });

        let expected_list_pointer = dyn_malloc::DYN_MALLOC_FIRST_ADDRESS;
        let inputs = vec![];
        let expected_outputs = vec![
            bfe_lit(expected_list_pointer),
            u32_lit(2),
            u64_lit(4000),
            u64_lit(2000),
        ];

        let mut expected_final_memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();

        list_new(expected_list_pointer, &mut expected_final_memory);

        let elem_0 = vec![BFieldElement::new(2000), BFieldElement::new(0)];
        list_push(
            expected_list_pointer,
            elem_0.clone(),
            &mut expected_final_memory,
            elem_0.len(),
        );

        let elem_1 = vec![BFieldElement::new(5000), BFieldElement::new(0)];
        list_push(
            expected_list_pointer,
            elem_1.clone(),
            &mut expected_final_memory,
            elem_1.len(),
        );

        let elem_2 = vec![BFieldElement::new(4000), BFieldElement::new(0)];
        list_push(
            expected_list_pointer,
            elem_2.clone(),
            &mut expected_final_memory,
            elem_2.len(),
        );

        list_pop(
            expected_list_pointer,
            &mut expected_final_memory,
            ast_types::DataType::U64.stack_size(),
        );

        compare_prop_with_stack_safe_lists(&rust_ast, inputs, expected_outputs);
    }

    #[test]
    fn simple_list_support_test() {
        use tasm_lib::rust_shadowing_helper_functions::list::list_new;
        use tasm_lib::rust_shadowing_helper_functions::list::list_push;

        let mut memory = HashMap::default();
        let list_pointer = BFieldElement::one();
        list_new(list_pointer, &mut memory);

        let elem_1 = vec![BFieldElement::new(2000), BFieldElement::new(0)];
        list_push(list_pointer, elem_1.clone(), &mut memory, elem_1.len());

        let mut expected_final_memory = memory.clone();
        for i in 0..10 {
            let elem_i = vec![BFieldElement::new(i), BFieldElement::new(0)];
            list_push(
                list_pointer,
                elem_i.clone(),
                &mut expected_final_memory,
                elem_i.len(),
            );
        }

        let item_fn = &item_fn(parse_quote! {
            fn foo(values: &mut Vec<u64>) {
                let mut i: u64 = 0u64;
                while i < 10u64 {
                    values.push(i);
                    i += 1u64;
                }

                return;
            }
        });

        let expected_final_memory = None;
        let std_in = vec![];
        let non_determinism = NonDeterminism::default().with_ram(memory);
        compare_prop_with_stack_and_ins_safe_lists(
            item_fn,
            vec![bfe_lit(list_pointer)],
            vec![],
            expected_final_memory,
            std_in,
            non_determinism,
        );
    }

    #[test]
    fn build_and_read_vector_in_while_loop_test() {
        let item_type = DataType::U32;
        let item_size = item_type.stack_size() as isize;

        let list_type = DataType::List(Box::new(item_type));
        let list_size = list_type.stack_size() as isize;
        let expected_stack_diff = list_size + list_size + item_size;

        let exec_result = execute_with_stack_and_ins_safe_lists(
            &build_and_read_u32_vector_in_while_loop_rast(),
            vec![],
            vec![],
            NonDeterminism::new(vec![]),
            expected_stack_diff,
        )
        .unwrap();

        // Verify that expected lists were built
        let mut final_stack = exec_result.final_stack.clone();
        let b_10 = final_stack.pop().unwrap();
        assert_eq!(405, b_10.value());

        let list_pointer_b = final_stack.pop().unwrap();
        let expected_list_b = (400..416).map(u32_lit).rev().collect_vec();
        assert_list_equal(expected_list_b, list_pointer_b, &exec_result.final_ram);

        let list_pointer_a = final_stack.pop().unwrap();
        let expected_list_a = (0..16).map(u32_lit).collect_vec();
        assert_list_equal(expected_list_a, list_pointer_a, &exec_result.final_ram);

        fn build_and_read_u32_vector_in_while_loop_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn manage_vector() -> (Vec<u32>, Vec<u32>, u32) {
                    let mut list_a: Vec<u32> = Vec::<u32>::with_capacity(16);

                    let mut i: usize = 0;
                    while i < 16usize {
                        list_a.push(i);
                        i = i + 1;
                    }

                    // Declare b vector
                    i = 0;
                    let mut list_b: Vec<u32> = Vec::<u32>::with_capacity(32);
                    while i < 16 {
                        list_b.push(0u32);
                        i += 1;
                    }

                    // Construct b vector
                    i = 0;
                    while i < 16 {
                        list_b[i] = list_a[15 - i] + 400;
                        i += 1;
                    }

                    return (list_a, list_b, list_b[10]);
                }
            })
        }
    }

    #[test]
    fn build_vector_in_while_loop_test() {
        let mut exec_result = execute_with_stack_and_ins_safe_lists(
            &item_fn(parse_quote! {
                fn build_vector_in_while_loop() -> Vec<u32> {
                    let mut a: Vec<u32> = Vec::<u32>::with_capacity(16);

                    let mut i: usize = 0;
                    while i < 16usize {
                        a.push(i);
                        i = i + 1;
                    }

                    return a;
                }
            }),
            vec![],
            vec![],
            NonDeterminism::new(vec![]),
            DataType::List(Box::new(DataType::U32)).stack_size() as isize,
        )
        .unwrap();
        let mut list_pointer = exec_result.final_stack.last().unwrap();
        let mut expected_list = (0..16).map(u32_lit).collect_vec();
        assert_list_equal(expected_list, *list_pointer, &exec_result.final_ram);

        exec_result = execute_with_stack_and_ins_safe_lists(
            &item_fn(parse_quote! {
                fn build_vector_in_while_loop() -> Vec<u64> {
                    let mut a: Vec<u64> = Vec::<u64>::with_capacity(16);

                    let mut i: usize = 0;
                    while i < 16usize {
                        a.push(i as u64);
                        i = i + 1;
                    }

                    return a;
                }
            }),
            vec![],
            vec![],
            NonDeterminism::new(vec![]),
            DataType::List(Box::new(DataType::U64)).stack_size() as isize,
        )
        .unwrap();
        list_pointer = exec_result.final_stack.last().unwrap();
        expected_list = (0..16).map(u64_lit).collect_vec();
        assert_list_equal(expected_list, *list_pointer, &exec_result.final_ram)
    }

    #[test]
    fn polymorphic_vectors_test() {
        use tasm_lib::rust_shadowing_helper_functions::list::list_new;
        use tasm_lib::rust_shadowing_helper_functions::list::list_push;

        let rust_ast = item_fn(parse_quote! {
            fn polymorphic_vectors() -> (Vec<u32>, u32, Vec<u64>, u32) {
                let mut a: Vec<u32> = Vec::<u32>::with_capacity(16);
                let mut b: Vec<u64> = Vec::<u64>::with_capacity(16);
                a.push(1000u32);
                a.push(2000u32);
                b.push(3000u64);
                let a_len: u32 = a.len() as u32;
                let b_len: u32 = b.len() as u32;

                return (a, a_len, b, b_len);
            }
        });

        let inputs = vec![];
        let expected_list_pointer_a = dyn_malloc::DYN_MALLOC_FIRST_ADDRESS;
        let expected_list_pointer_b = expected_list_pointer_a + BFieldElement::new(16 + 2);
        let expected_outputs = vec![
            bfe_lit(expected_list_pointer_a),
            bfe_lit(BFieldElement::new(2)),
            bfe_lit(expected_list_pointer_b),
            bfe_lit(BFieldElement::new(1)),
        ];

        let mut expected_output_memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
        list_new(expected_list_pointer_a, &mut expected_output_memory);
        list_new(expected_list_pointer_b, &mut expected_output_memory);

        let elem_1 = vec![BFieldElement::new(1000)];
        list_push(
            expected_list_pointer_a,
            elem_1.clone(),
            &mut expected_output_memory,
            elem_1.len(),
        );

        let elem_2 = vec![BFieldElement::new(2000)];
        list_push(
            expected_list_pointer_a,
            elem_2.clone(),
            &mut expected_output_memory,
            elem_2.len(),
        );

        let elem_3 = vec![BFieldElement::new(3000), BFieldElement::new(0)];
        list_push(
            expected_list_pointer_b,
            elem_3.clone(),
            &mut expected_output_memory,
            elem_3.len(),
        );

        compare_prop_with_stack_safe_lists(&rust_ast, inputs, expected_outputs);
    }
}
