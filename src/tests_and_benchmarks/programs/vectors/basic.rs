#[cfg(test)]
mod compile_and_typecheck_tests {
    use num::{One, Zero};
    use std::collections::HashMap;
    use syn::parse_quote;
    use triton_vm::BFieldElement;

    use crate::{graft::item_fn, tests_and_benchmarks::test_helpers::shared_test::*};

    fn simple_list_support() -> syn::ItemFn {
        item_fn(parse_quote! {
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
        })
    }

    fn mut_list_argument() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn foo(values: &mut Vec<u64>) {
                let mut i: u64 = 0u64;
                while i < 10u64 {
                    values.push(i);
                    i += 1u64;
                }

                return;
            }
        })
    }

    fn polymorphic_vectors_rast() -> syn::ItemFn {
        item_fn(parse_quote! {
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
        })
    }

    fn build_u32_vector_in_while_loop_rast() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn build_vector_in_while_loop() -> Vec<u32> {
                let mut a: Vec<u32> = Vec::<u32>::with_capacity(16);

                let mut i: usize = 0;
                while i < 16usize {
                    a.push(i);
                    i = i + 1;
                }

                return a;
            }
        })
    }

    fn build_u64_vector_in_while_loop_rast() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn build_vector_in_while_loop() -> Vec<u64> {
                let mut a: Vec<u64> = Vec::<u64>::with_capacity(16);

                let mut i: usize = 0;
                while i < 16usize {
                    a.push(i as u64);
                    i = i + 1;
                }

                return a;
            }
        })
    }

    pub mod compile_tests {
        use super::*;
        #[test]
        fn simple_list_support_test() {
            graft_check_compile_prop(&simple_list_support());
        }

        #[test]
        fn mut_list_argument_test() {
            graft_check_compile_prop(&mut_list_argument());
        }
    }

    pub mod run_tests {
        use itertools::Itertools;
        use triton_vm::NonDeterminism;

        use crate::ast_types::{self, DataType};

        use super::*;

        #[test]
        fn simple_list_support_run_test() {
            use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_new;
            use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_pop;
            use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_push;

            let inputs = vec![];
            let outputs = vec![
                bfe_lit(BFieldElement::one()),
                u32_lit(2),
                u64_lit(4000),
                u64_lit(2000),
            ];

            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();

            // Free-pointer
            // `2 * 17 + 2` is used by list. `1` is used by the dynamic allocator.
            let total_memory_used = 2 * 17 + 2 + 1;
            assert!(memory
                .insert(BFieldElement::zero(), BFieldElement::new(total_memory_used))
                .is_none());

            let list_pointer = BFieldElement::one();
            safe_list_new(list_pointer, 17, &mut memory);

            let elem_0 = vec![BFieldElement::new(2000), BFieldElement::new(0)];
            safe_list_push(list_pointer, elem_0.clone(), &mut memory, elem_0.len());

            let elem_1 = vec![BFieldElement::new(5000), BFieldElement::new(0)];
            safe_list_push(list_pointer, elem_1.clone(), &mut memory, elem_1.len());

            let elem_2 = vec![BFieldElement::new(4000), BFieldElement::new(0)];
            safe_list_push(list_pointer, elem_2.clone(), &mut memory, elem_2.len());

            safe_list_pop(
                list_pointer,
                &mut memory,
                ast_types::DataType::U64.stack_size(),
            );

            let input_memory = HashMap::default();
            compare_prop_with_stack_and_memory(
                &simple_list_support(),
                inputs,
                outputs,
                input_memory,
                Some(memory),
            );
        }

        #[test]
        fn simple_list_support_test() {
            use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_new;
            use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_push;

            let mut memory = HashMap::default();
            let list_pointer = BFieldElement::one();
            safe_list_new(list_pointer, 43, &mut memory);

            let elem_1 = vec![BFieldElement::new(2000), BFieldElement::new(0)];
            safe_list_push(list_pointer, elem_1.clone(), &mut memory, elem_1.len());

            let mut expected_final_memory = memory.clone();
            for i in 0..10 {
                let elem_i = vec![BFieldElement::new(i), BFieldElement::new(0)];
                safe_list_push(
                    list_pointer,
                    elem_i.clone(),
                    &mut expected_final_memory,
                    elem_i.len(),
                );
            }

            compare_prop_with_stack_and_memory(
                &mut_list_argument(),
                vec![bfe_lit(list_pointer)],
                vec![],
                memory,
                Some(expected_final_memory),
            );
        }

        #[test]
        fn build_vector_in_while_loop_test() {
            let mut vm_memory = HashMap::default();
            let mut exec_result = execute_with_stack_memory_and_ins(
                &build_u32_vector_in_while_loop_rast(),
                vec![],
                &mut vm_memory,
                vec![],
                NonDeterminism::new(vec![]),
                DataType::List(Box::new(DataType::U32)).stack_size() as isize,
            )
            .unwrap();
            let mut list_pointer = exec_result.final_stack.last().unwrap();
            let mut expected_list = (0..16).map(u32_lit).collect_vec();
            assert_list_equal(expected_list, *list_pointer, &vm_memory);

            vm_memory = HashMap::default();
            exec_result = execute_with_stack_memory_and_ins(
                &build_u64_vector_in_while_loop_rast(),
                vec![],
                &mut vm_memory,
                vec![],
                NonDeterminism::new(vec![]),
                DataType::List(Box::new(DataType::U64)).stack_size() as isize,
            )
            .unwrap();
            list_pointer = exec_result.final_stack.last().unwrap();
            expected_list = (0..16).map(u64_lit).collect_vec();
            assert_list_equal(expected_list, *list_pointer, &vm_memory)
        }

        #[test]
        fn polymorphic_vectors_test() {
            use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_new;
            use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_push;

            let inputs = vec![];
            let outputs = vec![
                bfe_lit(BFieldElement::new(1)),
                bfe_lit(BFieldElement::new(2)),
                bfe_lit(BFieldElement::new(16 + 2 + 1)),
                bfe_lit(BFieldElement::new(1)),
            ];

            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();

            // Dynamic allocator
            let space_used = 1 + 16 + 2 + 16 * 2 + 2;
            assert!(memory
                .insert(BFieldElement::zero(), BFieldElement::new(space_used))
                .is_none());

            let list_pointer_a = BFieldElement::one();
            safe_list_new(list_pointer_a, 16, &mut memory);

            let list_pointer_b = BFieldElement::new(16 + 2 + 1);
            safe_list_new(list_pointer_b, 16, &mut memory);

            let elem_1 = vec![BFieldElement::new(1000)];
            safe_list_push(list_pointer_a, elem_1.clone(), &mut memory, elem_1.len());

            let elem_2 = vec![BFieldElement::new(2000)];
            safe_list_push(list_pointer_a, elem_2.clone(), &mut memory, elem_2.len());

            let elem_3 = vec![BFieldElement::new(3000), BFieldElement::new(0)];
            safe_list_push(list_pointer_b, elem_3.clone(), &mut memory, elem_3.len());

            let input_memory = HashMap::default();
            compare_prop_with_stack_and_memory(
                &polymorphic_vectors_rast(),
                inputs,
                outputs,
                input_memory,
                Some(memory),
            );
        }
    }
}
