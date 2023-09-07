#[cfg(test)]
mod tests {
    use syn::parse_quote;

    use crate::graft::item_fn;

    fn simple_map_mul_by_2() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn foo(values: Vec<u64>) -> Vec<u64> {
                fn local_function(input: u64) -> u64 {
                    return input * 2;
                }
                let return_values: Vec<u64> = values.into_iter().map(local_function).collect_vec();

                return return_values;
            }
        })
    }

    mod run_tests {
        use itertools::Itertools;
        use std::collections::HashMap;
        use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_insert;
        use triton_vm::BFieldElement;
        use twenty_first::shared_math::other::random_elements;

        use super::*;
        use crate::tests_and_benchmarks::test_helpers::shared_test::*;

        #[test]
        fn simple_map_mul_by_2_test() {
            let mut vm_memory = HashMap::default();
            let init_list_u32s: Vec<u32> = random_elements(1);
            let init_list_u64s: Vec<u64> =
                init_list_u32s.into_iter().map(|x| x as u64).collect_vec();
            let input_list_pointer = BFieldElement::new(10000);
            safe_list_insert(
                input_list_pointer,
                1,
                init_list_u64s.clone(),
                &mut vm_memory,
            );
            println!(
                "vm_memory: {}",
                vm_memory
                    .iter()
                    .sorted_by_key(|x| x.0.value())
                    .map(|(k, v)| format!("({k} => {v})"))
                    .join(",")
            );
            let exec_result = execute_with_stack_and_memory_safe_lists(
                &simple_map_mul_by_2(),
                vec![bfe_lit(input_list_pointer)],
                &mut vm_memory,
                0,
            )
            .unwrap();
            let list_pointer = exec_result.final_stack.last().unwrap();
            let expected_list = init_list_u64s
                .into_iter()
                .map(|x| u64_lit(x * 2))
                .collect_vec();
            assert_list_equal(expected_list, *list_pointer, &vm_memory);
        }
    }
}
