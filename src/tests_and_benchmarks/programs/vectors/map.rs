#[cfg(test)]
mod tests {
    use syn::parse_quote;

    use crate::tests_and_benchmarks::test_helpers::shared_test::item_fn;

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
        use std::collections::HashMap;

        use itertools::Itertools;
        use tasm_lib::rust_shadowing_helper_functions::list::list_insert;
        use tasm_lib::triton_vm::prelude::*;
        use tasm_lib::twenty_first::shared_math::other::random_elements;

        use crate::tests_and_benchmarks::test_helpers::shared_test::*;

        use super::*;

        #[test]
        fn simple_map_mul_by_2_test() {
            let mut init_memory = HashMap::default();
            let init_list_u32s: Vec<u32> = random_elements(1);
            let init_list_u64s: Vec<u64> =
                init_list_u32s.into_iter().map(|x| x as u64).collect_vec();
            let input_list_pointer = BFieldElement::new(10000);
            list_insert(input_list_pointer, init_list_u64s.clone(), &mut init_memory);

            let std_in = vec![];
            let non_determinism = NonDeterminism::default().with_ram(init_memory);
            let exec_result = execute_with_stack_and_ins_safe_lists(
                &simple_map_mul_by_2(),
                vec![bfe_lit(input_list_pointer)],
                std_in,
                non_determinism,
                0,
            )
            .unwrap();
            let list_pointer = exec_result.op_stack.stack.last().unwrap();
            let expected_list = init_list_u64s
                .into_iter()
                .map(|x| u64_lit(x * 2))
                .collect_vec();
            assert_list_equal(expected_list, *list_pointer, &exec_result.ram);
        }
    }
}
