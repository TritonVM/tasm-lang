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

    mod compile_tests {
        use itertools::Itertools;

        use super::*;
        use crate::tests_and_benchmarks::shared_test::graft_check_compile_prop;

        #[test]
        fn simple_map_mul_by_2_test() {
            fn local_function_verify(input: u64) -> u64 {
                return input * 2;
            }
            let values = vec![];
            let _return_values: Vec<u64> =
                values.into_iter().map(local_function_verify).collect_vec();
            graft_check_compile_prop(&simple_map_mul_by_2());
        }
    }
}
