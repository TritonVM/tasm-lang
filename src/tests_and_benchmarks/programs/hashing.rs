#[cfg(test)]
mod run_tests {
    use std::collections::HashMap;
    use std::vec;

    use itertools::Itertools;
    use num::One;
    use num::Zero;
    use rand::random;
    use syn::parse_quote;
    use tasm_lib::rust_shadowing_helper_functions;
    use triton_vm::twenty_first::shared_math::other::random_elements;
    use triton_vm::twenty_first::shared_math::tip5::Tip5;
    use triton_vm::twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
    use triton_vm::BFieldElement;
    use triton_vm::Digest;
    use triton_vm::NonDeterminism;

    use crate::graft::item_fn;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    type H = Tip5;

    #[test]
    fn default_digest_test() {
        fn default_digest_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                // Calling this `hash_pair` would result in a label collision
                fn default_digest_testf() -> Digest {
                    let a: Digest = Digest::default();
                    return a;
                }
            })
        }

        compare_prop_with_stack_safe_lists(
            &default_digest_rast(),
            vec![],
            vec![digest_lit(Digest::default())],
        );
    }

    #[test]
    fn hash_pair_test() {
        fn hash_pair_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                // Calling this `hash_pair` would result in a label collision
                fn hash_pair_test(left: Digest, right: Digest) -> Digest {
                    let ret: Digest = H::hash_pair(left, right);
                    return ret;
                }
            })
        }

        let test_iterations = 20;
        let lhss: Vec<Digest> = random_elements(test_iterations);
        let rhss: Vec<Digest> = random_elements(test_iterations);
        let test_cases = lhss
            .into_iter()
            .zip_eq(rhss)
            .map(|(left, right)| {
                InputOutputTestCase::new(
                    vec![digest_lit(left), digest_lit(right)],
                    vec![digest_lit(H::hash_pair(left, right))],
                )
            })
            .collect_vec();
        multiple_compare_prop_with_stack_safe_lists(&hash_pair_rast(), test_cases);
    }

    #[test]
    fn hash_pair_reference_test() {
        fn hash_pair_with_references_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                // Calling this `hash_pair` would result in a label collission
                fn hash_pair_test(left: Digest, right: Digest) -> Digest {
                    let ret: Digest = H::hash_pair(left, right);
                    return ret;
                }
            })
        }

        let left: Digest = random();
        let right: Digest = random();
        let test_case = InputOutputTestCase::new(
            vec![digest_lit(left), digest_lit(right)],
            vec![digest_lit(H::hash_pair(left, right))],
        );
        multiple_compare_prop_with_stack_safe_lists(
            &hash_pair_with_references_rast(),
            vec![test_case],
        );
    }

    fn hash_varlen_rast() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn hash_varlen_test_function(input: Vec<BFieldElement>) -> Digest {
                let ret: Digest = H::hash_varlen(&input);
                return ret;
            }
        })
    }

    #[test]
    fn hash_varlen_safe_list_test() {
        let random_bfes: Vec<BFieldElement> = random_elements(20);
        let mut init_memory = HashMap::default();
        rust_shadowing_helper_functions::safe_list::safe_list_insert(
            BFieldElement::one(),
            20,
            random_bfes.clone(),
            &mut init_memory,
        );
        init_memory.insert(BFieldElement::zero(), BFieldElement::new(23));
        let non_determinism = NonDeterminism::new(vec![]).with_ram(init_memory);
        let mut ram = non_determinism.ram.clone().into_iter().collect_vec();
        ram.sort_by_key(|x| x.0.value());

        compare_prop_with_stack_and_ins_safe_lists(
            &hash_varlen_rast(),
            vec![bfe_lit(BFieldElement::one())],
            vec![digest_lit(H::hash_varlen(&random_bfes))],
            None,
            vec![],
            non_determinism,
        );
    }

    #[test]
    fn hash_varlen_unsafe_list_test() {
        fn hash_varlen_rast() -> syn::ItemFn {
            item_fn(parse_quote! {
                fn hash_varlen_test_function(input: Vec<BFieldElement>) -> Digest {
                    let ret: Digest = H::hash_varlen(&input);
                    return ret;
                }
            })
        }

        let random_bfes: Vec<BFieldElement> = random_elements(20);
        let mut init_memory = HashMap::default();
        rust_shadowing_helper_functions::unsafe_list::unsafe_list_insert(
            BFieldElement::one(),
            random_bfes.clone(),
            &mut init_memory,
        );
        init_memory.insert(BFieldElement::zero(), BFieldElement::new(23));
        let non_determinism = NonDeterminism::new(vec![]).with_ram(init_memory);
        let mut ram = non_determinism.ram.clone().into_iter().collect_vec();
        ram.sort_by_key(|x| x.0.value());

        compare_prop_with_stack_and_ins_unsafe_lists(
            &hash_varlen_rast(),
            vec![bfe_lit(BFieldElement::one())],
            vec![digest_lit(H::hash_varlen(&random_bfes))],
            None,
            vec![],
            non_determinism,
        );
    }
}
