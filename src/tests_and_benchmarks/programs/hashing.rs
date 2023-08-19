#[cfg(test)]
mod run_tests {
    use crate::graft::item_fn;
    use crate::tests_and_benchmarks::{
        ozk_programs,
        test_helpers::{io_native, ozk_parsing, shared_test::*},
    };
    use itertools::Itertools;
    use rand::random;
    use std::vec;
    use syn::parse_quote;
    use triton_vm::Digest;
    use twenty_first::{
        shared_math::{other::random_elements, tip5::Tip5},
        util_types::algebraic_hasher::AlgebraicHasher,
    };

    fn hash_pair_rast() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn hash_pair(left: Digest, right: Digest) -> Digest {
                let ret: Digest = H::hash_pair(left, right);
                return ret;
            }
        })
    }

    #[test]
    fn hash_pair_test() {
        type H = Tip5;

        let test_iterations = 20;
        let lhss: Vec<Digest> = random_elements(test_iterations);
        let rhss: Vec<Digest> = random_elements(test_iterations);
        let test_cases = lhss
            .into_iter()
            .zip_eq(rhss.into_iter())
            .map(|(left, right)| {
                InputOutputTestCase::new(
                    vec![digest_lit(left), digest_lit(right)],
                    vec![digest_lit(H::hash_pair(&left, &right))],
                )
            })
            .collect_vec();
        multiple_compare_prop_with_stack(&hash_pair_rast(), test_cases);
    }

    fn hash_pair_with_references_rast() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn hash_pair(left: Digest, right: Digest) -> Digest {
                let ret: Digest = H::hash_pair(&left, &right);
                return ret;
            }
        })
    }

    #[test]
    fn hash_pair_reference_test() {
        type H = Tip5;

        let left: Digest = random();
        let right: Digest = random();
        let test_case = InputOutputTestCase::new(
            vec![digest_lit(left), digest_lit(right)],
            vec![digest_lit(H::hash_pair(&left, &right))],
        );
        multiple_compare_prop_with_stack(&hash_pair_with_references_rast(), vec![test_case]);
    }
}
