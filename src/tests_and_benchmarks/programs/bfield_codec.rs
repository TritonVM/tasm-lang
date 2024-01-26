#[cfg(test)]
mod run_tests {
    use std::vec;

    use itertools::Itertools;
    use rand::random;
    use syn::parse_quote;
    use triton_vm::twenty_first::shared_math::other::random_elements;
    use triton_vm::BFieldElement;
    use triton_vm::Digest;

    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn bfe_encode_test() {
        let rust_ast_to_compile = item_fn(parse_quote! {
            fn bfe_encode_test_fn(input: BFieldElement) -> BFieldElement {
                let a: Vec<BFieldElement> = input.encode();
                let b: Vec<BFieldElement> = input.encode();

                return a[0];
            }
        });

        let input_bfe: BFieldElement = BFieldElement::new(87);
        compare_prop_with_stack_safe_lists(
            &rust_ast_to_compile,
            vec![bfe_lit(input_bfe)],
            vec![bfe_lit(input_bfe)],
        );
    }

    fn digest_encode_rast() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn digest_encode_test_fn(input0: Digest, input1: Digest) -> (BFieldElement, BFieldElement, BFieldElement, BFieldElement, BFieldElement, BFieldElement, BFieldElement) {
                let a: Vec<BFieldElement> = input0.encode();
                let b: Vec<BFieldElement> = input1.encode();

                return (a[3], a[0], a[4], a[1], a[2], b[4], b[0]);
            }
        })
    }

    #[test]
    fn digest_encode_test() {
        let input_digest0: Digest = random();
        let input_digest1: Digest = random();
        let digest0_words = input_digest0.values();
        let digest1_words = input_digest1.values();
        compare_prop_with_stack_safe_lists(
            &digest_encode_rast(),
            vec![digest_lit(input_digest0), digest_lit(input_digest1)],
            vec![
                bfe_lit(digest0_words[3]),
                bfe_lit(digest0_words[0]),
                bfe_lit(digest0_words[4]),
                bfe_lit(digest0_words[1]),
                bfe_lit(digest0_words[2]),
                bfe_lit(digest1_words[4]),
                bfe_lit(digest1_words[0]),
            ],
        );

        let test_iterations = 3;
        let digests0: Vec<Digest> = random_elements(test_iterations);
        let digests1: Vec<Digest> = random_elements(test_iterations);
        let test_cases = digests0
            .into_iter()
            .zip_eq(digests1)
            .map(|(digest0, digest1)| {
                let digest0_words = digest0.values();
                let digest1_words = digest1.values();
                InputOutputTestCase::new(
                    vec![digest_lit(digest0), digest_lit(digest1)],
                    vec![
                        bfe_lit(digest0_words[3]),
                        bfe_lit(digest0_words[0]),
                        bfe_lit(digest0_words[4]),
                        bfe_lit(digest0_words[1]),
                        bfe_lit(digest0_words[2]),
                        bfe_lit(digest1_words[4]),
                        bfe_lit(digest1_words[0]),
                    ],
                )
            })
            .collect_vec();
        multiple_compare_prop_with_stack_safe_lists(&digest_encode_rast(), test_cases);
    }
}
