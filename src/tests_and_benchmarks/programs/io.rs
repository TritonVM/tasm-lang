mod run_tests {
    use crate::graft::item_fn;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use rand::random;
    use std::{collections::HashMap, vec};
    use syn::parse_quote;
    use triton_vm::{Digest, NonDeterminism};
    use twenty_first::shared_math::{
        b_field_element::BFieldElement, bfield_codec::BFieldCodec, x_field_element::XFieldElement,
    };

    fn stdin_rast() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn stdin() -> BFieldElement {
                let from_stdin: BFieldElement = tasm::tasm_io_read_stdin___bfe();
                return from_stdin;
            }
        })
    }

    fn stdin_rast_pair() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn stdin() -> (BFieldElement, BFieldElement) {
                let res1: BFieldElement = tasm::tasm_io_read_stdin___bfe();
                let res2: BFieldElement = tasm::tasm_io_read_stdin___bfe();
                return (res1, res2);
            }
        })
    }

    fn secretin_rast() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn secretin() -> BFieldElement {
                let res: BFieldElement = tasm::tasm_io_read_secret___bfe();
                return res;
            }
        })
    }

    fn secretin_rast_10() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn secretin() -> (BFieldElement, BFieldElement, BFieldElement, BFieldElement, BFieldElement, BFieldElement, BFieldElement, BFieldElement) {
                let r0: BFieldElement = tasm::tasm_io_read_secret___bfe();
                let r1: BFieldElement = tasm::tasm_io_read_secret___bfe();
                let r2: BFieldElement = tasm::tasm_io_read_secret___bfe();
                let r3: BFieldElement = tasm::tasm_io_read_secret___bfe();
                let r4: BFieldElement = tasm::tasm_io_read_secret___bfe();
                let r5: BFieldElement = tasm::tasm_io_read_secret___bfe();
                let r6: BFieldElement = tasm::tasm_io_read_secret___bfe();
                let r7: BFieldElement = tasm::tasm_io_read_secret___bfe();
                return (r0,r1,r2,r3,r4,r5,r6,r7);
            }
        })
    }

    fn stdin_rast_most_types() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn mosttypes() -> (bool, u32, u64, BFieldElement, XFieldElement) {
                let s0: bool = tasm::tasm_io_read_stdin___bool();
                let s1: u32 = tasm::tasm_io_read_stdin___u32();
                let s2: u64 = tasm::tasm_io_read_stdin___u64();
                let s3: BFieldElement = tasm::tasm_io_read_stdin___bfe();
                let s4: XFieldElement = tasm::tasm_io_read_stdin___xfe();
                return (s0,s1,s2,s3,s4);
            }
        })
    }

    fn stdin_rast_all_types() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn all_types() -> (bool, u32, u64, BFieldElement, XFieldElement, Digest) {
                let s0: bool = tasm::tasm_io_read_stdin___bool();
                let s1: u32 = tasm::tasm_io_read_stdin___u32();
                let s2: u64 = tasm::tasm_io_read_stdin___u64();
                let s3: BFieldElement = tasm::tasm_io_read_stdin___bfe();
                let s4: XFieldElement = tasm::tasm_io_read_stdin___xfe();
                let s5: Digest = tasm::tasm_io_read_stdin___digest();
                return (s0,s1,s2,s3,s4, s5);
            }
        })
    }

    fn stdin_rast_all_types_one_liner() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn all_types_one_liner() -> (bool, u32, u64, BFieldElement, XFieldElement, Digest) {
                return (
                    tasm::tasm_io_read_stdin___bool(),
                    tasm::tasm_io_read_stdin___u32(),
                    tasm::tasm_io_read_stdin___u64(),
                    tasm::tasm_io_read_stdin___bfe(),
                    tasm::tasm_io_read_stdin___xfe(),
                    tasm::tasm_io_read_stdin___digest()
                );
            }
        })
    }

    fn secret_rast_all_types_one_liner() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn all_types_one_liner() -> (bool, u32, u64, BFieldElement, XFieldElement, Digest) {
                return (
                    tasm::tasm_io_read_secret___bool(),
                    tasm::tasm_io_read_secret___u32(),
                    tasm::tasm_io_read_secret___u64(),
                    tasm::tasm_io_read_secret___bfe(),
                    tasm::tasm_io_read_secret___xfe(),
                    tasm::tasm_io_read_secret___digest()
                );
            }
        })
    }

    fn stdin_rast_digest() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn get_digest() -> Digest {
                let s: Digest = tasm::tasm_io_read_stdin___digest();
                return s;
            }
        })
    }

    fn secretin_rast_most_types() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn most_types() -> (bool, u32, u64, BFieldElement, XFieldElement) {
                let r0: bool = tasm::tasm_io_read_secret___bool();
                let r1: u32 = tasm::tasm_io_read_secret___u32();
                let r2: u64 = tasm::tasm_io_read_secret___u64();
                let r3: BFieldElement = tasm::tasm_io_read_secret___bfe();
                let r4: XFieldElement = tasm::tasm_io_read_secret___xfe();
                return (r0,r1,r2,r3,r4);
            }
        })
    }

    fn secretin_rast_digest() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn get_digest() -> Digest {
                let s: Digest = tasm::tasm_io_read_secret___digest();
                return s;
            }
        })
    }

    #[test]
    fn stdin_test() {
        let my_bfe = BFieldElement::new(42);
        compare_prop_with_stack_and_memory_and_ins_safe_lists(
            &stdin_rast(),
            vec![],
            vec![bfe_lit(my_bfe)],
            HashMap::default(),
            Some(HashMap::default()),
            vec![my_bfe],
            NonDeterminism::new(vec![]),
        )
    }

    #[test]
    fn stdin_test_pair() {
        let my_bfe = BFieldElement::new(42);
        let my_bfe34 = BFieldElement::new(34);
        compare_prop_with_stack_and_memory_and_ins_safe_lists(
            &stdin_rast_pair(),
            vec![],
            vec![bfe_lit(my_bfe), bfe_lit(my_bfe34)],
            HashMap::default(),
            Some(HashMap::default()),
            vec![my_bfe, my_bfe34],
            NonDeterminism::new(vec![]),
        )
    }

    #[test]
    fn secretin_test() {
        let my_bfe = BFieldElement::new(42);
        compare_prop_with_stack_and_memory_and_ins_safe_lists(
            &secretin_rast(),
            vec![],
            vec![bfe_lit(my_bfe)],
            HashMap::default(),
            Some(HashMap::default()),
            vec![],
            NonDeterminism::new(vec![my_bfe]),
        )
    }

    #[test]
    fn secretin_10_test() {
        let my_bfe = BFieldElement::new(42);
        let my_bfe999 = BFieldElement::new(999);
        compare_prop_with_stack_and_memory_and_ins_safe_lists(
            &secretin_rast_10(),
            vec![],
            vec![
                bfe_lit(my_bfe),
                bfe_lit(my_bfe),
                bfe_lit(my_bfe),
                bfe_lit(my_bfe),
                bfe_lit(my_bfe),
                bfe_lit(my_bfe),
                bfe_lit(my_bfe),
                bfe_lit(my_bfe999),
            ],
            HashMap::default(),
            None,
            vec![],
            NonDeterminism::new(vec![
                my_bfe, my_bfe, my_bfe, my_bfe, my_bfe, my_bfe, my_bfe, my_bfe999,
            ]),
        )
    }

    #[test]
    fn stdin_all_types_test() {
        let my_bool = true;
        let my_u32 = 125241;
        let my_u64 = 123456789012345u64;
        let my_bfe = BFieldElement::new(42);
        let my_xfe = XFieldElement::new([
            BFieldElement::new(1045),
            BFieldElement::new(1047),
            BFieldElement::new(1049),
        ]);
        let digest: Digest = random();
        let mut reversed_xfe = my_xfe.encode();
        reversed_xfe.reverse();
        let mut reversed_digest = digest.encode();
        reversed_digest.reverse();
        compare_prop_with_stack_and_memory_and_ins_safe_lists(
            &stdin_rast_all_types(),
            vec![],
            vec![
                bool_lit(my_bool),
                u32_lit(my_u32),
                u64_lit(my_u64),
                bfe_lit(my_bfe),
                xfe_lit(my_xfe),
                digest_lit(digest),
            ],
            HashMap::default(),
            None,
            [
                vec![bool_to_bfe(my_bool)],
                vec![my_u32.into()],
                split(my_u64),
                vec![my_bfe],
                reversed_xfe,
                reversed_digest,
            ]
            .concat(),
            NonDeterminism::new(vec![]),
        )
    }

    #[test]
    fn stdin_all_types_one_liner_test() {
        let my_bool = true;
        let my_u32 = 125241;
        let my_u64 = 123456789012345u64;
        let my_bfe = BFieldElement::new(42);
        let my_xfe = XFieldElement::new([
            BFieldElement::new(1045),
            BFieldElement::new(1047),
            BFieldElement::new(1049),
        ]);
        let digest: Digest = random();
        let mut reversed_xfe = my_xfe.encode();
        reversed_xfe.reverse();
        let mut reversed_digest = digest.encode();
        reversed_digest.reverse();
        compare_prop_with_stack_and_memory_and_ins_safe_lists(
            &stdin_rast_all_types_one_liner(),
            vec![],
            vec![
                bool_lit(my_bool),
                u32_lit(my_u32),
                u64_lit(my_u64),
                bfe_lit(my_bfe),
                xfe_lit(my_xfe),
                digest_lit(digest),
            ],
            HashMap::default(),
            Some(HashMap::default()),
            [
                vec![bool_to_bfe(my_bool)],
                vec![my_u32.into()],
                split(my_u64),
                vec![my_bfe],
                reversed_xfe,
                reversed_digest,
            ]
            .concat(),
            NonDeterminism::new(vec![]),
        )
    }

    #[test]
    fn secret_all_types_one_liner_test() {
        let my_bool = true;
        let my_u32 = 125241;
        let my_u64 = 123456789012345u64;
        let my_bfe = BFieldElement::new(42);
        let my_xfe = XFieldElement::new([
            BFieldElement::new(1045),
            BFieldElement::new(1047),
            BFieldElement::new(1049),
        ]);
        let digest: Digest = random();
        let mut reversed_xfe = my_xfe.encode();
        reversed_xfe.reverse();
        let mut reversed_digest = digest.encode();
        reversed_digest.reverse();
        compare_prop_with_stack_and_memory_and_ins_safe_lists(
            &secret_rast_all_types_one_liner(),
            vec![],
            vec![
                bool_lit(my_bool),
                u32_lit(my_u32),
                u64_lit(my_u64),
                bfe_lit(my_bfe),
                xfe_lit(my_xfe),
                digest_lit(digest),
            ],
            HashMap::default(),
            Some(HashMap::default()),
            vec![],
            NonDeterminism::new(
                [
                    vec![bool_to_bfe(my_bool)],
                    vec![my_u32.into()],
                    split(my_u64),
                    vec![my_bfe],
                    reversed_xfe,
                    reversed_digest,
                ]
                .concat(),
            ),
        )
    }

    #[test]
    fn stdin_most_types_test() {
        let my_bool = true;
        let my_u32 = 125241;
        let my_u64 = 12343214213427u64;
        let my_bfe = BFieldElement::new(42);
        let my_xfe = XFieldElement::new([my_bfe, my_bfe, my_bfe]);
        compare_prop_with_stack_and_memory_and_ins_safe_lists(
            &stdin_rast_most_types(),
            vec![],
            vec![
                bool_lit(my_bool),
                u32_lit(my_u32),
                u64_lit(my_u64),
                bfe_lit(my_bfe),
                xfe_lit(my_xfe),
            ],
            HashMap::default(),
            None,
            [
                vec![bool_to_bfe(my_bool)],
                vec![my_u32.into()],
                split(my_u64),
                vec![my_bfe],
                my_xfe.encode(),
            ]
            .concat(),
            NonDeterminism::new(vec![]),
        )
    }

    #[test]
    fn secretin_most_types_test() {
        let my_bool = true;
        let my_u32 = 125241;
        let my_u64 = 12343214213427u64;
        let my_bfe = BFieldElement::new(42);
        let my_xfe = XFieldElement::new([my_bfe, my_bfe, my_bfe]);
        compare_prop_with_stack_and_memory_and_ins_safe_lists(
            &secretin_rast_most_types(),
            vec![],
            vec![
                bool_lit(my_bool),
                u32_lit(my_u32),
                u64_lit(my_u64),
                bfe_lit(my_bfe),
                xfe_lit(my_xfe),
            ],
            HashMap::default(),
            None,
            vec![],
            NonDeterminism::new(
                [
                    vec![bool_to_bfe(my_bool)],
                    vec![my_u32.into()],
                    split(my_u64),
                    vec![my_bfe],
                    my_xfe.encode(),
                ]
                .concat(),
            ),
        )
    }

    #[test]
    fn stdin_digest_test() {
        let my_bfe = BFieldElement::new(42);
        let my_digest = Digest::new([my_bfe, my_bfe, my_bfe, my_bfe, my_bfe]);
        compare_prop_with_stack_and_memory_and_ins_safe_lists(
            &stdin_rast_digest(),
            vec![],
            vec![digest_lit(my_digest)],
            HashMap::default(),
            Some(HashMap::default()),
            my_digest.encode(),
            NonDeterminism::new(vec![]),
        )
    }

    #[test]
    fn secretin_digest_test() {
        let my_bfe = BFieldElement::new(42);
        let my_digest = Digest::new([my_bfe, my_bfe, my_bfe, my_bfe, my_bfe]);
        compare_prop_with_stack_and_memory_and_ins_safe_lists(
            &secretin_rast_digest(),
            vec![],
            vec![digest_lit(my_digest)],
            HashMap::default(),
            Some(HashMap::default()),
            vec![],
            NonDeterminism::new(my_digest.encode()),
        )
    }
}
