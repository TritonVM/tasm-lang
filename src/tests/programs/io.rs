use syn::parse_quote;

use crate::graft::item_fn;

#[allow(dead_code)]
pub fn stdin_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn stdin() -> BFieldElement {
            let from_stdin: BFieldElement = tasm::tasm_io_read_stdin_bfe();
            return from_stdin;
        }
    })
}

#[allow(dead_code)]
pub fn stdin_rast_pair() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn stdin() -> (BFieldElement, BFieldElement) {
            let res1: BFieldElement = tasm::tasm_io_read_stdin_bfe();
            let res2: BFieldElement = tasm::tasm_io_read_stdin_bfe();
            return (res1, res2);
        }
    })
}

#[allow(dead_code)]
pub fn secretin_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn secretin() -> BFieldElement {
            let res: BFieldElement = tasm::tasm_io_read_secret_bfe();
            return res;
        }
    })
}

#[allow(dead_code)]
pub fn secretin_rast_10() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn secretin() -> (BFieldElement, BFieldElement, BFieldElement, BFieldElement, BFieldElement, BFieldElement, BFieldElement, BFieldElement) {
            let r0: BFieldElement = tasm::tasm_io_read_secret_bfe();
            let r1: BFieldElement = tasm::tasm_io_read_secret_bfe();
            let r2: BFieldElement = tasm::tasm_io_read_secret_bfe();
            let r3: BFieldElement = tasm::tasm_io_read_secret_bfe();
            let r4: BFieldElement = tasm::tasm_io_read_secret_bfe();
            let r5: BFieldElement = tasm::tasm_io_read_secret_bfe();
            let r6: BFieldElement = tasm::tasm_io_read_secret_bfe();
            let r7: BFieldElement = tasm::tasm_io_read_secret_bfe();
            return (r0,r1,r2,r3,r4,r5,r6,r7);
        }
    })
}

#[allow(dead_code)]
pub fn stdin_rast_most_types() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn mosttypes() -> (bool, u32, u64, BFieldElement, XFieldElement) {
            let s0: bool = tasm::tasm_io_read_stdin_bool();
            let s1: u32 = tasm::tasm_io_read_stdin_u32();
            let s2: u64 = tasm::tasm_io_read_stdin_u64();
            let s3: BFieldElement = tasm::tasm_io_read_stdin_bfe();
            let s4: XFieldElement = tasm::tasm_io_read_stdin_xfe();
            return (s0,s1,s2,s3,s4);
        }
    })
}

#[allow(dead_code)]
pub fn stdin_rast_digest() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn get_digest() -> Digest {
            let s: Digest = tasm::tasm_io_read_stdin_digest();
            return s;
        }
    })
}

#[allow(dead_code)]
pub fn secretin_rast_most_types() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn most_types() -> (bool, u32, u64, BFieldElement, XFieldElement) {
            let r0: bool = tasm::tasm_io_read_secret_bool();
            let r1: u32 = tasm::tasm_io_read_secret_u32();
            let r2: u64 = tasm::tasm_io_read_secret_u64();
            let r3: BFieldElement = tasm::tasm_io_read_secret_bfe();
            let r4: XFieldElement = tasm::tasm_io_read_secret_xfe();
            return (r0,r1,r2,r3,r4);
        }
    })
}

#[allow(dead_code)]
pub fn secretin_rast_digest() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn get_digest() -> Digest {
            let s: Digest = tasm::tasm_io_read_secret_digest();
            return s;
        }
    })
}

#[cfg(test)]
mod run_tests {
    use super::*;
    use crate::tests::shared_test::*;
    use std::{collections::HashMap, vec};
    use triton_vm::bfield_codec::BFieldCodec;
    use twenty_first::shared_math::{
        b_field_element::{BFieldElement, BFIELD_ONE, BFIELD_ZERO},
        rescue_prime_digest::Digest,
        x_field_element::XFieldElement,
    };

    fn bool_to_bfe(b: bool) -> BFieldElement {
        if b {
            BFIELD_ONE
        } else {
            BFIELD_ZERO
        }
    }

    fn split(value: u64) -> Vec<BFieldElement> {
        vec![((value >> 32) as u32).into(), (value as u32).into()]
    }

    #[test]
    fn stdin_test() {
        let my_bfe = BFieldElement::new(42);
        compare_prop_with_stack_and_memory_and_ins(
            &stdin_rast(),
            vec![],
            vec![bfe_lit(my_bfe)],
            HashMap::default(),
            HashMap::default(),
            vec![my_bfe],
            vec![],
        )
    }

    #[test]
    fn stdin_test_pair() {
        let my_bfe = BFieldElement::new(42);
        let my_bfe34 = BFieldElement::new(34);
        compare_prop_with_stack_and_memory_and_ins(
            &stdin_rast_pair(),
            vec![],
            vec![bfe_lit(my_bfe), bfe_lit(my_bfe34)],
            HashMap::default(),
            HashMap::default(),
            vec![my_bfe, my_bfe34],
            vec![],
        )
    }

    #[test]
    fn secretin_test() {
        let my_bfe = BFieldElement::new(42);
        compare_prop_with_stack_and_memory_and_ins(
            &secretin_rast(),
            vec![],
            vec![bfe_lit(my_bfe)],
            HashMap::default(),
            HashMap::default(),
            vec![],
            vec![my_bfe],
        )
    }

    #[test]
    fn secretin_10_test() {
        let my_bfe = BFieldElement::new(42);
        let my_bfe999 = BFieldElement::new(999);
        compare_prop_with_stack_and_memory_and_ins(
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
            HashMap::default(),
            vec![],
            vec![
                my_bfe, my_bfe, my_bfe, my_bfe, my_bfe, my_bfe, my_bfe, my_bfe999,
            ],
        )
    }

    #[test]
    fn stdin_most_types_test() {
        let my_bool = true;
        let my_u32 = 125241;
        let my_u64 = 12343214213427u64;
        let my_bfe = BFieldElement::new(42);
        let my_xfe = XFieldElement::new([my_bfe, my_bfe, my_bfe]);
        compare_prop_with_stack_and_memory_and_ins(
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
            HashMap::default(),
            vec![
                vec![bool_to_bfe(my_bool)],
                vec![my_u32.into()],
                split(my_u64),
                vec![my_bfe],
                my_xfe.encode(),
            ]
            .concat(),
            vec![],
        )
    }

    #[test]
    fn secretin_most_types_test() {
        let my_bool = true;
        let my_u32 = 125241;
        let my_u64 = 12343214213427u64;
        let my_bfe = BFieldElement::new(42);
        let my_xfe = XFieldElement::new([my_bfe, my_bfe, my_bfe]);
        compare_prop_with_stack_and_memory_and_ins(
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
            HashMap::default(),
            vec![],
            vec![
                vec![bool_to_bfe(my_bool)],
                vec![my_u32.into()],
                split(my_u64),
                vec![my_bfe],
                my_xfe.encode(),
            ]
            .concat(),
        )
    }

    #[test]
    fn stdin_digest_test() {
        let my_bfe = BFieldElement::new(42);
        let my_digest = Digest::new([my_bfe, my_bfe, my_bfe, my_bfe, my_bfe]);
        compare_prop_with_stack_and_memory_and_ins(
            &stdin_rast_digest(),
            vec![],
            vec![digest_lit(my_digest)],
            HashMap::default(),
            HashMap::default(),
            my_digest.encode(),
            vec![],
        )
    }

    #[test]
    fn secretin_digest_test() {
        let my_bfe = BFieldElement::new(42);
        let my_digest = Digest::new([my_bfe, my_bfe, my_bfe, my_bfe, my_bfe]);
        compare_prop_with_stack_and_memory_and_ins(
            &secretin_rast_digest(),
            vec![],
            vec![digest_lit(my_digest)],
            HashMap::default(),
            HashMap::default(),
            vec![],
            my_digest.encode(),
        )
    }
}
