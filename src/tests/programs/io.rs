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

#[cfg(test)]
mod run_tests {
    use super::*;
    use crate::tests::shared_test::*;
    use std::{collections::HashMap, vec};
    use twenty_first::shared_math::b_field_element::BFieldElement;

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
}
