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
pub fn secretin_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn stdin() -> BFieldElement {
            let res: BFieldElement = tasm::tasm_io_read_secret_bfe();
            return res;
        }
    })
}

#[cfg(test)]
mod run_tests {

    use std::{collections::HashMap, vec};

    use twenty_first::shared_math::b_field_element::BFieldElement;

    use super::*;
    use crate::tests::shared_test::*;

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
}
