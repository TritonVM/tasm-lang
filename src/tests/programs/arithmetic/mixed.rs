use syn::parse_quote;

use crate::graft::item_fn;

#[allow(dead_code)]
pub fn powers_of_two_with_bit_shifting() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn powers_of_two_with_bit_shifting() -> u64 {
            let a: u64 = 1 << 40;
            let b: u64 = 1 << 60;
            let c: u32 = 1000;
            let d: u32 = 2000;
            let e: (u32, u64) = (2300u32, 4000u64);
            return a + b;
        }
    })
}

#[cfg(test)]
mod run_tests {
    use super::*;
    use crate::tests::shared_test::*;

    #[test]
    fn powers_of_two_with_bit_shifting_test() {
        compare_prop_with_stack(
            &powers_of_two_with_bit_shifting(),
            vec![],
            vec![u64_lit((1u64 << 40) + (1u64 << 60))],
        );
    }
}
