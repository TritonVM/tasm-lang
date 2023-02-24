use syn::parse_quote;

use crate::graft::item_fn;

#[allow(dead_code)]
pub fn add_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn add_u32_overwrite_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_u32_overwrite(lhs: u32, rhs: u32) -> u32 {
            let mut c: u32 = lhs + rhs;
            c = c + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn sub_u32_rast_1() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn sub_u32_rast_2() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u32(rhs: u32, lhs: u32) -> u32 {
            let c: u32 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn sub_u64_rast_1() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u64_test(lhs: u64, rhs: u64) -> u64 {
            let c: u64 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn sub_u64_rast_2() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u64_test(rhs: u64, lhs: u64) -> u64 {
            let c: u64 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn add_bfe_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_bfe(lhs: BFieldElement, rhs: BFieldElement) -> BFieldElement {
            let c: BFieldElement = lhs + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn add_xfe_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_xfe(lhs: XFieldElement, rhs: XFieldElement) -> XFieldElement {
            let c: XFieldElement = lhs + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn add_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        // using `add_u64` as function name here would create a name-clash
        // between a dependency and the function we are compiling.
        fn add_u64_test(lhs: u64, rhs: u64) -> u64 {
            let c: u64 = lhs + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn and_bool_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn boolean_and_bool(lhs: bool, rhs: bool) -> bool {
            let c: bool = lhs && rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn bitwise_and_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn bitwise_and_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs & rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn bitwise_and_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn bitwise_and_u64(lhs: u64, rhs: u64) -> u64 {
            let c: u64 = lhs & rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn simple_sub() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn simple_sub(a: u32, b: u32) -> u32 {
            return a - b;
        }
    })
}

#[allow(dead_code)]
pub fn operator_evaluation_ordering_with_div_u32() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_div() -> u32 {
            return 100u32 - 14u32 / 2u32 + 1u32;
        }
    })
}

#[allow(dead_code)]
pub fn operator_evaluation_ordering_with_div_u64() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_div() -> u64 {
            return 100u64 - 14u64 / 2u64 + 1u64;
        }
    })
}

#[allow(dead_code)]
pub fn operator_evaluation_ordering_with_mul() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_mul() -> u32 {
            return 380u32 - 14u32 * 2u32 * 10u32 + 1u32 - 41u32 * 1u32;
        }
    })
}

#[allow(dead_code)]
pub fn lt_u32() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn lt_for_u32_test_function() -> bool {
            let a: u32 = 14u32;
            let b: u32 = 20u32;
            let c: u32 = 10u32;
            let bool_0: bool = a < b;
            let bool_1: bool = b < c;
            let bool_2: bool = c < a;

            return bool_1 || bool_2;
        }
    })
}
