use syn::parse_quote;

use crate::graft::item_fn;

#[allow(dead_code)]
pub fn left_child_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn left_child(node_index: u64, height: u32) -> u64 {
            return node_index - (1 << height);
        }
    })
}

#[allow(dead_code)]
pub fn right_child_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn right_child(node_index: u64) -> u64 {
            return node_index - 1;
        }
    })
}

#[allow(dead_code)]
pub fn leftmost_ancestor_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn leftmost_ancestor(node_index: u64) -> (u64, u32) {
            let h: u32 = tasm::tasm_arithmetic_u64_log_2_floor(node_index);
            let ret: u64 = (1 << (h + 1)) - 1;

            return (ret, h);
    }
    })
}

#[allow(dead_code)]
pub fn right_lineage_length_stmt_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        pub fn right_lineage_length(node_index: u64) -> u32 {
            let bit_width: u32 = tasm::tasm_arithmetic_u64_log_2_floor(node_index) + 1u32;
            let npo2: u64 = 1u64 << bit_width;
            let dist: u64 = npo2 - node_index;

            let mut ret: u32 = 0u32;
            if (bit_width as u64) < dist {
                ret = right_lineage_length(node_index - (npo2 / 2u64) + 1u64);
            } else {
                ret = (dist - 1u64) as u32;
            }

            return ret;
        }
    })
}

#[allow(dead_code)]
pub fn right_lineage_length_expr_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn right_lineage_length(node_index: u64) -> u32 {
            let bit_width: u32 = tasm::tasm_arithmetic_u64_log_2_floor(node_index) + 1u32;
            let npo2: u64 = 1u64 << bit_width;
            let dist: u64 = npo2 - node_index;

            // let bit_width_u64: u64 = bit_width.into();
            let bit_width_u64: u64 = bit_width as u64;
            let ret: u32 = if bit_width_u64 < dist {
                right_lineage_length(node_index - (npo2 / 2u64) + 1u64)
            } else {
                (dist - 1u64) as u32
            };

            return ret;
        }
    })
}
