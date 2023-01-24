use syn::parse_quote;

use crate::graft::{graft, item_fn};

fn nop_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn nop() {
            return;
        }
    })
}

fn add_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs + rhs;
            return c;
        }
    })
}

fn right_child_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn right_child(node_index: u64) -> u64 {
            return node_index - 1u64;
        }
    })
}

fn left_child_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn left_child(node_index: u64, height: u32) -> u64 {
            return node_index - (1u64 << height);
        }
    })
}

#[cfg(test)]
mod tests {
    use crate::types::annotate_fn;

    use super::*;

    fn graft_check_compile_prop(item_fn: &syn::ItemFn) {
        // parse test
        let mut function = graft(item_fn);

        println!("{:#?}", function);

        // type-check
        annotate_fn(&mut function);

        // compile
        // let tasm = compile(&function);
        // println!("{}", tasm);
    }

    #[test]
    fn nop_test() {
        graft_check_compile_prop(&nop_rast());
    }

    #[test]
    fn add_u32_test() {
        graft_check_compile_prop(&add_u32_rast());
    }

    #[test]
    fn right_child_test() {
        graft_check_compile_prop(&right_child_rast());
    }

    #[test]
    fn left_child_test() {
        graft_check_compile_prop(&left_child_rast());
    }
}
