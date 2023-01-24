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

fn add_u32_overwrite_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_u32_overwrite(lhs: u32, rhs: u32) -> u32 {
            let mut c: u32 = lhs + rhs;
            c = c + rhs;
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
    use itertools::Itertools;

    use crate::tasm::compile;
    use crate::types::annotate_fn;

    use super::*;

    fn graft_check_compile_prop(item_fn: &syn::ItemFn) {
        // parse test
        let mut function = graft(item_fn);

        // type-check and annotate
        annotate_fn(&mut function);

        println!("{:#?}", function);

        // compile
        let tasm = compile(&function);
        let tasm_string: String = tasm.iter().map(|instr| instr.to_string()).join("\n");
        println!("{}", tasm_string);
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
    fn add_u32_overwrite_rast_test() {
        graft_check_compile_prop(&add_u32_overwrite_rast());
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