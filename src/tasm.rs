use itertools::Itertools;

use crate::ast;

pub fn compile(function: &ast::Fn<ast::Typing>) -> String {
    let fn_name = &function.name;
    let fn_stack_input_sig = function.args.iter().map(|arg| format!("({arg})")).join(" ");
    let fn_stack_output_sig = format!("{}", function.output);

    format!(
        "
        // before: _ {fn_stack_input_sig}
        // after: _ {fn_stack_output_sig}
        {fn_name}:
            return
        "
    )
}

#[cfg(test)]
mod tests {
    use syn::parse_quote;

    use crate::graft::graft;

    use super::*;

    fn _swap_digests_raw() -> syn::Item {
        parse_quote! {
            fn swap_digests(lhs: Digest, rhs: Digest) -> (Digest, Digest) {
                return (rhs, lhs);
            }
        }
    }

    fn _swap_digests_fn() -> ast::Fn<ast::Typing> {
        match _swap_digests_raw() {
            syn::Item::Fn(item_fn) => graft(&item_fn),
            _ => panic!("unsupported"),
        }
    }

    fn eq_digest_raw() -> syn::Item {
        parse_quote! {
            fn eq_digest(lhs: Digest, rhs: Digest) -> bool {
                return
                    lhs[0u32] == rhs[0u32] &&
                    lhs[1u32] == rhs[1u32] &&
                    lhs[2u32] == rhs[2u32] &&
                    lhs[3u32] == rhs[3u32] &&
                    lhs[4u32] == rhs[4u32];

            }
        }
    }

    fn eq_digest_fn() -> ast::Fn<ast::Typing> {
        match eq_digest_raw() {
            syn::Item::Fn(item_fn) => graft(&item_fn),
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn compile_test() {
        let function = eq_digest_fn();
        let tasm_code = compile(&function);

        println!("{function:#?}");
        println!("{tasm_code}");
    }
}
