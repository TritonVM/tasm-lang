use crate::ast::{self};
use itertools::Itertools;

fn graft_fn_arg(rust_fn_arg: &syn::FnArg) -> ast::FnArg {
    match rust_fn_arg {
        syn::FnArg::Typed(pat_type) => {
            let name = match pat_type.pat.as_ref() {
                syn::Pat::Ident(ident) => ident.ident.to_string(),
                _ => panic!("unsupported"),
            };
            let data_type = match pat_type.ty.as_ref() {
                syn::Type::Path(type_path) => {
                    assert_eq!(
                        1,
                        type_path.path.segments.len(),
                        "Length other than one not supported"
                    );
                    type_path.path.segments[0].ident.to_string()
                }
                _ => panic!("unsupported"),
            };

            ast::FnArg {
                name,
                data_type: data_type.into(),
            }
        }
        syn::FnArg::Receiver(_) => panic!("Unsupported"),
    }
}

fn graft_return_type(rust_return_type: &syn::ReturnType) -> Vec<ast::DataType> {
    let ret_type_name = match rust_return_type {
        syn::ReturnType::Type(_, path) => match path.as_ref() {
            syn::Type::Path(type_path) => {
                assert_eq!(
                    1,
                    type_path.path.segments.len(),
                    "Length other than one not supported"
                );
                type_path.path.segments[0].ident.to_string()
            }
            _ => panic!("unsupported"),
        },
        syn::ReturnType::Default => panic!("unsupported"),
    };

    vec![ret_type_name.into()]
}

pub fn graft(input: &syn::ItemFn) -> ast::Fn {
    let function_name = input.sig.ident.to_string();
    let fn_arguments = input.sig.inputs.iter().map(graft_fn_arg).collect_vec();
    let output_values = graft_return_type(&input.sig.output);

    let ret = ast::Fn {
        name: function_name,
        args: fn_arguments,
        body: vec![], // TODO: Implement!
        output: output_values,
    };

    ret
}

#[cfg(test)]
mod tests {
    use syn::parse_quote;

    use super::*;

    #[test]
    fn test_graft() {
        let tokens: syn::Item = parse_quote! {
            fn add_u32(lhs: u32, rhs: u32) -> u32 {
                return lhs + rhs;
            }
        };
        match &tokens {
            syn::Item::Fn(item_fn) => {
                let ret = graft(item_fn);
                println!("{:#?}", ret);
            }
            _ => panic!("unsupported"),
        }
    }
}
