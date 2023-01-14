use crate::ast::{self};
use itertools::Itertools;

fn rust_type_path_to_data_type(rust_type_path: &syn::TypePath) -> ast::DataType {
    assert_eq!(
        1,
        rust_type_path.path.segments.len(),
        "Length other than one not supported"
    );
    rust_type_path.path.segments[0].ident.to_string().into()
}

fn rust_type_to_data_type(x: &syn::Type) -> ast::DataType {
    match x {
        syn::Type::Path(data_type) => rust_type_path_to_data_type(&data_type),
        other_type => panic!("{other_type:#?}"),
    }
}

fn graft_fn_arg(rust_fn_arg: &syn::FnArg) -> ast::FnArg {
    match rust_fn_arg {
        syn::FnArg::Typed(pat_type) => {
            let name = match pat_type.pat.as_ref() {
                syn::Pat::Ident(ident) => ident.ident.to_string(),
                other => panic!("unsupported: {other:?}"),
            };
            let data_type: ast::DataType = match pat_type.ty.as_ref() {
                syn::Type::Path(type_path) => rust_type_path_to_data_type(type_path),
                other => panic!("unsupported: {other:?}"),
            };

            ast::FnArg {
                name,
                data_type: data_type,
            }
        }
        other => panic!("unsupported: {other:?}"),
    }
}

fn graft_return_type(rust_return_type: &syn::ReturnType) -> Vec<ast::DataType> {
    let ret_type = match rust_return_type {
        syn::ReturnType::Type(_, path) => match path.as_ref() {
            syn::Type::Path(type_path) => vec![rust_type_path_to_data_type(type_path)],
            syn::Type::Tuple(tuple_type) => {
                let tuple_type = tuple_type;
                println!("{tuple_type:#?}");
                let elements = tuple_type
                    .elems
                    .iter()
                    .map(|x| rust_type_to_data_type(x))
                    .collect_vec();

                elements
            }
            _ => panic!("unsupported: {path:?}"),
        },
        other => panic!("unsupported: {other:?}"),
    };

    ret_type
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
    fn u32_add() {
        let tokens: syn::Item = parse_quote! {
            fn add_u32(lhs: u32, rhs: u32) -> u32 {
                let c = lhs + rhs;
                return c;
            }
        };
        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn u32_swap() {
        let tokens: syn::Item = parse_quote! {
            fn swap_u32(lhs: u32, rhs: u32) -> (u32, u32) {
                return (rhs, lhs);
            }
        };
        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }
}
