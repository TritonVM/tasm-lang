use twenty_first::shared_math::{
    b_field_element::BFieldElement,
    x_field_element::{XFieldElement, EXTENSION_DEGREE},
};

use crate::{ast, graft, types};

use super::bfe;

const XFIELDELEMENT_LIB_INDICATOR: &str = "XFieldElement::";

type Annotation = types::Typing;

pub fn graft_function(
    name: &str,
    args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
) -> Option<ast::Expr<Annotation>> {
    if name != "new" {
        return None;
    }

    if args.len() != 1 {
        panic!("XFE instantiation only takes one argument which must be an array");
    }

    match &args[0] {
        syn::Expr::Array(syn::ExprArray {
            attrs: _,
            bracket_token: _,
            elems,
        }) => {
            let mut initializer_exprs = vec![];
            for elem in elems {
                match elem {
                    syn::Expr::Call(syn::ExprCall {
                        attrs: _,
                        func,
                        paren_token: _,
                        args,
                    }) => {
                        let (name, _type_parameter) = match func.as_ref() {
                            syn::Expr::Path(path) => (
                                graft::path_to_ident(&path.path),
                                graft::path_to_type_parameter(&path.path),
                            ),
                            other => panic!("unsupported: {other:?}"),
                        };

                        if let Some(bfe_fn_name) = bfe::get_graft_function_name(&name) {
                            initializer_exprs.push(bfe::graft_function(bfe_fn_name, args).unwrap());
                        } else {
                            panic!();
                        }
                    }
                    _ => panic!("unsupported: {elem:?}"),
                }
            }

            let mut bfe_literals = vec![];
            for expr in initializer_exprs {
                match expr {
                    ast::Expr::Lit(ast::ExprLit::BFE(bfe)) => {
                        bfe_literals.push(bfe);
                    }
                    _ => unreachable!("BFE grafting must return BFE literals. Got: {:#?}", expr),
                }
            }

            let bfe_literals: [BFieldElement; EXTENSION_DEGREE] = bfe_literals.try_into().unwrap();
            Some(ast::Expr::Lit(ast::ExprLit::XFE(XFieldElement::new(
                bfe_literals,
            ))))
        }
        _ => panic!("XFE instantiation must happen with an array"),
    }
}

/// Return stripped function name iff grafting should be handled by
/// this library.
pub fn get_graft_function_name(name: &str) -> Option<&str> {
    if !name.starts_with(XFIELDELEMENT_LIB_INDICATOR) {
        return None;
    }

    let stripped_name = &name[XFIELDELEMENT_LIB_INDICATOR.len()..name.len()];

    if stripped_name != "new" {
        return None;
    }

    Some(stripped_name)
}
