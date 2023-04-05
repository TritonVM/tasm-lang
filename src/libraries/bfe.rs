use itertools::Itertools;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{ast, graft::graft_expr, types};

const BFIELDELEMENT_LIB_INDICATOR: &str = "BFieldElement::";

type Annotation = types::Typing;

pub fn graft_function(
    name: &str,
    args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
) -> Option<ast::Expr<Annotation>> {
    if name != "new" {
        return None;
    }

    let args = args.iter().map(graft_expr).collect_vec();

    if args.len() == 1 && matches!(args[0], ast::Expr::Lit(_)) {
        if let ast::Expr::Lit(ast::ExprLit::U64(value)) = args[0] {
            Some(ast::Expr::Lit(ast::ExprLit::BFE(BFieldElement::new(value))))
        } else if let ast::Expr::Lit(ast::ExprLit::GenericNum(value, _)) = args[0] {
            Some(ast::Expr::Lit(ast::ExprLit::BFE(BFieldElement::new(value))))
        } else {
            panic!("Can only instantiate BFE with u64-literal. Please use casting for conversion to `BFieldElement`. Got: {:#?}", args[0]);
        }
    } else {
        panic!("Unknown initialization expression for BFE");
    }
}

/// Return stripped function name iff grafting should be handled by
/// this library.
pub fn get_graft_function_name(name: &str) -> Option<&str> {
    if !name.starts_with(BFIELDELEMENT_LIB_INDICATOR) {
        return None;
    }

    let stripped_name = &name[BFIELDELEMENT_LIB_INDICATOR.len()..name.len()];

    if stripped_name != "new" {
        return None;
    }

    Some(stripped_name)
}
