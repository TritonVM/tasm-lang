use crate::ast;

pub fn expr_lit_type(expr_lit: &ast::ExprLit) -> ast::DataType {
    match expr_lit {
        ast::ExprLit::Bool(_) => ast::DataType::Bool,
        ast::ExprLit::U32(_) => ast::DataType::U32,
        ast::ExprLit::U64(_) => ast::DataType::U64,
        ast::ExprLit::BFE(_) => ast::DataType::BFE,
        ast::ExprLit::XFE(_) => ast::DataType::XFE,
        ast::ExprLit::Digest(_) => ast::DataType::Digest,
    }
}
