use std::collections::HashMap;

use itertools::Itertools;

use crate::ast;
use crate::ast_types;

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub(crate) enum Typing {
    /// An `UnknownType` has not been determined. The front-end produces only
    /// known types; this variant exists for AST nodes built without a type.
    #[default]
    UnknownType,

    /// A `KnownType` has been determined; this is performed by the type checker.
    KnownType(ast_types::DataType),
}

impl GetType for Typing {
    fn get_type(&self) -> ast_types::DataType {
        match self {
            Typing::UnknownType => panic!("Cannot unpack type before complete type annotation."),
            Typing::KnownType(data_type) => data_type.clone(),
        }
    }
}

pub(crate) trait GetType {
    fn get_type(&self) -> ast_types::DataType;
}

impl<T: GetType> GetType for ast::ExprLit<T> {
    fn get_type(&self) -> ast_types::DataType {
        match self {
            ast::ExprLit::Bool(_) => ast_types::DataType::Bool,
            ast::ExprLit::U32(_) => ast_types::DataType::U32,
            ast::ExprLit::U64(_) => ast_types::DataType::U64,
            ast::ExprLit::U128(_) => ast_types::DataType::U128,
            ast::ExprLit::Bfe(_) => ast_types::DataType::Bfe,
            ast::ExprLit::Xfe(_) => ast_types::DataType::Xfe,
            ast::ExprLit::Digest(_) => ast_types::DataType::Digest,
            ast::ExprLit::GenericNum(_, t) => t.get_type(),
        }
    }
}

impl<T: GetType + std::fmt::Debug> GetType for ast::Expr<T> {
    fn get_type(&self) -> ast_types::DataType {
        match self {
            ast::Expr::Lit(lit) => lit.get_type(),
            ast::Expr::Var(id) => id.get_type(),
            ast::Expr::Array(_, t) => t.get_type(),
            ast::Expr::Tuple(t_list) => ast_types::DataType::Tuple(
                t_list
                    .iter()
                    .map(|elem| elem.get_type())
                    .collect_vec()
                    .into(),
            ),
            ast::Expr::Struct(struct_expr) => struct_expr.get_type(),
            ast::Expr::EnumDeclaration(enum_decl) => enum_decl.get_type(),
            ast::Expr::FnCall(fn_call) => fn_call.get_type(),
            ast::Expr::MethodCall(method_call) => method_call.get_type(),
            ast::Expr::Binop(_, _, _, t) => t.get_type(),
            ast::Expr::If(if_expr) => if_expr.get_type(),
            ast::Expr::Cast(_expr, t) => t.to_owned(),
            ast::Expr::Unary(_, _, t) => t.get_type(),
            ast::Expr::ReturningBlock(ret_block) => ret_block.get_type(),
            ast::Expr::Match(match_expr) => match_expr.arms.first().unwrap().body.get_type(),
            ast::Expr::Panic(_, t) => t.get_type(),
            ast::Expr::MemoryLocation(ast::MemPointerExpression { resolved_type, .. }) => {
                resolved_type.get_type()
            }
        }
    }
}

impl<T: GetType + std::fmt::Debug> GetType for ast::ReturningBlock<T> {
    fn get_type(&self) -> ast_types::DataType {
        self.return_expr.get_type()
    }
}

impl GetType for ast::EnumDeclaration {
    fn get_type(&self) -> ast_types::DataType {
        self.enum_type.clone()
    }
}

impl<T: GetType + std::fmt::Debug> GetType for ast::StructExpr<T> {
    fn get_type(&self) -> ast_types::DataType {
        self.struct_type.clone()
    }
}

impl<T: GetType + std::fmt::Debug> GetType for ast::ExprIf<T> {
    fn get_type(&self) -> ast_types::DataType {
        self.then_branch.get_type()
    }
}

impl<T: GetType + std::fmt::Debug> GetType for ast::MatchExpr<T> {
    fn get_type(&self) -> ast_types::DataType {
        self.arms.first().unwrap().body.get_type()
    }
}

impl<T: GetType> GetType for ast::Identifier<T> {
    fn get_type(&self) -> ast_types::DataType {
        match self {
            ast::Identifier::String(_, t) => t.get_type(),
            ast::Identifier::Index(_, _, t) => t.get_type(),
            ast::Identifier::Field(_, _, t) => t.get_type(),
        }
    }
}

impl<T: GetType> GetType for ast::FnCall<T> {
    fn get_type(&self) -> ast_types::DataType {
        self.annot.get_type()
    }
}

impl<T: GetType> GetType for ast::MethodCall<T> {
    fn get_type(&self) -> ast_types::DataType {
        self.annot.get_type()
    }
}

/// What libraries need to know about the program in order to produce the
/// signature of one of their methods.
#[derive(Debug)]
pub(crate) struct CheckState {
    /// The `ftable` maps function names to their signature (argument and output) types.
    ///
    /// This is used for determining the type of function calls in expressions.
    pub(crate) ftable: HashMap<String, Vec<ast::FnSignature>>,
}

/// A type that is implemented in terms of `U32` values.
///
/// E.g. `U32` and `U64`.
pub(crate) fn is_u32_based_type(data_type: &ast_types::DataType) -> bool {
    use ast_types::DataType::*;
    matches!(data_type, U32 | U64 | U128)
}
