use std::collections::HashMap;
use std::fmt::Display;
use triton_vm::Digest;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::ast_types::{AbstractArgument, DataType};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Fn<T> {
    pub fn_signature: FnSignature,
    pub body: Vec<Stmt<T>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FnSignature {
    pub name: String,
    pub args: Vec<AbstractArgument>,
    pub output: DataType,
    pub arg_evaluation_order: ArgEvaluationOrder,
}

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub enum ArgEvaluationOrder {
    #[default]
    LeftToRight,
    RightToLeft,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Stmt<T> {
    Let(LetStmt<T>),
    Assign(AssignStmt<T>),
    Return(Option<Expr<T>>),
    FnCall(FnCall<T>),
    MethodCall(MethodCall<T>),
    While(WhileStmt<T>),
    If(IfStmt<T>),
    Block(BlockStmt<T>),
    Assert(AssertStmt<T>),
    FnDeclaration(Fn<T>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AssertStmt<T> {
    pub expression: Expr<T>,
    // pub decription: Option<String>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct WhileStmt<T> {
    pub condition: Expr<T>,
    pub block: BlockStmt<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IfStmt<T> {
    pub condition: Expr<T>,
    pub then_branch: BlockStmt<T>,
    pub else_branch: BlockStmt<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BlockStmt<T> {
    pub stmts: Vec<Stmt<T>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ExprLit<T> {
    Bool(bool),
    U32(u32),
    U64(u64),
    BFE(BFieldElement),
    XFE(XFieldElement),
    Digest(Digest),
    MemPointer(MemPointerLiteral<T>),
    GenericNum(u128, T),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MemPointerLiteral<T> {
    pub mem_pointer_address: BFieldElement,
    pub struct_name: String,
    pub resolved_type: T,
}

impl<T> BFieldCodec for ExprLit<T> {
    fn decode(_sequence: &[BFieldElement]) -> anyhow::Result<Box<Self>> {
        todo!()
    }

    fn encode(&self) -> Vec<BFieldElement> {
        match self {
            ExprLit::Bool(value) => vec![BFieldElement::new(*value as u64)],
            ExprLit::U32(value) => value.encode(),
            ExprLit::U64(value) => vec![
                BFieldElement::new(value & 0xffff_ffff),
                BFieldElement::new(value >> 32),
            ],
            ExprLit::BFE(value) => value.encode(),
            ExprLit::XFE(value) => value.encode(),
            ExprLit::Digest(value) => value.encode(),
            ExprLit::GenericNum(_, _) => todo!(),
            ExprLit::MemPointer(_) => todo!(),
        }
    }

    fn static_length() -> Option<usize> {
        None
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum BinOp {
    Add,
    And,
    BitAnd,
    BitXor,
    BitOr,
    Div,
    Eq,
    Lt,
    Gt,
    Mul,
    Neq,
    Or,
    Rem,
    Shl,
    Shr,
    Sub,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    Deref,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr<T> {
    Lit(ExprLit<T>),
    Var(Identifier<T>),
    Tuple(Vec<Expr<T>>),
    FnCall(FnCall<T>),
    MethodCall(MethodCall<T>),
    Binop(Box<Expr<T>>, BinOp, Box<Expr<T>>, T),
    Unary(UnaryOp, Box<Expr<T>>, T),
    If(ExprIf<T>),
    Cast(Box<Expr<T>>, DataType),
    Field(Box<Expr<T>>, String, T),
    // Index(Box<Expr<T>>, Box<Expr<T>>), // a_expr[i_expr]    (a + 5)[3]
    // TODO: VM-specific intrinsics (hash, absorb, squeeze, etc.)
}

// Used for making nice value identifiers whose position you might be
// able to guess in the source code.
impl<T> Display for Expr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Expr::Lit(_lit) => "lit".to_owned(),
            Expr::Var(_) => "var_copy".to_owned(),
            Expr::Tuple(_) => "tuple".to_owned(),
            Expr::FnCall(_) => "fn_call".to_owned(),
            Expr::MethodCall(_) => "method_call.method_name".to_owned(),
            Expr::Binop(_, binop, _, _) => format!("binop_{binop:?}"),
            Expr::If(_) => "if_else".to_owned(),
            Expr::Cast(_, dt) => format!("cast_{dt}"),
            Expr::Unary(unaryop, _, _) => format!("unaryop_{unaryop:?}"),
            Expr::Field(expr, field, _) => format!("field expression:{expr}.{field}"),
        };

        write!(f, "{str}")
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ExprIf<T> {
    pub condition: Box<Expr<T>>,
    pub then_branch: Box<Expr<T>>,
    pub else_branch: Box<Expr<T>>,
}

pub struct SymTable(HashMap<String, (u8, DataType)>);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Identifier<T> {
    String(String, T),                           // x
    TupleIndex(Box<Identifier<T>>, usize),       // x.0
    ListIndex(Box<Identifier<T>>, Box<Expr<T>>), // x[0]
}

impl<T> Identifier<T> {
    pub fn binding_name(&self) -> String {
        match self {
            Identifier::String(name, _) => name.to_owned(),
            Identifier::TupleIndex(inner_id, _) => inner_id.binding_name(),
            Identifier::ListIndex(inner_id, _) => inner_id.binding_name(),
        }
    }
}

impl<T: core::fmt::Debug> Display for Identifier<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Identifier::String(name, _) => name.to_string(),
                Identifier::TupleIndex(ident, t_index) => format!("{ident}.{t_index}"),
                Identifier::ListIndex(ident, l_index_expr) => format!("{ident}[{l_index_expr:?}]"),
            }
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AssignStmt<T> {
    pub identifier: Identifier<T>,
    pub expr: Expr<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LetStmt<T> {
    pub var_name: String,
    pub mutable: bool,
    pub data_type: DataType,
    pub expr: Expr<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FnCall<T> {
    pub name: String,
    pub args: Vec<Expr<T>>,
    pub type_parameter: Option<DataType>,
    pub arg_evaluation_order: ArgEvaluationOrder,
    pub annot: T,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MethodCall<T> {
    pub method_name: String,
    pub args: Vec<Expr<T>>,
    pub annot: T,
}
