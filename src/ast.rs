use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::x_field_element::XFieldElement;

pub struct Fn {
    name: String,
    args: Vec<FnArg>,
    body: Vec<Stmt>,
    output: Vec<DataType>,
}

pub struct FnArg {
    name: String,
    data_type: DataType,
}

pub enum Stmt {
    Let(LetStmt),
    Return(Expr),
    // FIXME: Type-check that functions not bound to variables don't return anything
    FnCall(FnCall),
    // TODO: Control-flow operators: if-else, while, etc.
}

pub enum ExprLit {
    CBool(bool),
    CU32(u32),
    CU64(u64),
    BFE(BFieldElement),
    XFE(XFieldElement),
    Digest(Digest),
}

pub enum Expr {
    Lit(ExprLit),
    Var(String),
    FlatList(Box<Expr>),
    FnCall(FnCall),
    // TODO: Overloaded arithmetic operators
    // TODO: VM-specific intrinsics (hash, absorb, squeeze, etc.)
}

pub struct SymTable(HashMap<String, (u8, DataType)>);

pub enum DataType {
    Bool,
    U32,
    U64,
    BFE,
    XFE,
    Digest,
}

pub struct LetStmt {
    pub var_name: String,
    pub data_type: DataType,
    pub expr: Expr,
}

pub struct FnCall {
    pub name: String,
    pub args: Vec<Expr>, // FIXME: type-check that this is flat
}
