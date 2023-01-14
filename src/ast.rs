use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::x_field_element::XFieldElement;

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: String,
    pub args: Vec<FnArg>,
    pub body: Vec<Stmt>,
    pub output: Vec<DataType>,
}

#[derive(Debug, Clone)]
pub struct FnArg {
    pub name: String,
    pub data_type: DataType,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let(LetStmt),
    Return(Expr),
    // FIXME: Type-check that functions not bound to variables don't return anything
    FnCall(FnCall),
    // TODO: Control-flow operators: if-else, while, etc.
}

#[derive(Debug, Clone)]
pub enum ExprLit {
    CBool(bool),
    CU32(u32),
    CU64(u64),
    BFE(BFieldElement),
    XFE(XFieldElement),
    Digest(Digest),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Lit(ExprLit),
    Var(String),
    FlatList(Box<Expr>),
    FnCall(FnCall),
    // TODO: Overloaded arithmetic operators
    // TODO: VM-specific intrinsics (hash, absorb, squeeze, etc.)
}

pub struct SymTable(HashMap<String, (u8, DataType)>);

#[derive(Debug, Clone)]
pub enum DataType {
    Bool,
    U32,
    U64,
    BFE,
    XFE,
    Digest,
}

impl From<String> for DataType {
    fn from(str: String) -> Self {
        match str.as_str() {
            "bool" => DataType::Bool,
            "Digest" => DataType::Digest,
            "u32" => DataType::U32,
            "u64" => DataType::U64,
            "BFieldElement" => DataType::BFE,
            "XFieldElement" => DataType::XFE,
            str => panic!("Unsupported type: {}", str),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub var_name: String,
    pub data_type: DataType,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct FnCall {
    pub name: String,
    pub args: Vec<Expr>, // FIXME: type-check that this is flat
}
