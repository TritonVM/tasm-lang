use std::collections::HashMap;
use std::str::FromStr;

use anyhow::bail;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::x_field_element::XFieldElement;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Fn {
    pub name: String,
    pub args: Vec<FnArg>,
    pub body: Vec<Stmt>,
    pub output: Vec<DataType>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FnArg {
    pub name: String,
    pub data_type: DataType,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Stmt {
    Let(LetStmt),
    Return(Expr),
    // FIXME: Type-check that functions not bound to variables don't return anything
    FnCall(FnCall),
    // TODO: Control-flow operators: if-else, while, etc.
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ExprLit {
    CBool(bool),
    CU32(u32),
    CU64(u64),
    BFE(BFieldElement),
    XFE(XFieldElement),
    Digest(Digest),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum BinOperator {
    Add,
    BitAnd,
    BitXor,
    // TODO: Add more
}

impl From<syn::BinOp> for BinOperator {
    fn from(rust_binop: syn::BinOp) -> Self {
        match rust_binop {
            syn::BinOp::Add(_) => BinOperator::Add,
            syn::BinOp::BitAnd(_) => BinOperator::BitAnd,
            syn::BinOp::BitXor(_) => BinOperator::BitXor,
            other => panic!("unsupported: {other:?}"),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr {
    Lit(ExprLit),
    Var(String),
    FlatList(Vec<Expr>),
    FnCall(FnCall),
    Binop(Box<Expr>, BinOperator, Box<Expr>),
    // TODO: Overloaded arithmetic operators
    // TODO: VM-specific intrinsics (hash, absorb, squeeze, etc.)
}

pub struct SymTable(HashMap<String, (u8, DataType)>);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum DataType {
    Bool,
    U32,
    U64,
    BFE,
    XFE,
    Digest,
}

impl FromStr for DataType {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bool" => Ok(DataType::Bool),
            "u32" => Ok(DataType::U32),
            "u64" => Ok(DataType::U64),
            "BFieldElement" => Ok(DataType::BFE),
            "XFieldElement" => Ok(DataType::XFE),
            "Digest" => Ok(DataType::Digest),
            ty => bail!("Unsupported type {}", ty),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LetStmt {
    pub var_name: String,
    pub data_type: DataType,
    pub expr: Expr,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FnCall {
    pub name: String,
    pub args: Vec<Expr>, // FIXME: type-check that this is flat
}
