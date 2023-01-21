use std::collections::HashMap;
use std::fmt::Display;
use std::str::FromStr;

use anyhow::bail;
use itertools::Itertools;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::x_field_element::XFieldElement;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Fn {
    pub name: String,
    pub args: Vec<FnArg>,
    pub body: Vec<Stmt>,
    pub output: DataType,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FnArg {
    pub name: String,
    pub data_type: DataType,
}

impl Display for FnArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.data_type)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Stmt {
    Let(LetStmt),
    Assign(AssignStmt),
    Return(Expr),
    // FIXME: Type-check that functions not bound to variables don't return anything
    FnCall(FnCall),
    While(WhileStmt), // TODO: Control-flow operators: if-else, while, etc.
    If(IfStmt),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct WhileStmt {
    pub condition: Expr,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IfStmt {
    pub condition: Expr,
    pub if_branch: Vec<Stmt>,
    pub else_branch: Vec<Stmt>,
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
pub enum BinOp {
    Add,
    And,
    BitAnd,
    BitXor,
    Div,
    Eq,
    Lt,
    Mul,
    Ne,
    Or,
    Rem,
    Shl,
    Shr,
    Sub,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr {
    Lit(ExprLit),
    Var(Identifier),
    Index(Box<Expr>, Box<Expr>),
    FlatList(Vec<Expr>),
    FnCall(FnCall),
    Binop(Box<Expr>, BinOp, Box<Expr>),
    If(ExprIf),
    // TODO: Overloaded arithmetic operators
    // TODO: VM-specific intrinsics (hash, absorb, squeeze, etc.)
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ExprIf {
    pub condition: Box<Expr>,
    pub then_branch: Box<Expr>,
    pub else_branch: Box<Expr>,
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
    List(Box<DataType>),
    FlatList(Vec<DataType>),
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

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use DataType::*;
        write!(
            f,
            "{}",
            match self {
                Bool => "bool".to_string(),
                U32 => "u32".to_string(),
                U64 => "u64".to_string(),
                BFE => "BField".to_string(),
                XFE => "XField".to_string(),
                Digest => "Digest".to_string(),
                List(ty) => format!("List({})", ty),
                FlatList(tys) => tys.iter().map(|ty| format!("{}", ty)).join(" "),
            }
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Identifier {
    String(String),
    Tuple(Box<Identifier>, usize),
    ListIndex(Box<Identifier>, Box<Expr>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AssignStmt {
    pub identifier: Identifier,
    pub expr: Expr,
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
