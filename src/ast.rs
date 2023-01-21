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

impl From<&syn::Lit> for ExprLit {
    fn from(rust_val: &syn::Lit) -> Self {
        const MIN_INT_LITERAL_LENGTH: usize = 4;
        match rust_val {
            syn::Lit::Bool(b) => Self::CBool(b.value),
            syn::Lit::Int(int_lit) => {
                // integer literals are expected to be read as e.g. `4u32` or `332u64`.
                // So the type aanotation is required.
                let int_lit: String = int_lit.token().to_string();
                let str_len = int_lit.len();
                if str_len < MIN_INT_LITERAL_LENGTH {
                    panic!(
                        "Error in declaration of int literal. Did you forget a type annotation? Got: \"{int_lit}\""
                    );
                }
                let int_lit_value = &int_lit.as_str()[0..str_len - 3];
                let type_annotation = &int_lit[str_len - 3..];
                let int_val: ExprLit = match type_annotation {
                    "u32" => {
                        let my_int = int_lit_value.parse::<u32>().unwrap();
                        ExprLit::CU32(my_int)
                    }
                    "u64" => {
                        let my_int = int_lit_value.parse::<u64>().unwrap();
                        ExprLit::CU64(my_int)
                    }
                    other => panic!("unsupported int type annotation: {other:?}"),
                };

                int_val
            }
            other => panic!("unsupported: {other:?}"),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum BinOperator {
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

impl From<syn::BinOp> for BinOperator {
    fn from(rust_binop: syn::BinOp) -> Self {
        match rust_binop {
            syn::BinOp::Add(_) => BinOperator::Add,
            syn::BinOp::And(_) => BinOperator::And,
            syn::BinOp::BitAnd(_) => BinOperator::BitAnd,
            syn::BinOp::BitXor(_) => BinOperator::BitXor,
            syn::BinOp::Div(_) => BinOperator::Div,
            syn::BinOp::Eq(_) => BinOperator::Eq,
            syn::BinOp::Lt(_) => BinOperator::Lt,
            syn::BinOp::Mul(_) => BinOperator::Mul,
            syn::BinOp::Ne(_) => BinOperator::Ne,
            syn::BinOp::Or(_) => BinOperator::Or,
            syn::BinOp::Rem(_) => BinOperator::Rem,
            syn::BinOp::Shl(_) => BinOperator::Shl,
            syn::BinOp::Shr(_) => BinOperator::Shr,
            syn::BinOp::Sub(_) => BinOperator::Sub,
            other => panic!("unsupported: {other:?}"),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr {
    Lit(ExprLit),
    Var(Identifier),
    Index(Box<Expr>, Box<Expr>),
    FlatList(Vec<Expr>),
    FnCall(FnCall),
    Binop(Box<Expr>, BinOperator, Box<Expr>),
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
