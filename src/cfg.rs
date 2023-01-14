use crate::ast::{DataType, ExprLit};

#[derive(Debug)]
pub struct ControlFlowGraph {
    pub entrypoint: usize,
    pub edges: Vec<Edge>,
    pub nodes: Vec<BasicBlock>,
    pub exitpoint: usize,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Edge {
    pub source: usize,
    pub destination: usize,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct BasicBlock {
    pub index: usize,
    pub stmts: Vec<LetStmt>,
    pub param: Option<Variable>,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Variable {
    pub name: String,
    pub data_type: DataType,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct LetStmt {
    pub var: Variable,
    pub expr: Option<Expr>, // empty only for first statement
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Expr {
    Var(String),
    Lit(ExprLit),
}
