use crate::ast::{DataType, ExprLit};

#[derive(Debug, Default)]
pub struct ControlFlowGraph {
    pub entrypoint: usize,
    pub edges: Vec<Edge>,
    pub nodes: Vec<BasicBlock>,
    pub exitpoint: usize,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Edge {
    pub source: usize,
    pub destination: usize,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct BasicBlock {
    pub index: usize,
    pub param: Option<Variable>,
    pub stmts: Vec<LetStmt>,
    pub last: Expr,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Variable {
    pub name: String,
    pub data_type: DataType,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct LetStmt {
    pub var: Variable,
    pub expr: Expr,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Expr {
    Var(Variable),
    Lit(ExprLit),
}
