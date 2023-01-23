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
    pub annotations: Vec<Variable>,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct BasicBlock {
    pub index: usize,
    pub params: Vec<Variable>,
    pub statements: Vec<Statement>,
}

impl BasicBlock {
    /// Free variables are variables that are used in this basic
    /// block but not defined here, or used prior to being defined.
    pub fn free_variables(&self) -> Vec<Variable> {
        let mut free_variables = vec![];
        let mut free_variable_names = vec![];
        let mut defined_variable_names = vec![];
        for statement in self.statements.iter() {
            let expression = match &statement {
                Statement::Let(assignment) => &assignment.expr,
                Statement::Re(assignment) => &assignment.expr,
                Statement::Cond(expr) => expr,
            };
            match expression {
                Expr::Var(var) => {
                    if !free_variable_names.contains(&var.name)
                        && !defined_variable_names.contains(&var.name)
                    {
                        free_variables.push(var.clone());
                        free_variable_names.push(var.name.clone());
                    }
                }
                Expr::Lit(_) => {}
            }

            if let Statement::Let(assignment) = statement {
                defined_variable_names.push(assignment.var.name.clone());
            }
        }
        free_variables
    }

    /// Used variables are variables referenced in expressions in
    /// this basic block.
    pub fn used_variables(&self) -> Vec<Variable> {
        let mut used_variables = vec![];
        let mut used_variable_names = vec![];
        for statement in self.statements.iter() {
            let expression = match statement {
                Statement::Let(assignment) => &assignment.expr,
                Statement::Re(assignment) => &assignment.expr,
                Statement::Cond(expr) => expr,
            };
            match expression {
                Expr::Var(var) => {
                    if !used_variable_names.contains(&var.name) {
                        used_variables.push(var.clone());
                        used_variable_names.push(var.name.clone());
                    }
                }
                Expr::Lit(_) => {}
            }
        }
        used_variables
    }

    /// Defined variables are variables that are cast into existence
    /// by a let-statement in this basic block.
    pub fn defined_variables(&self) -> Vec<Variable> {
        let mut defined_variables = vec![];
        let mut defined_variable_names = vec![];
        for statement in self.statements.iter() {
            if let Statement::Let(assignment) = statement {
                if !defined_variable_names.contains(&assignment.var.name) {
                    defined_variables.push(assignment.var.clone());
                    defined_variable_names.push(assignment.var.name.clone());
                }
            }
        }
        defined_variables
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Variable {
    pub name: String,
    pub data_type: DataType,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Statement {
    Let(Assignment),
    Re(Assignment),
    Cond(Expr),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Assignment {
    pub var: Variable,
    pub expr: Expr,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Expr {
    Var(Variable),
    Lit(ExprLit),
}
