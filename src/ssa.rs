pub struct Block {
    stmts: Vec<Assign>,
}

pub struct Assign {
    var: String,
    expr: Expr,
}

pub enum Expr {
    Var(String),
    Phi(Box<Block>),
}
