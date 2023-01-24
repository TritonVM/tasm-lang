use std::collections::HashMap;

use itertools::Itertools;

use crate::ast;

#[derive(Debug, Default)]
pub struct CompilerState {
    pub counter: usize,
    pub stack: Vec<(Address, ast::DataType)>,
    pub var_addr: HashMap<String, Address>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Address {
    name: String,
}

impl CompilerState {
    pub fn new_address(&mut self, prefix: &str, data_type: &ast::DataType) -> Address {
        let name = format!("_{}_{}_{}", prefix, data_type, self.counter);
        let address = Address { name };
        self.stack.push((address.clone(), data_type.clone()));
        self.counter += 1;
        address
    }
}

pub fn compile(function: &ast::Fn<ast::Typing>) -> String {
    let fn_name = &function.name;
    let fn_stack_input_sig = function.args.iter().map(|arg| format!("({arg})")).join(" ");
    let fn_stack_output_sig = function
        .output
        .as_ref()
        .map(|data_type| format!("{}", data_type))
        .unwrap_or_default();

    let mut state = CompilerState::default();
    for stmt in function.body.iter() {
        compile_stmt(stmt, function, &mut state);
    }

    format!(
        "
        // before: _ {fn_stack_input_sig}
        // after: _ {fn_stack_output_sig}
        {fn_name}:
            return
        "
    )
}

fn compile_stmt(
    stmt: &ast::Stmt<ast::Typing>,
    _function: &ast::Fn<ast::Typing>,
    state: &mut CompilerState,
) -> String {
    use ast::Stmt::*;
    match stmt {
        Let(ast::LetStmt {
            var_name,
            data_type,
            expr,
        }) => {
            let expr_addr = compile_expr(expr, var_name, data_type, state);
            state.var_addr.insert(var_name.clone(), expr_addr);

            todo!()
        }
        Assign(_) => todo!(),
        Return(_) => todo!(),
        FnCall(_) => todo!(),
        While(_) => todo!(),
        If(_) => todo!(),
    }
}

fn compile_expr(
    _expr: &ast::Expr<ast::Typing>,
    _context: &str,
    _data_type: &ast::DataType,
    _state: &mut CompilerState,
) -> Address {
    todo!()
}
