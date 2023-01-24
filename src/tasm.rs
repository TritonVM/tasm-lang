use itertools::Itertools;

use crate::ast;

#[derive(Debug, Default)]
pub struct CompilerState {
    pub stack: Vec<(String, ast::DataType)>,
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
    function: &ast::Fn<ast::Typing>,
    state: &mut CompilerState,
) -> String {
    match stmt {
        ast::Stmt::Let(ast::LetStmt {
            var_name,
            data_type,
            expr,
        }) => {
            todo!()
        }
        ast::Stmt::Assign(_) => todo!(),
        ast::Stmt::Return(_) => todo!(),
        ast::Stmt::FnCall(_) => todo!(),
        ast::Stmt::While(_) => todo!(),
        ast::Stmt::If(_) => todo!(),
    }
}
