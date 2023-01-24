use itertools::Itertools;

use crate::ast;

pub fn compile(function: &ast::Fn<ast::Typing>) -> String {
    let fn_name = &function.name;
    let fn_stack_input_sig = function.args.iter().map(|arg| format!("({arg})")).join(" ");
    let fn_stack_output_sig = format!("{}", function.output);

    format!(
        "
        // before: _ {fn_stack_input_sig}
        // after: _ {fn_stack_output_sig}
        {fn_name}:
            return
        "
    )
}
