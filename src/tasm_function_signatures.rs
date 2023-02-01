use tasm_lib::all_snippets::name_to_snippet;

use crate::ast::{self, FnSignature};

pub fn function_name_to_signature(
    tasm_fn_name: &str,
    element_type: Option<ast::DataType>,
) -> ast::FnSignature {
    let tasm_type: Option<tasm_lib::snippet::DataType> =
        element_type.map(|x| x.try_into().unwrap());
    let snippet = name_to_snippet(tasm_fn_name, tasm_type);

    let input_types_lib = snippet.input_types();
    let output_types_lib = snippet.output_types();
    let name = snippet.entrypoint().to_string();
    let mut args: Vec<ast::FnArg> = vec![];
    for (i, itl) in input_types_lib.into_iter().enumerate() {
        let fn_arg = ast::FnArg {
            name: format!("input_{i}"),
            data_type: itl.into(),
        };
        args.push(fn_arg);
    }

    let mut output_types: Vec<ast::DataType> = vec![];
    for otl in output_types_lib {
        output_types.push(otl.into());
    }

    let output = match output_types.len() {
        1 => Some(output_types[0].clone()),
        0 => None,
        _ => Some(ast::DataType::FlatList(output_types)),
    };

    FnSignature { name, args, output }
}
