use tasm_lib::all_snippets::name_to_snippet;

use crate::ast::{self, FnSignature};
use crate::tasm_code_generator::CompilerState;

const TASM_LIB_INDICATOR: &str = "tasm::";

pub fn get_function_name(name: &str) -> Option<&str> {
    if name.starts_with(TASM_LIB_INDICATOR) {
        let stripped_name = &name[TASM_LIB_INDICATOR.len()..name.len()];
        return Some(stripped_name);
    }

    None
}

/// tasm-lib contains no methods, only functions
pub fn get_method_name(_name: &str) -> Option<&str> {
    None
}

pub fn import_tasm_snippet(tasm_fn_name: &str, state: &mut CompilerState) -> String {
    let snippet = name_to_snippet(tasm_fn_name);
    let entrypoint = snippet.entrypoint();
    state.import_snippet(snippet);

    entrypoint
}

pub fn function_name_to_signature(tasm_fn_name: &str) -> ast::FnSignature {
    let snippet = name_to_snippet(tasm_fn_name);

    let input_types_lib = snippet.input_types();
    let output_types_lib = snippet.output_types();
    let name = snippet.entrypoint();
    let mut args: Vec<ast::FnArg> = vec![];
    for (i, itl) in input_types_lib.into_iter().enumerate() {
        let fn_arg = ast::FnArg {
            name: format!("input_{i}"),
            data_type: itl.into(),
            // The tasm snippet input arguments are all considered mutable
            mutable: true,
        };
        args.push(fn_arg);
    }

    let mut output_types: Vec<ast::DataType> = vec![];
    for otl in output_types_lib {
        output_types.push(otl.into());
    }

    let output = match output_types.len() {
        1 => output_types[0].clone(),
        0 => ast::DataType::FlatList(vec![]),
        _ => ast::DataType::FlatList(output_types),
    };

    FnSignature { name, args, output }
}
