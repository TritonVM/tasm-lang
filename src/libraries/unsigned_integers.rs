use tasm_lib::snippet::Snippet;

use crate::{ast, tasm_code_generator::CompilerState};

/// Map list-function or method name to the TASM lib snippet type
fn name_to_tasm_lib_snippet(
    public_name: &str,
    receiver_type: &ast::DataType,
) -> Option<Box<dyn Snippet>> {
    match public_name {
        "leading_zeros" => match receiver_type {
            ast::DataType::U32 => Some(Box::new(
                tasm_lib::arithmetic::u32::leading_zeros_u32::LeadingZerosU32,
            )),
            ast::DataType::U64 => Some(Box::new(
                tasm_lib::arithmetic::u64::leading_zeros_u64::LeadingZerosU64,
            )),
            _ => panic!("Dont know `leading_zeros` for {receiver_type}"),
        },
        "count_ones" => match receiver_type {
            ast::DataType::U64 => Some(Box::new(
                tasm_lib::arithmetic::u64::popcount_u64::PopCountU64,
            )),
            _ => panic!("Dont know `count_ones` for {receiver_type}"),
        },
        _ => None,
    }
}

pub fn get_function_name(_name: &str) -> Option<&str> {
    None
}

pub fn get_method_name(name: &str) -> Option<&str> {
    if matches!(name, "leading_zeros" | "count_ones") {
        Some(name)
    } else {
        None
    }
}

pub fn import_tasm_snippet(
    vector_fn_name: &str,
    receiver_type: &ast::DataType,
    state: &mut CompilerState,
) -> String {
    let snippet = name_to_tasm_lib_snippet(vector_fn_name, receiver_type)
        .unwrap_or_else(|| panic!("Unknown function name {vector_fn_name}"));
    let entrypoint = snippet.entrypoint();
    state.import_snippet(snippet);

    entrypoint
}

pub fn function_name_to_signature(
    fn_name: &str,
    receiver_type: &ast::DataType,
) -> ast::FnSignature {
    let snippet = name_to_tasm_lib_snippet(fn_name, receiver_type)
        .unwrap_or_else(|| panic!("Unknown function name {fn_name}"));

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
        0 => ast::DataType::Tuple(vec![]),
        _ => ast::DataType::Tuple(output_types),
    };

    ast::FnSignature { name, args, output }
}

pub fn method_name_to_signature(fn_name: &str, receiver_type: &ast::DataType) -> ast::FnSignature {
    function_name_to_signature(fn_name, receiver_type)
}
