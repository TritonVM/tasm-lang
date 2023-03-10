use tasm_lib::snippet::Snippet;

use crate::{ast, tasm_code_generator::CompilerState};

const VECTOR_LIB_INDICATOR: &str = "Vec::";

/// Map list-function or method name to the TASM lib snippet type
fn name_to_tasm_lib_snippet(
    public_name: &str,
    type_parameter: &Option<ast::DataType>,
) -> Option<Box<dyn Snippet>> {
    let tasm_type: Option<tasm_lib::snippet::DataType> =
        type_parameter.clone().map(|x| x.try_into().unwrap());
    match public_name {
        // TODO: Replace with "Vec::with_capacity" and use Safe list implementation
        "Vec::default" => Some(Box::new(tasm_lib::list::unsafe_u32::new::UnsafeNew(
            tasm_type.unwrap(),
        ))),
        "default" => Some(Box::new(tasm_lib::list::unsafe_u32::new::UnsafeNew(
            tasm_type.unwrap(),
        ))),
        "push" => Some(Box::new(tasm_lib::list::unsafe_u32::push::UnsafePush(
            tasm_type.unwrap(),
        ))),
        "pop" => Some(Box::new(tasm_lib::list::unsafe_u32::pop::UnsafePop(
            tasm_type.unwrap(),
        ))),
        "len" => Some(Box::new(
            tasm_lib::list::unsafe_u32::length::UnsafeLengthLong(tasm_type.unwrap()),
        )),
        _ => None,
    }
}

pub fn get_function_name(name: &str) -> Option<&str> {
    if name.starts_with(VECTOR_LIB_INDICATOR) {
        let stripped_name = &name[VECTOR_LIB_INDICATOR.len()..name.len()];
        return Some(stripped_name);
    }

    None
}

pub fn get_method_name(name: &str) -> Option<&str> {
    if matches!(name, "push" | "pop" | "len") {
        Some(name)
    } else {
        None
    }
}

pub fn import_tasm_snippet(
    vector_fn_name: &str,
    type_parameter: &Option<ast::DataType>,
    state: &mut CompilerState,
) -> String {
    let snippet = name_to_tasm_lib_snippet(vector_fn_name, type_parameter)
        .unwrap_or_else(|| panic!("Unknown function name {vector_fn_name}"));
    let entrypoint = snippet.entrypoint();
    state.import_snippet(snippet);

    entrypoint
}

pub fn function_name_to_signature(
    fn_name: &str,
    element_type: &Option<ast::DataType>,
) -> ast::FnSignature {
    let snippet = name_to_tasm_lib_snippet(fn_name, element_type)
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

pub fn method_name_to_signature(
    fn_name: &str,
    element_type: &Option<ast::DataType>,
) -> ast::FnSignature {
    function_name_to_signature(fn_name, element_type)
}
