use tasm_lib::snippet::Snippet;

use crate::{
    ast,
    tasm_code_generator::{size_of, CompilerState},
};

const VECTOR_LIB_INDICATOR: &str = "Vec::";

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
    type_parameter: Option<ast::DataType>,
    state: &mut CompilerState,
) -> String {
    // TODO: This does not allow for a collission of function names in the
    // `tasm-lib` library and this library. Maybe we could prepend all tasm-lib
    // names with something?
    let tasm_type: Option<tasm_lib::snippet::DataType> =
        type_parameter.clone().map(|x| x.try_into().unwrap());
    let snippet_name: String = match vector_fn_name {
        "default" => {
            let snippet: Box<dyn Snippet> =
                Box::new(tasm_lib::list::u32::new::New(tasm_type.unwrap()));
            let entrypoint = snippet.entrypoint().to_owned();
            state.import_snippet(snippet);
            entrypoint
        }
        "push" => {
            let snippet: Box<dyn Snippet> = match size_of(type_parameter.as_ref().unwrap()) {
                1 => Box::new(tasm_lib::list::u32::push::Push::<1>(tasm_type.unwrap())),
                2 => Box::new(tasm_lib::list::u32::push::Push::<2>(tasm_type.unwrap())),
                3 => Box::new(tasm_lib::list::u32::push::Push::<3>(tasm_type.unwrap())),
                5 => Box::new(tasm_lib::list::u32::push::Push::<5>(tasm_type.unwrap())),
                _ => panic!("Cannot push to Vec<{}> yet", &type_parameter.unwrap()),
            };
            let entrypoint = snippet.entrypoint().to_owned();
            state.import_snippet(snippet);
            entrypoint
        }
        "pop" => {
            let snippet: Box<dyn Snippet> = match size_of(type_parameter.as_ref().unwrap()) {
                1 => Box::new(tasm_lib::list::u32::pop::Pop::<1>(tasm_type.unwrap())),
                2 => Box::new(tasm_lib::list::u32::pop::Pop::<2>(tasm_type.unwrap())),
                3 => Box::new(tasm_lib::list::u32::pop::Pop::<3>(tasm_type.unwrap())),
                5 => Box::new(tasm_lib::list::u32::pop::Pop::<5>(tasm_type.unwrap())),
                _ => panic!("Cannot pop to Vec<{}> yet", &type_parameter.unwrap()),
            };
            let entrypoint = snippet.entrypoint().to_owned();
            state.import_snippet(snippet);
            entrypoint
        }
        "len" => {
            let snippet: Box<dyn Snippet> =
                Box::new(tasm_lib::list::u32::length::LengthLong(tasm_type.unwrap()));
            let entrypoint = snippet.entrypoint().to_owned();
            state.import_snippet(snippet);
            entrypoint
        }
        _ => panic!("Unknown list method {vector_fn_name}"),
    };

    snippet_name
}

pub fn function_name_to_signature(
    fn_name: &str,
    element_type: &Option<ast::DataType>,
) -> ast::FnSignature {
    match fn_name {
        "default" => {
            assert!(
                element_type.is_some(),
                "Type parameter for element type needed for list functions"
            );
            ast::FnSignature {
                name: "default".to_string(),
                args: vec![],
                output: ast::DataType::List(Box::new(element_type.as_ref().unwrap().to_owned())),
            }
        }
        &_ => todo!(),
    }
}

pub fn method_name_to_signature(
    fn_name: &str,
    element_type: &Option<ast::DataType>,
) -> ast::FnSignature {
    let receiver = ast::FnArg {
        name: "receiver".to_string(),
        data_type: ast::DataType::List(Box::new(element_type.as_ref().unwrap().to_owned())),
    };

    match fn_name {
        "push" => ast::FnSignature {
            name: "push".to_string(),
            args: vec![
                receiver,
                ast::FnArg {
                    name: "push_arg".to_string(),
                    data_type: element_type.as_ref().unwrap().to_owned(),
                },
            ],
            output: ast::DataType::unit(),
        },

        "pop" => ast::FnSignature {
            name: "pop".to_string(),
            args: vec![receiver],
            output: element_type.as_ref().unwrap().to_owned(),
        },

        "len" => ast::FnSignature {
            name: "len".to_string(),
            args: vec![receiver],
            output: ast::DataType::U32,
        },

        _ => panic!("Unknown method Vec::{fn_name}"),
    }
}
