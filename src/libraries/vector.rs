use crate::ast;

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
        return Some(name);
    } else {
        return None;
    }
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
    match fn_name {
        "push" => ast::FnSignature {
            name: "push".to_string(),
            args: vec![ast::FnArg {
                name: "push_arg".to_string(),
                data_type: element_type.as_ref().unwrap().to_owned(),
            }],
            output: ast::DataType::unit(),
        },

        "pop" => ast::FnSignature {
            name: "pop".to_string(),
            args: vec![],
            output: element_type.as_ref().unwrap().to_owned(),
        },

        "len" => ast::FnSignature {
            name: "len".to_string(),
            args: vec![],
            output: ast::DataType::U32,
        },

        _ => panic!("Unknown method Vec::{fn_name}"),
    }
}
