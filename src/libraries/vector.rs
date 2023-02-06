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
