use crate::{ast, tasm_code_generator::CompilerState};

// pub fn import_tasm_snippet(
//     vector_fn_name: &str,
//     type_parameter: &Option<ast::DataType>,
//     state: &mut CompilerState,
// ) -> String {
//     let snippet = name_to_tasm_lib_snippet(vector_fn_name, type_parameter)
//         .unwrap_or_else(|| panic!("Unknown function name {vector_fn_name}"));
//     let entrypoint = snippet.entrypoint();
//     state.import_snippet(snippet);

//     entrypoint
// }

pub fn function_name_to_signature(
    fn_name: &str,
    receiver_type: &ast::DataType,
) -> ast::FnSignature {
    // let snippet = name_to_tasm_lib_snippet(fn_name, element_type)
    //     .unwrap_or_else(|| panic!("Unknown function name {fn_name}"));

    let (args, output) = match fn_name {
        "leading_zeros" => (
            vec![ast::FnArg {
                name: "val".to_string(),
                data_type: receiver_type.to_owned(),
                mutable: true,
            }],
            receiver_type.to_owned(),
        ),
        _ => panic!(),
    };
    // let input_types_lib = snippet.input_types();
    // let output_types_lib = snippet.output_types();
    // let name = snippet.entrypoint();
    // let mut args: Vec<ast::FnArg> = vec![];
    // for (i, itl) in input_types_lib.into_iter().enumerate() {
    //     let fn_arg = ast::FnArg {
    //         name: format!("input_{i}"),
    //         data_type: itl.into(),
    //         // The tasm snippet input arguments are all considered mutable
    //         mutable: true,
    //     };
    //     args.push(fn_arg);
    // }

    // let mut output_types: Vec<ast::DataType> = vec![];
    // for otl in output_types_lib {
    //     output_types.push(otl.into());
    // }

    // let output = match output_types.len() {
    //     1 => output_types[0].clone(),
    //     0 => ast::DataType::Tuple(vec![]),
    //     _ => ast::DataType::Tuple(output_types),
    // };

    ast::FnSignature {
        name: fn_name.to_owned(),
        args,
        output,
    }
}

pub fn method_name_to_signature(fn_name: &str, receiver_type: &ast::DataType) -> ast::FnSignature {
    function_name_to_signature(fn_name, receiver_type)
}

pub fn get_method_name(name: &str) -> Option<&str> {
    if matches!(name, "leading_zeros" | "count_ones") {
        Some(name)
    } else {
        None
    }
}
