use tasm_lib::snippet::Snippet;
use triton_opcodes::{instruction::LabelledInstruction, shortcuts::call};

use crate::{ast, tasm_code_generator::CompilerState};

use super::Library;

const VECTOR_LIB_INDICATOR: &str = "Vec::";

#[derive(Clone, Debug)]
pub struct VectorLib;

impl Library for VectorLib {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        if full_name.starts_with(VECTOR_LIB_INDICATOR) {
            let stripped_name = &full_name[VECTOR_LIB_INDICATOR.len()..full_name.len()];
            return Some(stripped_name.to_owned());
        }

        None
    }

    fn get_method_name(&self, method_name: &str, _receiver_type: &ast::DataType) -> Option<String> {
        if matches!(method_name, "push" | "pop" | "len") {
            Some(method_name.to_owned())
        } else {
            None
        }
    }

    fn method_name_to_signature(
        &self,
        fn_name: &str,
        receiver_type: &ast::DataType,
    ) -> ast::FnSignature {
        self.function_name_to_signature(fn_name, receiver_type.type_parameter())
    }

    fn function_name_to_signature(
        &self,
        fn_name: &str,
        type_parameter: Option<ast::DataType>,
    ) -> ast::FnSignature {
        let snippet = name_to_tasm_lib_snippet(fn_name, &type_parameter)
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

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &ast::DataType,
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        let type_param: ast::DataType = if let ast::DataType::List(type_param) = receiver_type {
            *type_param.to_owned()
        } else {
            panic!(
                "Cannot call vector method without type param. Got receiver_type: {receiver_type}"
            )
        };
        let snippet = name_to_tasm_lib_snippet(method_name, &Some(type_param))
            .unwrap_or_else(|| panic!("Unknown function name {method_name}"));
        let entrypoint = snippet.entrypoint();
        state.import_snippet(snippet);

        vec![call(entrypoint)]
    }

    fn call_function(
        &self,
        fn_name: &str,
        type_parameter: Option<ast::DataType>,
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        let snippet = name_to_tasm_lib_snippet(fn_name, &type_parameter)
            .unwrap_or_else(|| panic!("Unknown function name {fn_name}"));
        let entrypoint = snippet.entrypoint();
        state.import_snippet(snippet);

        vec![call(entrypoint)]
    }

    fn get_graft_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn graft_function(
        &self,
        _fn_name: &str,
        _args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
    ) -> Option<ast::Expr<super::Annotation>> {
        todo!()
    }
}

/// Map list-function or method name to the TASM lib snippet type
fn name_to_tasm_lib_snippet(
    public_name: &str,
    type_parameter: &Option<ast::DataType>,
) -> Option<Box<dyn Snippet>> {
    let tasm_type: Option<tasm_lib::snippet::DataType> =
        type_parameter.clone().map(|x| x.try_into().unwrap());
    match public_name {
        "default" => panic!("Change `Vec::default()` to `Vec::with_capacity(n)`."),
        "with_capacity" => Some(Box::new(tasm_lib::list::safe_u32::new::SafeNew(
            tasm_type.unwrap(),
        ))),
        "push" => Some(Box::new(tasm_lib::list::safe_u32::push::SafePush(
            tasm_type.unwrap(),
        ))),
        "pop" => Some(Box::new(tasm_lib::list::safe_u32::pop::SafePop(
            tasm_type.unwrap(),
        ))),
        "len" => Some(Box::new(tasm_lib::list::safe_u32::length::SafeLength(
            tasm_type.unwrap(),
        ))),
        _ => None,
    }
}
