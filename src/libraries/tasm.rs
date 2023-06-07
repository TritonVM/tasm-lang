use triton_opcodes::shortcuts::call;

use crate::ast::{self, FnSignature};
use crate::tasm_code_generator::CompilerState;

use super::Library;

const TASM_LIB_INDICATOR: &str = "tasm::";

#[derive(Clone, Debug)]
pub struct TasmLibrary;

impl Library for TasmLibrary {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        if full_name.starts_with(TASM_LIB_INDICATOR) {
            let stripped_name = &full_name[TASM_LIB_INDICATOR.len()..full_name.len()];
            return Some(stripped_name.to_owned());
        }

        None
    }

    /// tasm-lib contains no methods, only functions
    fn get_method_name(
        &self,
        _method_name: &str,
        _receiver_type: &ast::DataType,
    ) -> Option<String> {
        None
    }

    fn method_name_to_signature(
        &self,
        _fn_name: &str,
        _receiver_type: &ast::DataType,
    ) -> ast::FnSignature {
        panic!("TASM lib only contains functions, no methods")
    }

    fn function_name_to_signature(
        &self,
        fn_name: &str,
        _type_parameter: Option<ast::DataType>,
    ) -> ast::FnSignature {
        let snippet = tasm_lib::exported_snippets::name_to_snippet(fn_name);

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

        FnSignature {
            name,
            args,
            output,
            arg_evaluation_order: Default::default(),
        }
    }

    fn call_method(
        &self,
        _method_name: &str,
        _receiver_type: &ast::DataType,
        _state: &mut CompilerState,
    ) -> Vec<triton_opcodes::instruction::LabelledInstruction> {
        panic!("TASM lib only contains functions, no methods")
    }

    fn call_function(
        &self,
        fn_name: &str,
        _type_parameter: Option<ast::DataType>,
        state: &mut CompilerState,
    ) -> Vec<triton_opcodes::instruction::LabelledInstruction> {
        let snippet = tasm_lib::exported_snippets::name_to_snippet(fn_name);
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
        panic!("No grafting is handled by TASM lib")
    }
}
