use triton_vm::triton_asm;

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
        _args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        panic!("TASM lib only contains functions, no methods")
    }

    fn function_name_to_signature(
        &self,
        stripped_name: &str,
        _type_parameter: Option<ast::DataType>,
        _args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        // TODO:
        // Note that this function expects `fn_name` to be stripped of `tasm::`
        // Maybe that behavior should be changed though?
        let snippet = tasm_lib::exported_snippets::name_to_snippet(stripped_name);

        let name = snippet.entrypoint();
        let mut args: Vec<ast::AbstractArgument> = vec![];
        for (ty, name) in snippet.inputs().into_iter() {
            let fn_arg = ast::AbstractValueArg {
                name,
                data_type: ty.into(),
                // The tasm snippet input arguments are all considered mutable
                mutable: true,
            };
            args.push(ast::AbstractArgument::ValueArgument(fn_arg));
        }

        let mut output_types: Vec<ast::DataType> = vec![];
        for (otl, _name) in snippet.outputs() {
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
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        panic!("TASM lib only contains functions, no methods")
    }

    fn call_function(
        &self,
        stripped_name: &str,
        _type_parameter: Option<ast::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        // TODO:
        // Note that this function expects `fn_name` to be stripped of `tasm::`
        // Maybe that behavior should be changed though?
        let snippet = tasm_lib::exported_snippets::name_to_snippet(stripped_name);
        let entrypoint = snippet.entrypoint();
        state.import_snippet(snippet);

        triton_asm!(call { entrypoint })
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

    fn graft_method(
        &self,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::MethodCall<super::Annotation>> {
        None
    }
}
