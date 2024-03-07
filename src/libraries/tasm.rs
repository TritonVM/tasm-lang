use tasm_lib::triton_vm::prelude::*;

use crate::ast;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::graft::Graft;
use crate::tasm_code_generator::CompilerState;

use super::Library;

const TASM_LIB_INDICATOR: &str = "tasm::";

#[derive(Clone, Debug)]
pub(crate) struct TasmLibrary;

impl Library for TasmLibrary {
    fn graft_type(
        &self,
        _graft: &mut Graft,
        _rust_type_as_string: &str,
        _path_args: &syn::PathArguments,
    ) -> Option<DataType> {
        None
    }

    fn get_function_name(&self, full_name: &str) -> Option<String> {
        if full_name.starts_with(TASM_LIB_INDICATOR) {
            Some(full_name.to_owned())
        } else {
            None
        }
    }

    /// tasm-lib contains no methods, only functions
    fn get_method_name(
        &self,
        _method_name: &str,
        _receiver_type: &ast_types::DataType,
    ) -> Option<String> {
        None
    }

    fn method_name_to_signature(
        &self,
        _fn_name: &str,
        _receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature {
        panic!("TASM lib only contains functions, no methods")
    }

    fn function_name_to_signature(
        &self,
        full_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        let stripped_name = &full_name[TASM_LIB_INDICATOR.len()..full_name.len()];
        let snippet = tasm_lib::exported_snippets::name_to_snippet(stripped_name);

        ast::FnSignature::from_basic_snippet(snippet)
    }

    fn call_method(
        &self,
        _method_name: &str,
        _receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        panic!("TASM lib only contains functions, no methods")
    }

    fn call_function(
        &self,
        full_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        let stripped_name = &full_name[TASM_LIB_INDICATOR.len()..full_name.len()];
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
        _graft_config: &mut Graft,
        _fn_name: &str,
        _args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        _function_type_parameter: Option<ast_types::DataType>,
    ) -> Option<ast::Expr<super::Annotation>> {
        panic!("No grafting is handled by TASM lib")
    }

    fn graft_method_call(
        &self,
        _graft_config: &mut Graft,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        None
    }
}
