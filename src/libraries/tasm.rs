use tasm_lib::triton_vm::prelude::*;

use crate::ast;
use crate::ast_types;
use crate::graft::Graft;
use crate::tasm_code_generator::CompilerState;

use super::tasm_lib_snippet_to_fn_signature;
use super::Library;

const TASM_LIB_INDICATOR: &str = "tasm::";

#[derive(Clone, Debug)]
pub(crate) struct TasmLibrary {
    pub(crate) list_type: ast_types::ListType,
}

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
        stripped_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        // Note that this function expects `fn_name` to be stripped of `tasm::`
        // Maybe that behavior should be changed though?
        let snippet = tasm_lib::exported_snippets::name_to_snippet(stripped_name);

        tasm_lib_snippet_to_fn_signature(self.list_type, snippet)
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
        stripped_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
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
        _graft_config: &mut Graft,
        _fn_name: &str,
        _args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        _function_type_parameter: Option<ast_types::DataType>,
    ) -> Option<ast::Expr<super::Annotation>> {
        panic!("No grafting is handled by TASM lib")
    }

    fn graft_method(
        &self,
        _graft_config: &mut Graft,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        None
    }
}
