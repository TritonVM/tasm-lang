use tasm_lib::triton_vm::prelude::*;

use crate::ast;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::composite_types::CompositeTypes;
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

    fn handle_function_call(
        &self,
        full_name: &str,
        _qualified_self_type: &Option<DataType>,
    ) -> bool {
        full_name.starts_with(TASM_LIB_INDICATOR)
    }

    /// tasm-lib contains no methods, only functions
    fn handle_method_call(&self, _method_name: &str, _receiver_type: &ast_types::DataType) -> bool {
        false
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
        _qualified_self_type: &Option<DataType>,
        _composite_types: &mut CompositeTypes,
    ) -> ast::FnSignature {
        let stripped_name = &full_name[TASM_LIB_INDICATOR.len()..full_name.len()];
        let snippet = tasm_lib::exported_snippets::name_to_snippet(stripped_name).unwrap();

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
        _qualified_self_type: &Option<DataType>,
    ) -> Vec<LabelledInstruction> {
        let stripped_name = &full_name[TASM_LIB_INDICATOR.len()..full_name.len()];
        let snippet = tasm_lib::exported_snippets::name_to_snippet(stripped_name).unwrap();
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
