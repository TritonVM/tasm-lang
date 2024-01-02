use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::ExprMethodCall;
use triton_vm::instruction::LabelledInstruction;

use crate::ast::{Expr, FnSignature};
use crate::ast_types::DataType;
use crate::graft::Graft;
use crate::libraries::{Annotation, Library};
use crate::tasm_code_generator::CompilerState;
use crate::type_checker::CheckState;

pub mod result_type;

/// Everything that lives in the Rust `core` module
/// belongs in here.
#[derive(Debug)]
pub struct Core {}

impl Library for Core {
    fn get_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn get_method_name(&self, _method_name: &str, _receiver_type: &DataType) -> Option<String> {
        None
    }

    fn method_name_to_signature(
        &self,
        _fn_name: &str,
        _receiver_type: &DataType,
        _args: &[Expr<Annotation>],
        _type_checker_state: &CheckState,
    ) -> FnSignature {
        panic!()
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<DataType>,
        _args: &[Expr<Annotation>],
    ) -> FnSignature {
        todo!()
    }

    fn call_method(
        &self,
        _method_name: &str,
        _receiver_type: &DataType,
        _args: &[Expr<Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<DataType>,
        _args: &[Expr<Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_graft_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn graft_function(
        &self,
        _graft_config: &mut Graft,
        _fn_name: &str,
        _args: &Punctuated<syn::Expr, Comma>,
        _type_parameter: Option<DataType>,
    ) -> Option<Expr<Annotation>> {
        panic!()
    }

    fn graft_method(
        &self,
        _graft_config: &mut Graft,
        _rust_method_call: &ExprMethodCall,
    ) -> Option<Expr<Annotation>> {
        None
    }
}
