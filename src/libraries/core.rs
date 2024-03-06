use syn::punctuated::Punctuated;
use syn::token::Comma;
use tasm_lib::triton_vm::prelude::LabelledInstruction;

use crate::ast::Expr;
use crate::ast::FnSignature;
use crate::ast_types::DataType;
use crate::graft::Graft;
use crate::libraries::Annotation;
use crate::libraries::Library;
use crate::tasm_code_generator::CompilerState;
use crate::type_checker::CheckState;

pub(crate) mod array;
pub(crate) mod option_type;
pub(crate) mod result_type;

/// Everything that lives in the Rust `core` module belongs in here.
#[derive(Debug)]
pub(crate) struct Core;

impl Library for Core {
    fn get_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn get_method_name(&self, method_name: &str, receiver_type: &DataType) -> Option<String> {
        match (receiver_type, method_name) {
            (DataType::Array(_), "len") => Some("len".to_owned()),
            _ => None,
        }
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        receiver_type: &DataType,
        args: &[Expr<Annotation>],
        _type_checker_state: &CheckState,
    ) -> FnSignature {
        match (receiver_type, method_name, args.len()) {
            (DataType::Array(array_type), "len", 1) => array::len_method_signature(array_type),
            _ => panic!(),
        }
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
        method_name: &str,
        receiver_type: &DataType,
        args: &[Expr<Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        match (receiver_type, method_name, args.len()) {
            (DataType::Array(array_type), "len", 1) => array::len_method_body(array_type),
            _ => panic!(),
        }
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

    fn graft_method_call(
        &self,
        _graft_config: &mut Graft,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<Expr<super::Annotation>> {
        None
    }
}
