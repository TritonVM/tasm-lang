use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::ExprMethodCall;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;

use crate::ast::{Expr, FnSignature};
use crate::ast_types;
use crate::ast_types::DataType;
use crate::graft::Graft;
use crate::libraries::{Annotation, Library};
use crate::tasm_code_generator::CompilerState;
use crate::type_checker::CheckState;

#[derive(Debug)]
pub struct Result;

const FUNCTION_NAME_OK: &str = "Ok";

impl Library for Result {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        if full_name == FUNCTION_NAME_OK {
            return Some(full_name.to_owned());
        }
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
        fn_name: &str,
        type_parameter: Option<DataType>,
        args: &[Expr<Annotation>],
    ) -> FnSignature {
        assert_eq!(fn_name, FUNCTION_NAME_OK);
        assert_eq!(1, args.len());

        let type_parameter = type_parameter.unwrap();
        let argument = ast_types::AbstractValueArg {
            name: "x".to_string(),
            data_type: type_parameter.clone(),
            mutable: false,
        };
        let argument = ast_types::AbstractArgument::ValueArgument(argument);

        FnSignature {
            name: fn_name.to_owned(),
            args: vec![argument],
            output: DataType::Result(Box::new(type_parameter)),
            arg_evaluation_order: Default::default(),
        }
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
        fn_name: &str,
        _type_parameter: Option<DataType>,
        _args: &[Expr<Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        assert_eq!(fn_name, FUNCTION_NAME_OK);
        triton_asm! { push 1 }
    }

    fn get_graft_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn graft_function(
        &self,
        _graft_config: &Graft,
        _fn_name: &str,
        _args: &Punctuated<syn::Expr, Comma>,
        _type_parameter: Option<DataType>,
    ) -> Option<Expr<Annotation>> {
        panic!()
    }

    fn graft_method(
        &self,
        _graft_config: &Graft,
        _rust_method_call: &ExprMethodCall,
    ) -> Option<Expr<Annotation>> {
        None
    }
}
