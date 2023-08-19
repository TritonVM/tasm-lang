use itertools::Itertools;
use triton_vm::triton_asm;

use crate::{
    ast::{self, FnSignature},
    tasm_code_generator::CompilerState,
};

use super::{CompiledFunction, Library};

#[derive(Clone, Debug)]
pub struct IO;

fn functions() -> Vec<CompiledFunction> {
    vec![
        CompiledFunction {
            signature: FnSignature {
                name: "pub_output".to_owned(),
                args: vec![ast::AbstractArgument::ValueArgument(
                    ast::AbstractValueArg {
                        name: "value".to_owned(),
                        data_type: ast::DataType::BFE,
                        mutable: false,
                    },
                )],
                output: ast::DataType::Tuple(vec![]),
                arg_evaluation_order: Default::default(),
            },
            body: triton_asm!(write_io),
        },
        CompiledFunction {
            signature: FnSignature {
                name: "pub_input".to_owned(),
                args: vec![],
                output: ast::DataType::BFE,
                arg_evaluation_order: Default::default(),
            },
            body: triton_asm!(read_io),
        },
        CompiledFunction {
            signature: FnSignature {
                name: "divine".to_owned(),
                args: vec![],
                output: ast::DataType::BFE,
                arg_evaluation_order: Default::default(),
            },
            body: triton_asm!(divine),
        },
    ]
}

impl Library for IO {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        functions()
            .iter()
            .find(|f| f.signature.name == full_name)
            .map(|_| full_name.to_owned())
    }

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
        panic!("IO library does not contain any methods")
    }

    fn function_name_to_signature(
        &self,
        fn_name: &str,
        _type_parameter: Option<ast::DataType>,
        _args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        functions()
            .iter()
            .find(|f| f.signature.name == fn_name)
            .unwrap()
            .signature
            .clone()
    }

    fn call_method(
        &self,
        _method_name: &str,
        _receiver_type: &ast::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        panic!("IO library does not contain any methods")
    }

    fn call_function(
        &self,
        fn_name: &str,
        _type_parameter: Option<ast::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        functions()
            .iter()
            .find(|x| x.signature.name == fn_name)
            .unwrap()
            .body
            .clone()
    }

    fn get_graft_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn graft_function(
        &self,
        _fn_name: &str,
        _args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
    ) -> Option<ast::Expr<super::Annotation>> {
        panic!("IO library does not graft")
    }

    fn graft_method(
        &self,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::MethodCall<super::Annotation>> {
        None
    }
}
