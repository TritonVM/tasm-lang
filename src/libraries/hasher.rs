use triton_vm::triton_asm;

use crate::{ast, tasm_code_generator::CompilerState};

use super::{CompiledFunction, Library};

const HASHER_LIB_INDICATOR: &str = "H::";

#[derive(Clone, Debug)]
pub struct HasherLib;

impl Library for HasherLib {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        // Any function call that starts with `H::` is assumed to exist in this library
        if full_name.starts_with(HASHER_LIB_INDICATOR) {
            let stripped_name = &full_name[HASHER_LIB_INDICATOR.len()..full_name.len()];
            return Some(stripped_name.to_owned());
        }

        None
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
        panic!("HasherLib does not contain any methods")
    }

    fn function_name_to_signature(
        &self,
        fn_name: &str,
        _type_parameter: Option<ast::DataType>,
        _args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        if fn_name == "hash_pair" {
            get_hash_pair_function().signature
        } else {
            panic!("Unknown function {fn_name}");
        }
    }

    fn call_method(
        &self,
        _method_name: &str,
        _receiver_type: &ast::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        panic!("HasherLib does not contain any methods")
    }

    fn call_function(
        &self,
        fn_name: &str,
        _type_parameter: Option<ast::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        if fn_name == "hash_pair" {
            get_hash_pair_function().body
        } else {
            panic!("Unknown function {fn_name}");
        }
    }

    fn get_graft_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn graft_function(
        &self,
        _fn_name: &str,
        _args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
    ) -> Option<ast::Expr<super::Annotation>> {
        panic!("HasherLib cannot graft")
    }

    fn graft_method(
        &self,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::MethodCall<super::Annotation>> {
        None
    }
}

fn get_hash_pair_function() -> CompiledFunction {
    let fn_signature = ast::FnSignature {
        name: "hash_pair".to_owned(),
        args: vec![
            ast::AbstractArgument::ValueArgument(ast::AbstractValueArg {
                name: "left".to_owned(),
                data_type: ast::DataType::Digest,
                mutable: false,
            }),
            ast::AbstractArgument::ValueArgument(ast::AbstractValueArg {
                name: "right".to_owned(),
                data_type: ast::DataType::Digest,
                mutable: false,
            }),
        ],
        output: ast::DataType::Digest,
        // If the definition of Tip5's `hash_pair` was changed, this could
        // be left-to-right instead
        arg_evaluation_order: ast::ArgEvaluationOrder::RightToLeft,
    };

    CompiledFunction {
        signature: fn_signature,
        body: triton_asm!(hash pop pop pop pop pop),
    }
}
