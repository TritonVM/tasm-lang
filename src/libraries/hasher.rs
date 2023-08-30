use triton_vm::triton_asm;

use crate::{
    ast, ast_types,
    tasm_code_generator::{subroutine::SubRoutine, CompilerState},
};

use super::{Library, LibraryFunction};

const HASHER_LIB_INDICATOR: &str = "H::";
const HASH_PAIR_FUNCTION_NAME: &str = "H::hash_pair";

#[derive(Clone, Debug)]
pub struct HasherLib;

impl Library for HasherLib {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        // Any function call that starts with `H::` is assumed to exist in this library
        if full_name.starts_with(HASHER_LIB_INDICATOR) {
            return Some(full_name.to_owned());
        }

        None
    }

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
    ) -> ast::FnSignature {
        panic!("HasherLib does not contain any methods")
    }

    fn function_name_to_signature(
        &self,
        fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        if fn_name == HASH_PAIR_FUNCTION_NAME {
            return get_hash_pair_function().signature;
        }

        panic!("Unknown function {fn_name}");
    }

    fn call_method(
        &self,
        _method_name: &str,
        _receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        panic!("HasherLib does not contain any methods")
    }

    fn call_function(
        &self,
        fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        if fn_name == HASH_PAIR_FUNCTION_NAME {
            let hash_pair: SubRoutine = get_hash_pair_function().try_into().unwrap();
            let hash_pair_label = hash_pair.get_label();
            state.add_library_function(hash_pair);

            return triton_asm!(call { hash_pair_label });
        }

        panic!("Unknown function {fn_name}");
    }

    fn get_graft_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn graft_function(
        &self,
        _fn_name: &str,
        _args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        _list_type: ast_types::ListType,
    ) -> Option<ast::Expr<super::Annotation>> {
        panic!("HasherLib cannot graft")
    }

    fn graft_method(
        &self,
        _rust_method_call: &syn::ExprMethodCall,
        _list_type: ast_types::ListType,
    ) -> Option<ast::Expr<super::Annotation>> {
        None
    }
}

fn get_hash_pair_function() -> LibraryFunction {
    let fn_signature = ast::FnSignature {
        name: "hash_pair".to_owned(),
        args: vec![
            ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
                name: "left".to_owned(),
                data_type: ast_types::DataType::Digest,
                mutable: false,
            }),
            ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
                name: "right".to_owned(),
                data_type: ast_types::DataType::Digest,
                mutable: false,
            }),
        ],
        output: ast_types::DataType::Digest,
        // If the definition of Tip5's `hash_pair` was changed, this could
        // be left-to-right instead
        arg_evaluation_order: ast::ArgEvaluationOrder::RightToLeft,
    };

    LibraryFunction {
        signature: fn_signature,
        body: triton_asm!(hash pop pop pop pop pop),
    }
}
