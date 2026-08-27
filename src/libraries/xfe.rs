use tasm_lib::triton_vm::prelude::*;

use super::LibraryFunction;
use crate::ast;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::composite_types::CompositeTypes;
use crate::libraries::Library;
use crate::tasm_code_generator::CompilerState;

const UNLIFT_NAME: &str = "unlift";
const METHOD_NAME_MOD_POW_U32: &str = "mod_pow_u32";
const INVERSE_METHOD_NAME: &str = "inverse";

#[derive(Clone, Debug)]
pub(crate) struct XfeLibrary;

fn xfe_lib_has_method(method_name: &str) -> bool {
    method_name == UNLIFT_NAME
        || method_name == METHOD_NAME_MOD_POW_U32
        || method_name == INVERSE_METHOD_NAME
}

fn method_name_to_signature_inner(method_name: &str) -> ast::FnSignature {
    match method_name {
        UNLIFT_NAME => xfe_unlift_method().signature,
        METHOD_NAME_MOD_POW_U32 => ast::FnSignature::value_function_immutable_args(
            METHOD_NAME_MOD_POW_U32,
            vec![("base", DataType::Xfe), ("exponent", DataType::U32)],
            DataType::Xfe,
        ),
        INVERSE_METHOD_NAME => ast::FnSignature::value_function_immutable_args(
            "xfe_inverse",
            vec![("self", ast_types::DataType::Xfe)],
            ast_types::DataType::Xfe,
        ),
        _ => panic!("XFE library does not know method {method_name}"),
    }
}

fn call_method_inner(method_name: &str, state: &mut CompilerState) -> Vec<LabelledInstruction> {
    match method_name {
        UNLIFT_NAME => xfe_unlift_method().body,
        METHOD_NAME_MOD_POW_U32 => {
            let xfe_mod_pow_u32_generic_label = state.import_snippet(Box::new(
                tasm_lib::arithmetic::xfe::mod_pow_u32::XfeModPowU32,
            ));

            triton_asm!(
                // _ [base] exponent

                swap 3
                swap 2
                swap 1
                // _ exponent [base]

                call {xfe_mod_pow_u32_generic_label}

                // _ [result]
            )
        }
        INVERSE_METHOD_NAME => {
            triton_asm!(x_invert)
        }
        _ => panic!("XFE library does not know method {method_name}"),
    }
}

impl Library for XfeLibrary {
    fn handle_function_call(
        &self,
        _full_name: &str,
        _qualified_self_type: &Option<DataType>,
    ) -> bool {
        false
    }

    fn handle_method_call(&self, method_name: &str, receiver_type: &ast_types::DataType) -> bool {
        *receiver_type == ast_types::DataType::Xfe && xfe_lib_has_method(method_name)
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        _receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature {
        method_name_to_signature_inner(method_name)
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        _qualified_self_type: &Option<DataType>,
        _composite_types: &mut CompositeTypes,
    ) -> ast::FnSignature {
        panic!("No functions implemented for XFE library");
    }

    fn call_method(
        &self,
        method_name: &str,
        _receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<LabelledInstruction> {
        call_method_inner(method_name, state)
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
        _qualified_self_type: &Option<DataType>,
    ) -> Vec<LabelledInstruction> {
        panic!("No functions implemented for XFE library");
    }
}

fn xfe_unlift_method() -> LibraryFunction {
    let signature = ast::FnSignature::value_function_immutable_args(
        "unlift",
        vec![("value", ast_types::DataType::Xfe)],
        ast_types::DataType::Bfe,
    );

    LibraryFunction {
        signature,
        body: triton_asm!(swap 2 push 0 eq assert push 0 eq assert),
    }
}
