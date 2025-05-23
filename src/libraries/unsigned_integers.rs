use tasm_lib::traits::basic_snippet::BasicSnippet;
use tasm_lib::triton_vm::prelude::*;

use super::Library;
use super::LibraryFunction;
use crate::ast;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::composite_types::CompositeTypes;
use crate::graft::Graft;
use crate::tasm_code_generator::CompilerState;
use crate::type_checker::is_u32_based_type;

#[derive(Clone, Debug)]
pub(crate) struct UnsignedIntegersLib;

const LEADING_ZEROS_METHOD: &str = "leading_zeros";
const COUNT_ONES_METHOD: &str = "count_ones";
const POW_METHOD: &str = "pow";
const OVERFLOWING_ADD_METHOD: &str = "overflowing_add";
const OVERFLOWING_SUB_METHOD: &str = "overflowing_sub";
const WRAPPING_SUB_METHOD: &str = "wrapping_sub";
const ILOG2_METHOD: &str = "ilog2";
const NEXT_POWER_OF_TWO_METHOD: &str = "next_power_of_two";

impl Library for UnsignedIntegersLib {
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
        _full_name: &str,
        _qualified_self_type: &Option<DataType>,
    ) -> bool {
        false
    }

    fn handle_method_call(&self, method_name: &str, receiver_type: &ast_types::DataType) -> bool {
        is_u32_based_type(receiver_type)
            && matches!(
                method_name,
                LEADING_ZEROS_METHOD
                    | COUNT_ONES_METHOD
                    | POW_METHOD
                    | OVERFLOWING_ADD_METHOD
                    | OVERFLOWING_SUB_METHOD
                    | WRAPPING_SUB_METHOD
                    | ILOG2_METHOD
                    | NEXT_POWER_OF_TWO_METHOD
            )
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature {
        if !is_u32_based_type(receiver_type) {
            panic!("Cannot call unsigned integer method on non-u32 based value. Receiver type was: {receiver_type}");
        }

        if method_name == COUNT_ONES_METHOD && ast_types::DataType::U32 == *receiver_type {
            return get_count_ones_u32_method().signature;
        }

        if method_name == ILOG2_METHOD && ast_types::DataType::U32 == *receiver_type {
            return ilog2_method_signature();
        }

        let snippet = name_to_tasm_lib_snippet(method_name, receiver_type)
            .unwrap_or_else(|| panic!("Unknown function name {method_name} for {receiver_type}"));

        ast::FnSignature::from_basic_snippet(snippet)
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        _qualified_self_type: &Option<DataType>,
        _composite_types: &mut CompositeTypes,
    ) -> ast::FnSignature {
        panic!("unsigned_integers lib does not contain any functions");
    }

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        if method_name == COUNT_ONES_METHOD && ast_types::DataType::U32 == *receiver_type {
            return get_count_ones_u32_method().body;
        }

        if method_name == ILOG2_METHOD && ast_types::DataType::U32 == *receiver_type {
            return triton_asm!(log_2_floor);
        }

        let snippet = name_to_tasm_lib_snippet(method_name, receiver_type)
            .unwrap_or_else(|| panic!("Unknown function name {method_name} for {receiver_type}"));
        let entrypoint = snippet.entrypoint();
        state.import_snippet(snippet);

        let flip_input_arguments = match receiver_type {
            DataType::U32 => triton_asm!(pick 1),
            DataType::U64 => triton_asm!(
                pick 3
                pick 3
            ),
            DataType::U128 => triton_asm!(
                pick 7
                pick 7
                pick 7
                pick 7
            ),
            _ => unreachable!(),
        };
        let fix_input_arguments_order =
            if method_name == WRAPPING_SUB_METHOD || method_name == OVERFLOWING_SUB_METHOD {
                flip_input_arguments
            } else {
                triton_asm!()
            };

        triton_asm!({&fix_input_arguments_order} call { entrypoint })
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut CompilerState,
        _qualified_self_type: &Option<DataType>,
    ) -> Vec<LabelledInstruction> {
        panic!("unsigned_integers lib does not contain any functions");
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
        panic!("unsigned_integers lib cannot graft");
    }

    fn graft_method_call(
        &self,
        _graft_config: &mut Graft,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        None
    }
}

fn ilog2_method_signature() -> ast::FnSignature {
    ast::FnSignature::value_function_immutable_args(
        ILOG2_METHOD,
        vec![("value", ast_types::DataType::U32)],
        ast_types::DataType::U32,
    )
}

fn get_count_ones_u32_method() -> LibraryFunction {
    let signature = ast::FnSignature::value_function_immutable_args(
        COUNT_ONES_METHOD,
        vec![("value", ast_types::DataType::U32)],
        ast_types::DataType::U32,
    );

    LibraryFunction {
        signature,
        body: triton_asm!(pop_count),
    }
}

/// Map list-function or method name to the TASM lib snippet type
fn name_to_tasm_lib_snippet(
    public_name: &str,
    receiver_type: &ast_types::DataType,
) -> Option<Box<dyn BasicSnippet>> {
    match (public_name, receiver_type) {
        (LEADING_ZEROS_METHOD, ast_types::DataType::U32) => Some(Box::new(
            tasm_lib::arithmetic::u32::leading_zeros::LeadingZeros,
        )),
        (LEADING_ZEROS_METHOD, ast_types::DataType::U64) => Some(Box::new(
            tasm_lib::arithmetic::u64::leading_zeros::LeadingZeros,
        )),
        (COUNT_ONES_METHOD, ast_types::DataType::U64) => {
            Some(Box::new(tasm_lib::arithmetic::u64::popcount::PopCount))
        }
        (POW_METHOD, ast_types::DataType::U32) => {
            Some(Box::new(tasm_lib::arithmetic::u32::safe_pow::SafePow))
        }
        (OVERFLOWING_ADD_METHOD, ast_types::DataType::U64) => Some(Box::new(
            tasm_lib::arithmetic::u64::overflowing_add::OverflowingAdd,
        )),
        (OVERFLOWING_SUB_METHOD, ast_types::DataType::U64) => Some(Box::new(
            tasm_lib::arithmetic::u64::overflowing_sub::OverflowingSub,
        )),
        (WRAPPING_SUB_METHOD, ast_types::DataType::U64) => Some(Box::new(
            tasm_lib::arithmetic::u64::wrapping_sub::WrappingSub,
        )),
        (NEXT_POWER_OF_TWO_METHOD, ast_types::DataType::U32) => Some(Box::new(
            tasm_lib::arithmetic::u32::next_power_of_two::NextPowerOfTwo,
        )),
        _ => None,
    }
}
