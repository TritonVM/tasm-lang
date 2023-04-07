use tasm_lib::snippet::Snippet;
use triton_opcodes::instruction::LabelledInstruction;
use triton_opcodes::instruction::{AnInstruction::*, LabelledInstruction::*};
use triton_opcodes::shortcuts::*;

use crate::{ast, tasm_code_generator::CompilerState};

use super::{CompiledFunction, Library};

#[derive(Clone, Debug)]
pub struct UnsignedIntegersLib;

impl Library for UnsignedIntegersLib {
    fn get_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn get_method_name(&self, method_name: &str, _receiver_type: &ast::DataType) -> Option<String> {
        if matches!(method_name, "leading_zeros" | "count_ones") {
            Some(method_name.to_owned())
        } else {
            None
        }
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        receiver_type: &ast::DataType,
    ) -> ast::FnSignature {
        if method_name == "count_ones" && *receiver_type == ast::DataType::U32 {
            return get_count_ones_u32_method().signature;
        }

        let snippet = name_to_tasm_lib_snippet(method_name, receiver_type)
            .unwrap_or_else(|| panic!("Unknown function name {method_name}"));

        let input_types_lib = snippet.input_types();
        let output_types_lib = snippet.output_types();
        let name = snippet.entrypoint();
        let mut args: Vec<ast::FnArg> = vec![];
        for (i, itl) in input_types_lib.into_iter().enumerate() {
            let fn_arg = ast::FnArg {
                name: format!("input_{i}"),
                data_type: itl.into(),
                // The tasm snippet input arguments are all considered mutable
                mutable: true,
            };
            args.push(fn_arg);
        }

        let mut output_types: Vec<ast::DataType> = vec![];
        for otl in output_types_lib {
            output_types.push(otl.into());
        }

        let output = match output_types.len() {
            1 => output_types[0].clone(),
            0 => ast::DataType::Tuple(vec![]),
            _ => ast::DataType::Tuple(output_types),
        };

        ast::FnSignature { name, args, output }
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast::DataType>,
    ) -> ast::FnSignature {
        panic!("unsigned_integers lib does not contain any functions");
    }

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &ast::DataType,
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        if method_name == "count_ones" && ast::DataType::U32 == *receiver_type {
            return get_count_ones_u32_method().body;
        }

        let snippet = name_to_tasm_lib_snippet(method_name, receiver_type)
            .unwrap_or_else(|| panic!("Unknown function name {method_name}"));
        let entrypoint = snippet.entrypoint();
        state.import_snippet(snippet);

        vec![call(entrypoint)]
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast::DataType>,
        _state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        panic!("unsigned_integers lib does not contain any functions");
    }

    fn get_graft_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn graft_function(
        &self,
        _fn_name: &str,
        _args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
    ) -> Option<ast::Expr<super::Annotation>> {
        panic!("unsigned_integers lib cannot graft");
    }
}

fn get_count_ones_u32_method() -> CompiledFunction {
    let fn_signature = ast::FnSignature {
        name: "count_ones".to_owned(),
        args: vec![ast::FnArg {
            name: "value".to_owned(),
            data_type: ast::DataType::U32,
            mutable: false,
        }],
        output: ast::DataType::U32,
    };

    CompiledFunction {
        signature: fn_signature,
        body: vec![Instruction(PopCount)],
    }
}

/// Map list-function or method name to the TASM lib snippet type
fn name_to_tasm_lib_snippet(
    public_name: &str,
    receiver_type: &ast::DataType,
) -> Option<Box<dyn Snippet>> {
    match public_name {
        "leading_zeros" => match receiver_type {
            ast::DataType::U32 => Some(Box::new(
                tasm_lib::arithmetic::u32::leading_zeros_u32::LeadingZerosU32,
            )),
            ast::DataType::U64 => Some(Box::new(
                tasm_lib::arithmetic::u64::leading_zeros_u64::LeadingZerosU64,
            )),
            _ => panic!("Dont know `leading_zeros` for {receiver_type}"),
        },
        "count_ones" => match receiver_type {
            ast::DataType::U64 => Some(Box::new(
                tasm_lib::arithmetic::u64::popcount_u64::PopCountU64,
            )),
            _ => panic!("Dont know `count_ones` for {receiver_type}"),
        },
        _ => None,
    }
}
