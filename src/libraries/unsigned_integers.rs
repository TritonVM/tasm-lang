use tasm_lib::snippet::BasicSnippet;
use triton_vm::triton_asm;

use super::{CompiledFunction, Library};
use crate::{ast, tasm_code_generator::CompilerState};

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
        _args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        if method_name == "count_ones" && *receiver_type == ast::DataType::U32 {
            return get_count_ones_u32_method().signature;
        }

        let snippet = name_to_tasm_lib_snippet(method_name, receiver_type)
            .unwrap_or_else(|| panic!("Unknown function name {method_name}"));

        let name = snippet.entrypoint();

        let mut args: Vec<ast::AbstractArgument> = vec![];
        for (ty, name) in snippet.inputs().into_iter() {
            let fn_arg = ast::AbstractValueArg {
                name,
                data_type: ty.into(),
                mutable: true,
            };
            args.push(ast::AbstractArgument::ValueArgument(fn_arg));
        }

        let mut output_types: Vec<ast::DataType> = vec![];
        for (ty, _name) in snippet.outputs() {
            output_types.push(ty.into());
        }

        let output = match output_types.len() {
            1 => output_types[0].clone(),
            0 => ast::DataType::Tuple(vec![]),
            _ => ast::DataType::Tuple(output_types),
        };

        ast::FnSignature {
            name,
            args,
            output,
            arg_evaluation_order: Default::default(),
        }
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast::DataType>,
        _args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        panic!("unsigned_integers lib does not contain any functions");
    }

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &ast::DataType,
        _args: &[ast::Expr<super::Annotation>],
        state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        if method_name == "count_ones" && ast::DataType::U32 == *receiver_type {
            return get_count_ones_u32_method().body;
        }

        let snippet = name_to_tasm_lib_snippet(method_name, receiver_type)
            .unwrap_or_else(|| panic!("Unknown function name {method_name}"));
        let entrypoint = snippet.entrypoint();
        state.import_snippet(snippet);

        triton_asm!(call { entrypoint })
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
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

    fn graft_method(
        &self,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::MethodCall<super::Annotation>> {
        None
    }
}

fn get_count_ones_u32_method() -> CompiledFunction {
    let fn_signature = ast::FnSignature {
        name: "count_ones".to_owned(),
        args: vec![ast::AbstractArgument::ValueArgument(
            ast::AbstractValueArg {
                name: "value".to_owned(),
                data_type: ast::DataType::U32,
                mutable: false,
            },
        )],
        output: ast::DataType::U32,
        arg_evaluation_order: Default::default(),
    };

    CompiledFunction {
        signature: fn_signature,
        body: triton_asm!(pop_count),
    }
}

/// Map list-function or method name to the TASM lib snippet type
fn name_to_tasm_lib_snippet(
    public_name: &str,
    receiver_type: &ast::DataType,
) -> Option<Box<dyn BasicSnippet>> {
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
