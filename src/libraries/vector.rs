use itertools::Itertools;
use tasm_lib::snippet::Snippet;
use triton_opcodes::{instruction::LabelledInstruction, shortcuts::call};

use crate::{
    ast,
    graft::{self, graft_expr},
    tasm_code_generator::CompilerState,
};

use super::Library;

const VECTOR_LIB_INDICATOR: &str = "Vec::";

#[derive(Clone, Debug)]
pub struct VectorLib;

impl Library for VectorLib {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        if full_name.starts_with(VECTOR_LIB_INDICATOR) {
            let stripped_name = &full_name[VECTOR_LIB_INDICATOR.len()..full_name.len()];
            return Some(stripped_name.to_owned());
        }

        None
    }

    fn get_method_name(&self, method_name: &str, _receiver_type: &ast::DataType) -> Option<String> {
        if matches!(method_name, "push" | "pop" | "len") {
            Some(method_name.to_owned())
        } else {
            None
        }
    }

    fn method_name_to_signature(
        &self,
        fn_name: &str,
        receiver_type: &ast::DataType,
    ) -> ast::FnSignature {
        self.function_name_to_signature(fn_name, receiver_type.type_parameter())
    }

    fn function_name_to_signature(
        &self,
        fn_name: &str,
        type_parameter: Option<ast::DataType>,
    ) -> ast::FnSignature {
        let snippet = name_to_tasm_lib_snippet(fn_name, &type_parameter)
            .unwrap_or_else(|| panic!("Unknown function name {fn_name}"));

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

        ast::FnSignature {
            name,
            args,
            output,
            arg_evaluation_order: Default::default(),
        }
    }

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &ast::DataType,
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        let type_param: ast::DataType = if let ast::DataType::List(type_param) = receiver_type {
            *type_param.to_owned()
        } else {
            panic!(
                "Cannot call vector method without type param. Got receiver_type: {receiver_type}"
            )
        };
        let snippet = name_to_tasm_lib_snippet(method_name, &Some(type_param))
            .unwrap_or_else(|| panic!("Unknown function name {method_name}"));
        let entrypoint = snippet.entrypoint();
        state.import_snippet(snippet);

        vec![call(entrypoint)]
    }

    fn call_function(
        &self,
        fn_name: &str,
        type_parameter: Option<ast::DataType>,
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        let snippet = name_to_tasm_lib_snippet(fn_name, &type_parameter)
            .unwrap_or_else(|| panic!("Unknown function name {fn_name}"));
        let entrypoint = snippet.entrypoint();
        state.import_snippet(snippet);

        vec![call(entrypoint)]
    }

    fn get_graft_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn graft_function(
        &self,
        _fn_name: &str,
        _args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
    ) -> Option<ast::Expr<super::Annotation>> {
        todo!()
    }

    fn graft_method(
        &self,
        rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::MethodCall<super::Annotation>> {
        // Handle `pop().unwrap()`. Ignore everything else.
        const UNWRAP_NAME: &str = "unwrap";
        const POP_NAME: &str = "pop";

        let last_method_name = rust_method_call.method.to_string();

        if last_method_name != UNWRAP_NAME {
            return None;
        }

        match rust_method_call.receiver.as_ref() {
            syn::Expr::MethodCall(rust_inner_method_call) => {
                let inner_method_call = graft::graft_method_call(rust_inner_method_call);
                if inner_method_call.method_name != POP_NAME {
                    return None;
                }

                let identifier = match &inner_method_call.args[0] {
                    ast::Expr::Var(ident) => ident.to_owned(),
                    // Maybe cover more cases here?
                    _ => todo!(),
                };

                let mut args = vec![ast::Expr::Var(identifier)];
                args.append(
                    &mut rust_inner_method_call
                        .args
                        .iter()
                        .map(graft_expr)
                        .collect_vec(),
                );
                let annot = Default::default();

                return Some(ast::MethodCall {
                    method_name: POP_NAME.to_owned(),
                    args,
                    annot,
                });
            }
            _ => todo!(),
        }
    }
}

/// Map list-function or method name to the TASM lib snippet type
fn name_to_tasm_lib_snippet(
    public_name: &str,
    type_parameter: &Option<ast::DataType>,
) -> Option<Box<dyn Snippet>> {
    let tasm_type: Option<tasm_lib::snippet::DataType> =
        type_parameter.clone().map(|x| x.try_into().unwrap());
    match public_name {
        "default" => panic!("Change `Vec::default()` to `Vec::with_capacity(n)`."),
        "with_capacity" => Some(Box::new(tasm_lib::list::safe_u32::new::SafeNew(
            tasm_type.unwrap(),
        ))),
        "push" => Some(Box::new(tasm_lib::list::safe_u32::push::SafePush(
            tasm_type.unwrap(),
        ))),
        "pop" => Some(Box::new(tasm_lib::list::safe_u32::pop::SafePop(
            tasm_type.unwrap(),
        ))),
        "len" => Some(Box::new(tasm_lib::list::safe_u32::length::SafeLength(
            tasm_type.unwrap(),
        ))),
        _ => None,
    }
}
