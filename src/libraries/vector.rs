use itertools::Itertools;
use tasm_lib::{list::ListType, snippet::BasicSnippet};
use triton_vm::triton_asm;

use crate::{
    ast, ast_types,
    graft::{self, graft_expr, Annotation},
    tasm_code_generator::CompilerState,
    type_checker::GetType,
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

    fn get_method_name(
        &self,
        method_name: &str,
        _receiver_type: &ast_types::DataType,
    ) -> Option<String> {
        if matches!(method_name, "push" | "pop" | "len" | "map") {
            Some(method_name.to_owned())
        } else {
            None
        }
    }

    fn method_name_to_signature(
        &self,
        fn_name: &str,
        receiver_type: &ast_types::DataType,
        args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        self.function_name_to_signature(fn_name, receiver_type.type_parameter(), args)
    }

    fn function_name_to_signature(
        &self,
        fn_name: &str,
        type_parameter: Option<ast_types::DataType>,
        args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        println!("fn_name: {fn_name}");
        println!("type_parameter: {type_parameter:?}");
        println!("args: {args:?}");
        let snippet = name_to_tasm_lib_snippet(fn_name, &type_parameter, args)
            .unwrap_or_else(|| panic!("Unknown function name {fn_name}"));

        let name = snippet.entrypoint();
        let mut args: Vec<ast_types::AbstractArgument> = vec![];
        for (ty, name) in snippet.inputs().into_iter() {
            let fn_arg = ast_types::AbstractValueArg {
                name,
                data_type: ty.into(),
                mutable: true,
            };
            args.push(ast_types::AbstractArgument::ValueArgument(fn_arg));
        }

        let mut output_types: Vec<ast_types::DataType> = vec![];
        for (ty, _name) in snippet.outputs() {
            output_types.push(ty.into());
        }

        let output = match output_types.len() {
            1 => output_types[0].clone(),
            0 => ast_types::DataType::Tuple(vec![]),
            _ => ast_types::DataType::Tuple(output_types),
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
        receiver_type: &ast_types::DataType,
        args: &[ast::Expr<super::Annotation>],
        state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let type_param: ast_types::DataType =
            if let ast_types::DataType::List(type_param) = receiver_type {
                *type_param.to_owned()
            } else {
                panic!(
                "Cannot call vector method without type param. Got receiver_type: {receiver_type}"
            )
            };
        // find inner function if needed
        let snippet = name_to_tasm_lib_snippet(method_name, &Some(type_param), args)
            .unwrap_or_else(|| panic!("Unknown function name {method_name}"));
        let entrypoint = snippet.entrypoint();
        state.import_snippet(snippet);

        triton_asm!(call { entrypoint })
    }

    fn call_function(
        &self,
        fn_name: &str,
        type_parameter: Option<ast_types::DataType>,
        args: &[ast::Expr<super::Annotation>],
        state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let snippet = name_to_tasm_lib_snippet(fn_name, &type_parameter, args)
            .unwrap_or_else(|| panic!("Unknown function name {fn_name}"));
        let entrypoint = snippet.entrypoint();
        state.import_snippet(snippet);

        triton_asm!(call { entrypoint })
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
    ) -> Option<ast::Expr<super::Annotation>> {
        const POP_NAME: &str = "pop";
        const COLLECT_VEC_NAME: &str = "collect_vec";
        const UNWRAP_NAME: &str = "unwrap";
        const INTO_ITER_NAME: &str = "into_iter";
        const MAP_NAME: &str = "map";

        let last_method_name = rust_method_call.method.to_string();

        match last_method_name.as_str() {
            UNWRAP_NAME => {
                match rust_method_call.receiver.as_ref() {
                    syn::Expr::MethodCall(rust_inner_method_call) => {
                        let inner_method_call = graft::graft_method_call(rust_inner_method_call);
                        let inner_method_call = match inner_method_call {
                            ast::Expr::MethodCall(mc) => mc,
                            _ => return None,
                        };
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

                        Some(ast::Expr::MethodCall(ast::MethodCall {
                            method_name: POP_NAME.to_owned(),
                            args,
                            annot,
                        }))
                    }
                    _ => None,
                }
            }
            COLLECT_VEC_NAME => {
                match rust_method_call.receiver.as_ref() {
                    syn::Expr::MethodCall(rust_inner_method_call) => {
                        let inner_method_call = graft::graft_method_call(rust_inner_method_call);
                        let inner_method_call = match inner_method_call {
                            ast::Expr::MethodCall(mc) => mc,
                            _ => return None,
                        };
                        if inner_method_call.method_name != MAP_NAME {
                            return None;
                        }

                        let identifier = match rust_inner_method_call.receiver.as_ref() {
                            syn::Expr::MethodCall(rust_inner_inner_method_call) => {
                                let maybe_iter_name =
                                    rust_inner_inner_method_call.method.to_string();
                                if maybe_iter_name != INTO_ITER_NAME {
                                    panic!("Only allowed syntax with `map` is `x.into_iter().map(<function_name>).collect_vec()")
                                }

                                let inner_inner_method_call =
                                    graft::graft_method_call(rust_inner_inner_method_call);
                                let inner_inner_method_call = match inner_inner_method_call {
                                    ast::Expr::MethodCall(mc) => mc,
                                    _ => return None,
                                };

                                match &inner_inner_method_call.args[0] {
                                    ast::Expr::Var(ident) => ident.to_owned(),
                                    // Maybe cover more cases here?
                                    _ => todo!(),
                                }
                            }
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
                        let annot: Annotation = Default::default();

                        Some(ast::Expr::MethodCall(ast::MethodCall {
                            method_name: MAP_NAME.to_owned(),
                            args,
                            annot,
                        }))
                    }
                    _ => todo!(),
                }
            }
            _ => None,
        }
    }
}

/// Map list-function or method name to the TASM lib snippet type
fn name_to_tasm_lib_snippet(
    public_name: &str,
    type_parameter: &Option<ast_types::DataType>,
    args: &[ast::Expr<super::Annotation>],
) -> Option<Box<dyn BasicSnippet>> {
    println!("type_parameter: {type_parameter:?}");
    let tasm_type: Option<tasm_lib::snippet::DataType> =
        type_parameter.clone().map(|x| x.try_into().unwrap());
    println!("tasm_type: {tasm_type:?}");
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
        "map" => {
            let inner_function_type =
                if let ast_types::DataType::Function(fun_type) = args[1].get_type() {
                    fun_type
                } else {
                    panic!()
                };
            let inner_function_name = if let ast::Expr::Var(ident) = &args[1] {
                ident
            } else {
                panic!()
            };

            let lnat = tasm_lib::list::higher_order::inner_function::NoFunctionBody {
                label_name: inner_function_name.to_string(),
                input_type: inner_function_type.input_argument.try_into().unwrap(),
                output_type: inner_function_type.output.try_into().unwrap(),
            };
            Some(Box::new(tasm_lib::list::higher_order::map::Map {
                list_type: ListType::Safe,
                f: tasm_lib::list::higher_order::inner_function::InnerFunction::NoFunctionBody(
                    lnat,
                ),
            }))
        }
        _ => None,
    }
}
