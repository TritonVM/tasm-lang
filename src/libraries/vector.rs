use itertools::Itertools;
use tasm_lib::snippet::BasicSnippet;
use triton_vm::triton_asm;

use crate::{
    ast,
    ast_types::{self, ListType},
    graft::{Annotation, Graft},
    tasm_code_generator::CompilerState,
    type_checker::GetType,
};

use super::Library;

const VECTOR_LIB_INDICATOR: &str = "Vec::";

#[derive(Clone, Debug)]
pub struct VectorLib {
    pub list_type: ListType,
}

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
        receiver_type: &ast_types::DataType,
    ) -> Option<String> {
        if let ast_types::DataType::List(_, _) = receiver_type {
            if matches!(method_name, "push" | "pop" | "len" | "map") {
                return Some(method_name.to_owned());
            }
        }

        None
    }

    fn method_name_to_signature(
        &self,
        fn_name: &str,
        receiver_type: &ast_types::DataType,
        args: &[ast::Expr<super::Annotation>],
        type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature {
        if !matches!(receiver_type, ast_types::DataType::List(_, _)) {
            panic!("Receiver type must be `List`");
        }

        // Special-case on `map` as we need to dig into the type checker state to find the
        // function signature.
        if fn_name == "map" {
            return self.fn_signature_for_map(args, type_checker_state);
        }

        self.function_name_to_signature(fn_name, receiver_type.type_parameter(), args)
    }

    fn function_name_to_signature(
        &self,
        fn_name: &str,
        type_parameter: Option<ast_types::DataType>,
        args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        let snippet = self
            .name_to_tasm_lib_snippet(fn_name, &type_parameter, args)
            .unwrap_or_else(|| panic!("Unknown function name {fn_name}"));

        let name = snippet.entrypoint();
        let mut args: Vec<ast_types::AbstractArgument> = vec![];
        for (ty, name) in snippet.inputs().into_iter() {
            let fn_arg = ast_types::AbstractValueArg {
                name,
                data_type: ast_types::DataType::from_tasm_lib_datatype(ty, self.list_type),
                mutable: true,
            };
            args.push(ast_types::AbstractArgument::ValueArgument(fn_arg));
        }

        let mut output_types: Vec<ast_types::DataType> = vec![];
        for (ty, _name) in snippet.outputs() {
            output_types.push(ast_types::DataType::from_tasm_lib_datatype(
                ty,
                self.list_type,
            ));
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
            if let ast_types::DataType::List(type_param, _list_type) = receiver_type {
                *type_param.to_owned()
            } else {
                panic!(
                "Cannot call vector method without type param. Got receiver_type: {receiver_type}"
            )
            };
        // find inner function if needed
        let snippet = self
            .name_to_tasm_lib_snippet(method_name, &Some(type_param), args)
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
        let snippet = self
            .name_to_tasm_lib_snippet(fn_name, &type_parameter, args)
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
        _graft_config: &Graft,
        _fn_name: &str,
        _args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
    ) -> Option<ast::Expr<super::Annotation>> {
        todo!()
    }

    fn graft_method(
        &self,
        graft_config: &Graft,
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
                        let inner_method_call =
                            graft_config.graft_method_call(rust_inner_method_call);
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
                                .map(|x| graft_config.graft_expr(x))
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
                        let inner_method_call =
                            graft_config.graft_method_call(rust_inner_method_call);
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
                                    graft_config.graft_method_call(rust_inner_inner_method_call);
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
                                .map(|x| graft_config.graft_expr(x))
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
impl VectorLib {
    fn fn_signature_for_map(
        &self,
        args: &[ast::Expr<super::Annotation>],
        type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature {
        let inner_fn_name = match &args[1] {
            ast::Expr::Var(inner_fn_name) => inner_fn_name.to_string(),
            _ => panic!("unsupported"),
        };
        let inner_fn_signature = type_checker_state
            .ftable
            .get(inner_fn_name.as_str())
            .unwrap()
            .to_owned();
        let inner_output = inner_fn_signature.output;
        let inner_input = match &inner_fn_signature.args[0] {
            ast_types::AbstractArgument::FunctionArgument(_) => todo!(),
            ast_types::AbstractArgument::ValueArgument(value_arg) => value_arg.data_type.to_owned(),
        };
        let derived_inner_function_as_function_arg =
            ast_types::AbstractArgument::FunctionArgument(ast_types::AbstractFunctionArg {
                abstract_name: String::from("map_inner_function"),
                function_type: ast_types::FunctionType {
                    input_argument: inner_input,
                    output: inner_output.clone(),
                },
            });
        let vector_as_arg =
            ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
                name: "element_arg_0".to_owned(),
                data_type: args[0].get_type(),
                mutable: false,
            });
        ast::FnSignature {
            name: String::from("map"),
            // TODO: Use List<inner_fn_signature-args> here instead for betetr type checking
            args: vec![vector_as_arg, derived_inner_function_as_function_arg],
            output: ast_types::DataType::List(Box::new(inner_output), ast_types::ListType::Safe),
            arg_evaluation_order: Default::default(),
        }
    }

    fn name_to_tasm_lib_snippet(
        &self,
        public_name: &str,
        type_parameter: &Option<ast_types::DataType>,
        args: &[ast::Expr<super::Annotation>],
    ) -> Option<Box<dyn BasicSnippet>> {
        let tasm_type: Option<tasm_lib::snippet::DataType> =
            type_parameter.clone().map(|x| x.try_into().unwrap());
        match public_name {
            "default" => panic!("Change `Vec::default()` to `Vec::with_capacity(n)`."),
            "with_capacity" => match self.list_type {
                ListType::Safe => Some(Box::new(tasm_lib::list::safe_u32::new::SafeNew(
                    tasm_type.unwrap(),
                ))),
                ListType::Unsafe => Some(Box::new(tasm_lib::list::unsafe_u32::new::UnsafeNew(
                    tasm_type.unwrap(),
                ))),
            },
            "push" => match self.list_type {
                ListType::Safe => Some(Box::new(tasm_lib::list::safe_u32::push::SafePush(
                    tasm_type.unwrap(),
                ))),
                ListType::Unsafe => Some(Box::new(tasm_lib::list::unsafe_u32::push::UnsafePush(
                    tasm_type.unwrap(),
                ))),
            },
            "pop" => match self.list_type {
                ListType::Safe => Some(Box::new(tasm_lib::list::safe_u32::pop::SafePop(
                    tasm_type.unwrap(),
                ))),
                ListType::Unsafe => Some(Box::new(tasm_lib::list::unsafe_u32::pop::UnsafePop(
                    tasm_type.unwrap(),
                ))),
            },
            "len" => match self.list_type {
                ListType::Safe => Some(Box::new(tasm_lib::list::safe_u32::length::SafeLength(
                    tasm_type.unwrap(),
                ))),
                ListType::Unsafe => Some(Box::new(
                    tasm_lib::list::unsafe_u32::length::UnsafeLength(tasm_type.unwrap()),
                )),
            },
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
                    list_type: self.list_type.into(),
                    f: tasm_lib::list::higher_order::inner_function::InnerFunction::NoFunctionBody(
                        lnat,
                    ),
                }))
            }
            _ => None,
        }
    }
}
