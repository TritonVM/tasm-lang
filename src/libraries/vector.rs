use itertools::Itertools;
use num::One;
use tasm_lib::traits::basic_snippet::BasicSnippet;
use triton_vm::triton_asm;

use crate::{
    ast,
    ast_types::{self, ListType},
    graft::Graft,
    tasm_code_generator::CompilerState,
    type_checker::GetType,
};

use super::{Library, LibraryFunction};

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
            if matches!(method_name, "push" | "pop" | "len" | "map" | "clear") {
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
        let element_type = if let ast_types::DataType::List(ety, _) = receiver_type {
            *ety.to_owned()
        } else {
            panic!("Receiver type must be `List`");
        };

        // Special-case on `map` as we need to dig into the type checker state to find the
        // function signature.
        if fn_name == "map" {
            return self.fn_signature_for_map(args, type_checker_state);
        }

        if fn_name == "clear" {
            return self.clear_method(&element_type).signature;
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
            0 => ast_types::DataType::Tuple(vec![].into()),
            _ => ast_types::DataType::Tuple(output_types.into()),
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
        let element_type: ast_types::DataType =
            if let ast_types::DataType::List(type_param, _list_type) = receiver_type {
                *type_param.to_owned()
            } else {
                panic!(
                "Cannot call vector method without type param. Got receiver_type: {receiver_type}"
            )
            };

        if method_name == "clear" {
            return self.clear_method(&element_type).body;
        }

        // find inner function if needed
        let snippet = self
            .name_to_tasm_lib_snippet(method_name, &Some(element_type), args)
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
        _graft_config: &mut Graft,
        _fn_name: &str,
        _args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        _function_type_parameter: Option<ast_types::DataType>,
    ) -> Option<ast::Expr<super::Annotation>> {
        todo!()
    }

    fn graft_method(
        &self,
        graft_config: &mut Graft,
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
                // Handle `a.pop().unwrap();`
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
                        Some(ast::Expr::MethodCall(ast::MethodCall {
                            method_name: POP_NAME.to_owned(),
                            args,
                            annot: Default::default(),
                            associated_type: None,
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

                        Some(ast::Expr::MethodCall(ast::MethodCall {
                            method_name: MAP_NAME.to_owned(),
                            args,
                            annot: Default::default(),
                            associated_type: None,
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
    /// Defines the `a.clear()` method that can be called
    /// on a vector, resulting in an empty vector. Compatible with
    /// Rust's `clear` method on `Vec<T>`.
    fn clear_method(&self, element_type: &ast_types::DataType) -> LibraryFunction {
        let fn_signature = ast::FnSignature {
            name: "clear".to_owned(),
            args: vec![ast_types::AbstractArgument::ValueArgument(
                ast_types::AbstractValueArg {
                    name: "list".to_owned(),
                    data_type: ast_types::DataType::List(
                        Box::new(element_type.to_owned()),
                        self.list_type,
                    ),
                    mutable: true,
                },
            )],
            output: ast_types::DataType::Tuple(vec![].into()),
            arg_evaluation_order: Default::default(),
        };

        LibraryFunction {
            signature: fn_signature,
            // Notice that we don't have to check capacity for safe lists, since
            // length 0 can never exceed capacity.
            // Stack at function start:
            // _ *list
            // Stack at function end:
            // _
            body: triton_asm!(push 0 write_mem pop),
        }
    }

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
        assert!(
            inner_fn_signature.len().is_one(),
            "Duplicate definition of function {inner_fn_name} observed"
        );
        let inner_fn_signature = &inner_fn_signature[0];
        let inner_output = &inner_fn_signature.output;
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
            output: ast_types::DataType::List(Box::new(inner_output.to_owned()), self.list_type),
            arg_evaluation_order: Default::default(),
        }
    }

    fn name_to_tasm_lib_snippet(
        &self,
        public_name: &str,
        type_parameter: &Option<ast_types::DataType>,
        args: &[ast::Expr<super::Annotation>],
    ) -> Option<Box<dyn BasicSnippet>> {
        match public_name {
            "default" => panic!("Change `Vec::default()` to `Vec::with_capacity(n)`."),
            "with_capacity" => {
                let data_type = type_parameter.clone().expect("Type parameter must be set when instantiating a vector: `Vec::<T>::with_capacity(n)`");
                Some(self.list_type.with_capacity_snippet(data_type))
            }
            "push" => Some(self.list_type.push_snippet(type_parameter.clone().unwrap())),
            "pop" => Some(self.list_type.pop_snippet(type_parameter.clone().unwrap())),
            "len" => Some(self.list_type.len_snippet(type_parameter.clone().unwrap())),
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
