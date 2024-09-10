use itertools::Itertools;
use num::One;
use tasm_lib::memory::memcpy::MemCpy;
use tasm_lib::traits::basic_snippet::BasicSnippet;
use tasm_lib::triton_vm::prelude::triton_asm;
use tasm_lib::triton_vm::prelude::LabelledInstruction;

use crate::ast;
use crate::ast::FnSignature;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::composite_types::CompositeTypes;
use crate::graft::Graft;
use crate::tasm_code_generator::CompilerState;
use crate::type_checker::GetType;

use super::Library;
use super::LibraryFunction;

const VEC_DATA_TYPE_NAME: &str = "Vec";
const VECTOR_LIB_INDICATOR: &str = "Vec::";
const NEW_FUNCTION_NAME: &str = "new";
const DEFAULT_FUNCTION_NAME: &str = "default";
const CLONE_FROM_METHOD_NAME: &str = "clone_from";
const PUSH_METHOD_NAME: &str = "push";
const POP_METHOD_NAME: &str = "pop";
const LEN_METHOD_NAME: &str = "len";
const MAP_METHOD_NAME: &str = "map";
const CLEAR_METHOD_NAME: &str = "clear";
const SPLIT_OFF_METHOD_NAME: &str = "split_off";

#[derive(Clone, Debug)]
pub(crate) struct VectorLib;

impl Library for VectorLib {
    fn graft_type(
        &self,
        graft: &mut Graft,
        rust_type_as_string: &str,
        path_args: &syn::PathArguments,
    ) -> Option<DataType> {
        match rust_type_as_string {
            VEC_DATA_TYPE_NAME => Some(Self::rust_vec_to_data_type(graft, path_args)),
            _ => None,
        }
    }

    fn handle_function_call(
        &self,
        full_name: &str,
        _qualified_self_type: &Option<DataType>,
    ) -> bool {
        full_name.starts_with(VECTOR_LIB_INDICATOR)
    }

    fn handle_method_call(&self, method_name: &str, receiver_type: &ast_types::DataType) -> bool {
        matches!(receiver_type, ast_types::DataType::List(_))
            && matches!(
                method_name,
                PUSH_METHOD_NAME
                    | POP_METHOD_NAME
                    | LEN_METHOD_NAME
                    | MAP_METHOD_NAME
                    | CLEAR_METHOD_NAME
                    | CLONE_FROM_METHOD_NAME
                    | SPLIT_OFF_METHOD_NAME
            )
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
        args: &[ast::Expr<super::Annotation>],
        type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature {
        let ast_types::DataType::List(element_type) = receiver_type else {
            panic!("Receiver type must be `List`");
        };

        // Special-case on `map` as we need to dig into the type checker state to find the
        // function signature.
        match method_name {
            MAP_METHOD_NAME => self.fn_signature_for_map(args, type_checker_state),
            CLEAR_METHOD_NAME => self.clear_method(element_type).signature,
            CLONE_FROM_METHOD_NAME => self.clone_from_method_signature(element_type),
            PUSH_METHOD_NAME => self.push_method_signature(element_type),
            LEN_METHOD_NAME => self.len_method_signature(element_type),
            SPLIT_OFF_METHOD_NAME => ast::FnSignature::from_basic_snippet(Box::new(
                tasm_lib::list::split_off::SplitOff {
                    element_type: (*element_type.to_owned()).try_into().unwrap(),
                },
            )),
            POP_METHOD_NAME => {
                let snippet = Self::pop_snippet(element_type);
                FnSignature::from_basic_snippet(snippet)
            }
            _ => panic!(),
        }
    }

    fn function_name_to_signature(
        &self,
        full_name: &str,
        type_parameter: Option<ast_types::DataType>,
        args: &[ast::Expr<super::Annotation>],
        _qualified_self_type: &Option<DataType>,
        _composite_types: &mut CompositeTypes,
    ) -> ast::FnSignature {
        let stripped_name = &full_name[VECTOR_LIB_INDICATOR.len()..full_name.len()];
        let snippet = self
            .name_to_tasm_lib_snippet(stripped_name, &type_parameter, args)
            .unwrap_or_else(|| panic!("Unknown function name {stripped_name}"));

        FnSignature::from_basic_snippet(snippet)
    }

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
        args: &[ast::Expr<super::Annotation>],
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        let ast_types::DataType::List(element_type) = receiver_type else {
            panic!(
                "Cannot call vector method without type param. Got receiver_type: {receiver_type}"
            )
        };

        match method_name {
            CLEAR_METHOD_NAME => return self.clear_method(element_type).body,
            CLONE_FROM_METHOD_NAME => return Self::clone_from_method_body(element_type, state),
            PUSH_METHOD_NAME => return self.push_method_body(element_type, state),
            LEN_METHOD_NAME => return self.len_method_body(),
            _ => (),
        };

        // find inner function if needed
        let snippet = self
            .name_to_tasm_lib_snippet(method_name, &Some(*element_type.to_owned()), args)
            .unwrap_or_else(|| panic!("Unknown function name {method_name}"));
        let entrypoint = snippet.entrypoint();
        state.import_snippet(snippet);

        triton_asm!(call { entrypoint })
    }

    fn call_function(
        &self,
        full_name: &str,
        type_parameter: Option<ast_types::DataType>,
        args: &[ast::Expr<super::Annotation>],
        state: &mut CompilerState,
        _qualified_self_type: &Option<DataType>,
    ) -> Vec<LabelledInstruction> {
        let stripped_name = &full_name[VECTOR_LIB_INDICATOR.len()..full_name.len()];
        let snippet = self
            .name_to_tasm_lib_snippet(stripped_name, &type_parameter, args)
            .unwrap_or_else(|| panic!("Unknown function name {full_name}"));
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

    fn graft_method_call(
        &self,
        graft_config: &mut Graft,
        rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        const COLLECT_VEC_NAME: &str = "collect_vec";
        const UNWRAP_NAME: &str = "unwrap";
        const INTO_ITER_NAME: &str = "into_iter";

        let last_method_name = rust_method_call.method.to_string();

        match last_method_name.as_str() {
            UNWRAP_NAME => {
                // Handle `a.pop().unwrap();` or `a.try_into().unwrap()`
                match rust_method_call.receiver.as_ref() {
                    syn::Expr::MethodCall(rust_inner_method_call) => {
                        let inner_method_call =
                            graft_config.graft_method_call(rust_inner_method_call);
                        let inner_method_call = match inner_method_call {
                            ast::Expr::MethodCall(mc) => mc,
                            _ => return None,
                        };
                        if inner_method_call.method_name != POP_METHOD_NAME {
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
                            method_name: POP_METHOD_NAME.to_owned(),
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
                        if inner_method_call.method_name != MAP_METHOD_NAME {
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
                            method_name: MAP_METHOD_NAME.to_owned(),
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
    /// BEFORE: _ *list
    /// AFTER: _ list_size_in_memory
    pub(crate) fn list_encoding_size_code(
        elem_type: &ast_types::DataType,
    ) -> Vec<LabelledInstruction> {
        if let Some(static_size) = elem_type.bfield_codec_static_length() {
            triton_asm!(
                // _ *list

                read_mem 1
                pop 1
                // _ list_len

                push {static_size}
                mul
                addi 1
                // _ list_size

                push {ast_types::DataType::MAX_DYN_FIELD_SIZE}
                dup 1
                lt
                // _ list_size (list_size < max_size)

                assert

                // _ list_size
            )
        } else {
            todo!("Length-reading of list with dynamically-sized elements not yet supported");
        }
    }

    fn rust_vec_to_data_type(
        graft: &mut Graft,
        path_args: &syn::PathArguments,
    ) -> ast_types::DataType {
        match path_args {
            syn::PathArguments::AngleBracketed(ab) => {
                assert_eq!(1, ab.args.len(), "Must be Vec<T> for *one* generic T.");
                match &ab.args[0] {
                    syn::GenericArgument::Type(element_type) => ast_types::DataType::List(
                        Box::new(graft.syn_type_to_ast_type(element_type)),
                    ),
                    other => panic!("Unsupported type {other:#?}"),
                }
            }
            other => panic!("Unsupported type {other:#?}"),
        }
    }

    /// Defines the `a.clear()` method that can be called
    /// on a vector, resulting in an empty vector. Compatible with
    /// Rust's `clear` method on `Vec<T>`.
    fn clear_method(&self, element_type: &ast_types::DataType) -> LibraryFunction {
        let signature = ast::FnSignature::value_function_with_mutable_args(
            CLEAR_METHOD_NAME,
            vec![(
                "list",
                ast_types::DataType::List(Box::new(element_type.to_owned())),
            )],
            ast_types::DataType::unit(),
        );

        LibraryFunction {
            signature,
            // Stack at function start:
            // _ *list
            // Stack at function end:
            // _
            body: triton_asm!(push 0 swap 1 write_mem 1 pop 1),
        }
    }

    /// Returns the signature of the method `map`
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
            name: String::from(MAP_METHOD_NAME),
            // TODO: Use List<inner_fn_signature-args> here instead for betetr type checking
            args: vec![vector_as_arg, derived_inner_function_as_function_arg],
            output: ast_types::DataType::List(Box::new(inner_output.to_owned())),
            arg_evaluation_order: Default::default(),
        }
    }

    /// Returns the function signature of the method `clone_from`:
    fn clone_from_method_signature(&self, element_type: &ast_types::DataType) -> ast::FnSignature {
        let self_type = ast_types::DataType::List(Box::new(element_type.to_owned()));
        let self_as_arg = ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
            name: "self".to_owned(),
            data_type: self_type.clone(),
            mutable: true,
        });
        let source_type = ast_types::DataType::Boxed(Box::new(self_type.clone()));
        let source_as_arg =
            ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
                name: "other".to_owned(),
                data_type: source_type.clone(),
                mutable: false,
            });
        ast::FnSignature {
            name: CLONE_FROM_METHOD_NAME.to_owned(),
            args: vec![self_as_arg, source_as_arg],
            output: ast_types::DataType::unit(),
            arg_evaluation_order: Default::default(),
        }
    }

    /// Returns the code to copy a list from src into dest. Does not allocate.
    /// ```text
    /// BEFORE: _ *dest *src
    /// AFTER: _
    /// ```
    fn clone_from_method_body(
        element_type: &ast_types::DataType,
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        let element_size = element_type.stack_size();
        let memcpy_label = state.import_snippet(Box::new(MemCpy));
        triton_asm!(
            // _ *dest *src

            read_mem 1
            // _ *dest src_len (*src - 1)

            push 1 add
            // _ *dest src_len *src

            swap 2
            swap 1
            // _ *src *dest src_len

            push {element_size}
            mul
            // _ *src *dest src_elements_size

            push 1
            add
            // _ *src *dest total_list_size

            call {memcpy_label}
            // _
        )
    }

    fn push_method_signature(&self, element_type: &ast_types::DataType) -> ast::FnSignature {
        ast::FnSignature::from_basic_snippet(Self::push_snippet(element_type))
    }

    fn push_method_body(
        &self,
        element_type: &ast_types::DataType,
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        let entrypoint = if element_type.is_copy() {
            let snippet = Self::push_snippet(element_type);
            let entrypoint = snippet.entrypoint();
            state.import_snippet(snippet);
            entrypoint
        } else {
            // In this case, we would have to encode in accordance with `BFieldCodec`
            // which is currently very expensive as each element needs to be reallocated
            // *and* we would have to find the next free word in the vector data structure
            // in memory.
            unimplemented!("Can only construct vectors whose element type is `Copy`.");
        };

        triton_asm!(call { entrypoint })
    }

    fn new_list_snippet(element_type: &ast_types::DataType) -> Box<dyn BasicSnippet> {
        let tasm_type: tasm_lib::data_type::DataType = element_type.to_owned().try_into().unwrap();
        Box::new(tasm_lib::list::new::New::new(tasm_type))
    }

    fn push_snippet(element_type: &ast_types::DataType) -> Box<dyn BasicSnippet> {
        let tasm_type: tasm_lib::data_type::DataType = element_type.to_owned().try_into().unwrap();
        Box::new(tasm_lib::list::push::Push::new(tasm_type))
    }

    fn pop_snippet(element_type: &ast_types::DataType) -> Box<dyn BasicSnippet> {
        let tasm_type: tasm_lib::data_type::DataType = element_type.to_owned().try_into().unwrap();
        Box::new(tasm_lib::list::pop::Pop::new(tasm_type))
    }

    fn len_method_signature(&self, element_type: &ast_types::DataType) -> ast::FnSignature {
        let self_type = ast_types::DataType::List(Box::new(element_type.to_owned()));
        let self_as_arg = ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
            name: "self".to_owned(),
            data_type: self_type.clone(),
            mutable: false,
        });
        ast::FnSignature {
            name: LEN_METHOD_NAME.to_owned(),
            args: vec![self_as_arg],
            output: ast_types::DataType::U32,
            arg_evaluation_order: Default::default(),
        }
    }

    fn len_method_body(&self) -> Vec<LabelledInstruction> {
        triton_asm!(
            // _ *list
            read_mem 1
            // _ length (*list - 1)

            pop 1
            // _ length
        )
    }

    fn name_to_tasm_lib_snippet(
        &self,
        public_name: &str,
        type_parameter: &Option<ast_types::DataType>,
        args: &[ast::Expr<super::Annotation>],
    ) -> Option<Box<dyn BasicSnippet>> {
        match public_name {
            SPLIT_OFF_METHOD_NAME => Some(Box::new(tasm_lib::list::split_off::SplitOff {
                element_type: type_parameter
                    .as_ref()
                    .unwrap()
                    .to_owned()
                    .try_into()
                    .unwrap(),
            })),
            NEW_FUNCTION_NAME | DEFAULT_FUNCTION_NAME => {
                let type_parameter = type_parameter.as_ref().unwrap_or_else(|| panic!("Type parameter must be set when initializing a new vector.\nUse `Vec::<T>::{public_name}()` instead of Vec::{public_name}()"));
                Some(Self::new_list_snippet(type_parameter))
            }
            POP_METHOD_NAME => Some(Self::pop_snippet(type_parameter.as_ref().unwrap())),
            MAP_METHOD_NAME => {
                let ast_types::DataType::Function(inner_function_type) = args[1].get_type() else {
                    panic!()
                };
                let ast::Expr::Var(inner_function_name) = &args[1] else {
                    panic!()
                };

                let lnat = tasm_lib::list::higher_order::inner_function::NoFunctionBody {
                    label_name: inner_function_name.to_string(),
                    input_type: inner_function_type.input_argument.try_into().unwrap(),
                    output_type: inner_function_type.output.try_into().unwrap(),
                };
                Some(Box::new(tasm_lib::list::higher_order::map::Map {
                    f: tasm_lib::list::higher_order::inner_function::InnerFunction::NoFunctionBody(
                        lnat,
                    ),
                }))
            }
            _ => None,
        }
    }
}
