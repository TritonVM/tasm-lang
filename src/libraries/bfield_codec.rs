use tasm_lib::memory::dyn_malloc;
use triton_vm::triton_asm;

use crate::{
    ast::{self},
    ast_types,
    graft::Graft,
};

use super::Library;

const ENCODE_METHOD_NAME: &str = "encode";

#[derive(Clone, Debug)]
pub struct BFieldCodecLib {
    pub list_type: ast_types::ListType,
}

impl BFieldCodecLib {
    fn encode_method_signature(
        &self,
        receiver_type: &crate::ast_types::DataType,
    ) -> ast::FnSignature {
        ast::FnSignature {
            name: ENCODE_METHOD_NAME.to_owned(),
            args: vec![ast_types::AbstractArgument::ValueArgument(
                ast_types::AbstractValueArg {
                    name: "value".to_owned(),
                    data_type: receiver_type.to_owned(),
                    mutable: false,
                },
            )],
            output: ast_types::DataType::List(Box::new(ast_types::DataType::Bfe), self.list_type),
            arg_evaluation_order: Default::default(),
        }
    }
}

impl Library for BFieldCodecLib {
    fn get_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn get_method_name(
        &self,
        method_name: &str,
        receiver_type: &crate::ast_types::DataType,
    ) -> Option<String> {
        if method_name == ENCODE_METHOD_NAME {
            // For now, we only allow `encode` to be called on values
            // with a statically known length.
            if receiver_type.bfield_codec_length().is_some() {
                return Some(method_name.to_owned());
            } else {
                panic!(".encode() can only be called on values with a statically known length. Got: {receiver_type:#?}");
            }
        }

        None
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        receiver_type: &crate::ast_types::DataType,
        _args: &[crate::ast::Expr<super::Annotation>],
        _type_checker_state: &crate::type_checker::CheckState,
    ) -> crate::ast::FnSignature {
        if method_name == ENCODE_METHOD_NAME && receiver_type.bfield_codec_length().is_some() {
            self.encode_method_signature(receiver_type)
        } else {
            panic!("Unknown method in BFieldCodecLib. Got: {method_name} on receiver_type: {receiver_type}");
        }
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<crate::ast_types::DataType>,
        _args: &[crate::ast::Expr<super::Annotation>],
    ) -> crate::ast::FnSignature {
        todo!()
    }

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &crate::ast_types::DataType,
        _args: &[crate::ast::Expr<super::Annotation>],
        state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        if !(method_name == ENCODE_METHOD_NAME && receiver_type.bfield_codec_length().is_some()) {
            panic!("Unknown method in BFieldCodecLib. Got: {method_name} on receiver_type: {receiver_type}");
        }

        let value_size = receiver_type.bfield_codec_length().unwrap();
        let list_size_in_memory = (value_size + self.list_type.metadata_size()) as i32;

        // 1. Create a new list with the appropriate capacity
        // 2. Maybe
        let dyn_malloc_label = state.import_snippet(Box::new(dyn_malloc::DynMalloc));

        let write_capacity = match self.list_type {
            ast_types::ListType::Safe => {
                triton_asm!(
                    // _ *list
                    push 1 add
                    // _ (*list + 1)
                    push {value_size}
                    // _ (*list + 1) value_size

                    write_mem
                    // _ (*list + 1)

                    push -1 add
                    // _ *list
                )
            }
            ast_types::ListType::Unsafe => triton_asm!(),
        };
        let write_words_to_list = "swap 1 write_mem push 1 add\n".repeat(value_size);

        let encode_subroutine_label =
            format!("{method_name}_{}", receiver_type.label_friendly_name());
        let encode_subroutine_code = triton_asm!(
                {encode_subroutine_label}:
                    // _ [value]

                    push {list_size_in_memory}
                    call {dyn_malloc_label}
                    // _ [value] *list

                    // write length
                    push {value_size}
                    write_mem
                    // _ [value] *list

                    {&write_capacity}
                    // _ [value] *list

                    push {self.list_type.metadata_size()}
                    add
                    // _ [value] *word_0

                    {write_words_to_list}

                    // _ *word_{value_size}

                    push {-list_size_in_memory}
                    add

                    // _ *list
                    return
        );

        state.add_library_function(encode_subroutine_code.try_into().unwrap());

        triton_asm!(call {
            encode_subroutine_label
        })
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<crate::ast_types::DataType>,
        _args: &[crate::ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        todo!()
    }

    fn get_graft_function_name(&self, full_name: &str) -> Option<String> {
        if full_name.contains("::decode") {
            Some(full_name.to_owned())
        } else {
            None
        }
    }

    fn graft_function(
        &self,
        graft_config: &mut Graft,
        fn_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        function_type_parameter: Option<ast_types::DataType>,
    ) -> Option<crate::ast::Expr<super::Annotation>> {
        /// Handle the entire T::decode(...) grafting. Does not handle any appended `unwrap`.
        /// Expects the `decode` expression to be `T::decode(&tasm::load_from_memory(BFieldElement::new(x)))`
        /// Extracts the `x` from the above expression and returns it as a literal.
        fn handle_decode(
            graft_config: &mut Graft,
            fn_name: &str,
            args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
            function_type_parameter: Option<ast_types::DataType>,
            list_type: ast_types::ListType,
        ) -> crate::ast::Expr<super::Annotation> {
            // Fetch the returned type
            let split_fn_name: Vec<_> = fn_name.split("::").collect();
            assert_eq!(
                2,
                split_fn_name.len(),
                "Can only handle pattern T::decode. Got: {fn_name}"
            );
            let return_type = split_fn_name[0].to_owned();

            // TODO: This is not very elegant! Can only handle `Vec<T>` and declared structs.
            let mem_pointer_declared_type = if return_type == "Vec" {
                match function_type_parameter {
                    Some(t) => ast_types::DataType::List(Box::new(t), list_type),
                    None => panic!("Expected type parameter for Vec<T> in `decode` function"),
                }
            } else {
                ast_types::DataType::Unresolved(return_type)
            };

            let decode_arg = match args.len() {
                1 => args[0].clone(),
                _ => panic!("Argument list to `decode` function call must have length one. Got args:\n{args:#?}"),
            };

            let error_msg = format!("Expected T::decode(&tasm::load_from_memory(BFieldElement::new(<n>)))). Got: {decode_arg:#?}");
            let decode_arg = graft_config.graft_expr(&decode_arg);
            const LOAD_FROM_MEMORY_FN_NAME: &str = "tasm::load_from_memory";
            let pointer_to_struct = match decode_arg {
                ast::Expr::Unary(ast::UnaryOp::Ref(false), inner_expr, _) => {
                    match *inner_expr {
                        ast::Expr::FnCall(ast::FnCall {
                            name,
                            args: load_function_args,
                            type_parameter: _,
                            arg_evaluation_order: _,
                            annot: _,
                        }) => {
                            assert_eq!(LOAD_FROM_MEMORY_FN_NAME, name, "{error_msg}");
                            assert_eq!(1, load_function_args.len(), "{error_msg}");
                            match &load_function_args[0] {
                                // TODO: Maybe `address` can be an expression and doesn't have to be a literal?
                                // Do we need or want that?
                                ast::Expr::Lit(ast::ExprLit::Bfe(address_value)) => address_value.to_owned(),
                                _ => panic!("Argument to {LOAD_FROM_MEMORY_FN_NAME} must be known at compile time and must be a BFieldElement"),
                            }
                        }
                        _ => panic!("{error_msg}"),
                    }
                }
                _ => panic!("{error_msg}"),
            };

            let ret: ast::Expr<super::Annotation> =
                ast::Expr::Lit(ast::ExprLit::MemPointer(ast::MemPointerLiteral {
                    mem_pointer_address: pointer_to_struct,
                    mem_pointer_declared_type,
                    // `resolved_type` is to be filled out by the type checker
                    resolved_type: Default::default(),
                }));
            ret
        }

        if fn_name.contains("::decode") {
            return Some(handle_decode(
                graft_config,
                fn_name,
                args,
                function_type_parameter,
                self.list_type,
            ));
        }

        None
    }

    fn graft_method(
        &self,
        graft_config: &mut Graft,
        rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        fn handle_unwrap(
            graft_config: &mut Graft,
            rust_method_call: &syn::ExprMethodCall,
        ) -> Option<ast::Expr<super::Annotation>> {
            match rust_method_call.receiver.as_ref() {
                syn::Expr::Call(ca) => {
                    let preceding_function_call = graft_config.graft_call_exp(ca);
                    if matches!(
                        preceding_function_call,
                        ast::Expr::Lit(ast::ExprLit::MemPointer(_))
                    ) {
                        Some(preceding_function_call)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }

        // Remove `unwrap` if method call is `T::decode(...).unwrap()`
        const UNWRAP_NAME: &str = "unwrap";
        let last_method_name = rust_method_call.method.to_string();
        if last_method_name == UNWRAP_NAME {
            return handle_unwrap(graft_config, rust_method_call);
        }

        None
    }
}
