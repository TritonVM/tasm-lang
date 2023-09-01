use crate::{ast, graft::Graft};

use super::Library;

#[derive(Clone, Debug)]
pub struct BFieldCodecLib;

impl Library for BFieldCodecLib {
    fn get_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn get_method_name(
        &self,
        _method_name: &str,
        _receiver_type: &crate::ast_types::DataType,
    ) -> Option<String> {
        None
    }

    fn method_name_to_signature(
        &self,
        _fn_name: &str,
        _receiver_type: &crate::ast_types::DataType,
        _args: &[crate::ast::Expr<super::Annotation>],
    ) -> crate::ast::FnSignature {
        panic!("BFieldCodecLib does not contain any methods")
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
        _method_name: &str,
        _receiver_type: &crate::ast_types::DataType,
        _args: &[crate::ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        todo!()
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
        graft_config: &Graft,
        fn_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
    ) -> Option<crate::ast::Expr<super::Annotation>> {
        /// Handle the entire T::decode(...) grafting. Does not handle any appended `unwrap`.
        /// Expects the `decode` expression to be `T::decode(&tasm::load_from_memory(BFieldElement::new(x)))`
        /// Extracts the `x` from the above expression and returns it as a literal.
        fn handle_decode(
            graft_config: &Graft,
            fn_name: &str,
            args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        ) -> crate::ast::Expr<super::Annotation> {
            // Fetch the returned type
            let split_fn_name: Vec<_> = fn_name.split("::").collect();
            assert_eq!(
                2,
                split_fn_name.len(),
                "Can only handle pattern T::decode. Got: {fn_name}"
            );
            let return_type = split_fn_name[0].to_owned();

            let decode_arg = match args.len() {
                1 => args[0].clone(),
                _ => panic!("Argument list to `decode` function call must have length one. Got args:\n{args:#?}"),
            };

            let error_msg = format!("Expected T::decode(tasm::load_from_memory(BFieldElement::new(<n>)))). Got: {decode_arg:#?}");
            let decode_arg = graft_config.graft_expr(&decode_arg);
            const LOAD_FROM_MEMORY_FN_NAME: &str = "tasm::load_from_memory";
            let pointer_to_struct = match decode_arg {
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
                        ast::Expr::Lit(ast::ExprLit::BFE(address_value)) => address_value.to_owned(),
                        _ => panic!("Argument to {LOAD_FROM_MEMORY_FN_NAME} must be known at compile time and must be a BFieldElement"),
                    }
                }
                _ => panic!("{error_msg}"),
            };

            let ret: ast::Expr<super::Annotation> =
                ast::Expr::Lit(ast::ExprLit::MemPointer(ast::MemPointerLiteral {
                    mem_pointer_address: pointer_to_struct,
                    struct_name: return_type,
                    // `resolved_type` is to be filled out by the type checker
                    resolved_type: Default::default(),
                }));
            ret
        }

        if fn_name.contains("::decode") {
            return Some(handle_decode(graft_config, fn_name, args));
        }

        None
    }

    fn graft_method(
        &self,
        graft_config: &Graft,
        rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        fn handle_unwrap(
            graft_config: &Graft,
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
