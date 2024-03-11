use itertools::Itertools;
use tasm_lib::list::LIST_METADATA_SIZE;
use tasm_lib::memory::dyn_malloc;
use tasm_lib::triton_vm::prelude::*;

use crate::ast;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::composite_types::CompositeTypes;
use crate::graft::Graft;
use crate::tasm_code_generator::write_n_words_to_memory_leaving_address;
use crate::tasm_code_generator::CompilerState;

use super::Library;

const ENCODE_METHOD_NAME: &str = "encode";
const LOAD_FROM_MEMORY_FN_NAME: &str = "tasm::load_from_memory";
const DECODE_FROM_MEMORY_FN_NAME: &str = "bfield_codec::decode_from_memory";
const DECODE_FROM_MEMORY_USING_SIZE_FN_NAME: &str = "bfield_codec::decode_from_memory_using_size";

#[derive(Clone, Debug)]
pub(crate) struct BFieldCodecLib;

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
            output: ast_types::DataType::List(Box::new(ast_types::DataType::Bfe)),
            arg_evaluation_order: Default::default(),
        }
    }

    fn encode_method(
        &self,
        method_name: &str,
        receiver_type: &DataType,
        state: &mut CompilerState,
    ) -> (String, Vec<LabelledInstruction>) {
        let encoding_length = receiver_type.bfield_codec_static_length().unwrap();
        let list_size_in_memory = (LIST_METADATA_SIZE + encoding_length) as i32;

        let dyn_malloc_label = state.import_snippet(Box::new(dyn_malloc::DynMalloc));

        let encode_subroutine_label =
            format!("{method_name}_{}", receiver_type.label_friendly_name());
        let encode_subroutine_code = triton_asm!(
                {encode_subroutine_label}:
                                    // _ [value]

                    call {dyn_malloc_label}
                                    // _ [value] *list

                    // write length
                    push {encoding_length}
                    swap 1
                    write_mem 1     // _ [value] (*list + 1)
                                    // _ [value] *word_0

                    {&write_n_words_to_memory_leaving_address(encoding_length)}
                                    // _ (*last_word + 1)

                    push {-list_size_in_memory}
                    add             // _ *list
                    return
        );
        (encode_subroutine_label, encode_subroutine_code)
    }

    /// Handle the entire T::decode(...) grafting. Does not handle any appended `unwrap`.
    /// Expects the `decode` expression to be
    /// `T::decode(&tasm::load_from_memory(BFieldElement::new(x)))`.
    /// Extracts the `x` from the above expression and returns it as a literal.
    fn handle_decode(
        graft_config: &mut Graft,
        fn_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        function_type_parameter: Option<ast_types::DataType>,
    ) -> crate::ast::Expr<super::Annotation> {
        // Fetch the returned type
        let split_fn_name: Vec<_> = fn_name.split("::").collect();
        let [return_type, _] = split_fn_name[..] else {
            panic!("Can only handle pattern T::decode. Got: {fn_name}");
        };

        // TODO: This is not very elegant! Can only handle `Vec<T>` and declared structs.
        let mem_pointer_declared_type = if return_type == "Vec" {
            let Some(vec_element_type) = function_type_parameter else {
                panic!("Expected type parameter for Vec<T> in `decode` function");
            };
            ast_types::DataType::List(Box::new(vec_element_type))
        } else {
            ast_types::DataType::Unresolved(return_type.to_owned())
        };

        let [decode_arg] = args.iter().collect_vec()[..] else {
            panic!("`decode` requires exactly one argument. Got args:\n{args:#?}");
        };

        let error_msg = format!(
            "Expected T::decode(&tasm::load_from_memory(BFieldElement::new(<n>)))). \
            Got: {decode_arg:#?}"
        );
        let decode_arg = graft_config.graft_expr(decode_arg);
        let ast::Expr::Unary(ast::UnaryOp::Ref(false), inner_expr, _) = decode_arg else {
            panic!("{error_msg}");
        };
        let ast::Expr::FnCall(ast::FnCall {
            name,
            args: load_function_args,
            ..
        }) = *inner_expr
        else {
            panic!("{error_msg}");
        };

        assert_eq!(LOAD_FROM_MEMORY_FN_NAME, name, "{error_msg}");
        assert_eq!(1, load_function_args.len(), "{error_msg}");

        ast::Expr::MemoryLocation(ast::MemPointerExpression {
            mem_pointer_address: Box::new(load_function_args[0].to_owned()),
            mem_pointer_declared_type,
            // `resolved_type` is to be filled out by the type checker
            resolved_type: Default::default(),
        })
    }

    fn handle_decode_from_mem(
        graft_config: &mut Graft,
        fn_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        function_type_parameter: Option<ast_types::DataType>,
    ) -> crate::ast::Expr<super::Annotation> {
        assert!(matches!(
            fn_name,
            DECODE_FROM_MEMORY_FN_NAME | DECODE_FROM_MEMORY_USING_SIZE_FN_NAME
        ));
        let memory_address = match args.iter().collect_vec()[..] {
            [address] => address,
            [address, _] => address,
            _ => panic!("wrong number of arguments to `{fn_name}`"),
        };
        let memory_address = graft_config.graft_expr(memory_address);

        let Some(mem_pointer_declared_type) = function_type_parameter else {
            panic!("function `{fn_name}` needs explicit generic type");
        };

        let memory_expression = ast::MemPointerExpression {
            mem_pointer_address: Box::new(memory_address),
            mem_pointer_declared_type,
            resolved_type: Default::default(),
        };

        ast::Expr::MemoryLocation(memory_expression)
    }
}

impl Library for BFieldCodecLib {
    fn graft_type(
        &self,
        _graft: &mut Graft,
        _rust_type_as_string: &str,
        _path_args: &syn::PathArguments,
    ) -> Option<ast_types::DataType> {
        None
    }

    fn handle_function_call(
        &self,
        _full_name: &str,
        _qualified_self_type: &Option<DataType>,
    ) -> bool {
        false
    }

    fn handle_method_call(
        &self,
        method_name: &str,
        receiver_type: &crate::ast_types::DataType,
    ) -> bool {
        if method_name != ENCODE_METHOD_NAME {
            return false;
        }

        if receiver_type.bfield_codec_static_length().is_none() {
            panic!(
                ".encode() can only be called on values with a statically known length. \
                    Got:  {receiver_type:#?}"
            );
        }

        true
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        receiver_type: &crate::ast_types::DataType,
        _args: &[crate::ast::Expr<super::Annotation>],
        _type_checker_state: &crate::type_checker::CheckState,
    ) -> crate::ast::FnSignature {
        if method_name != ENCODE_METHOD_NAME || receiver_type.bfield_codec_static_length().is_none()
        {
            panic!(
                "Unknown method in BFieldCodecLib. \
                Got: {method_name} on receiver_type: {receiver_type}"
            );
        }

        self.encode_method_signature(receiver_type)
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<crate::ast_types::DataType>,
        _args: &[crate::ast::Expr<super::Annotation>],
        _qualified_self_type: &Option<DataType>,
        _composite_types: &mut CompositeTypes,
    ) -> crate::ast::FnSignature {
        todo!()
    }

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &crate::ast_types::DataType,
        _args: &[crate::ast::Expr<super::Annotation>],
        state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<LabelledInstruction> {
        if method_name != ENCODE_METHOD_NAME || receiver_type.bfield_codec_static_length().is_none()
        {
            panic!(
                "Unknown method in BFieldCodecLib. \
                Got: {method_name} on receiver_type: {receiver_type}"
            );
        }

        let (encode_subroutine_label, encode_subroutine_code) =
            self.encode_method(method_name, receiver_type, state);

        state.add_subroutine(encode_subroutine_code.try_into().unwrap());

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
        _qualified_self_type: &Option<DataType>,
    ) -> Vec<LabelledInstruction> {
        todo!()
    }

    fn get_graft_function_name(&self, full_name: &str) -> Option<String> {
        match full_name {
            name if name.starts_with("bfield_codec::") => Some(name.to_owned()),
            name if name.contains("::decode") => Some(name.to_owned()),
            _ => None,
        }
    }

    fn graft_function(
        &self,
        graft_config: &mut Graft,
        fn_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        function_type_parameter: Option<ast_types::DataType>,
    ) -> Option<crate::ast::Expr<super::Annotation>> {
        if fn_name.starts_with("bfield_codec::") {
            let decode_expr =
                Self::handle_decode_from_mem(graft_config, fn_name, args, function_type_parameter);
            return Some(decode_expr);
        }
        if fn_name.contains("::decode") {
            let decode_expr =
                Self::handle_decode(graft_config, fn_name, args, function_type_parameter);
            return Some(decode_expr);
        }

        None
    }

    fn graft_method_call(
        &self,
        graft_config: &mut Graft,
        rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        fn handle_unwrap(
            graft_config: &mut Graft,
            rust_method_call: &syn::ExprMethodCall,
        ) -> Option<ast::Expr<super::Annotation>> {
            let syn::Expr::Call(ca) = rust_method_call.receiver.as_ref() else {
                return None;
            };

            let preceding_function_call = graft_config.graft_call_exp(ca);
            let ast::Expr::MemoryLocation(_) = preceding_function_call else {
                return None;
            };

            Some(preceding_function_call)
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
