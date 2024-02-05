use std::str::FromStr;

use itertools::Itertools;
use strum::IntoEnumIterator;
use syn::{parse_quote, Data};
use tasm_lib::traits::basic_snippet::BasicSnippet;
use tasm_lib::triton_vm::proof_item::ProofItemVariant;
use tasm_lib::triton_vm::triton_asm;

use crate::ast::RoutineBody;
use crate::ast::{self, FnSignature};
use crate::ast_types::{AbstractArgument, AbstractValueArg, DataType, StructType};
use crate::composite_types::TypeContext;
use crate::graft::Graft;
use crate::type_checker::Typing;

use super::Library;

const NEXT_AS_METHOD_NAMES_PREFIX: &str = "next_as_";

#[derive(Debug)]
pub(crate) struct VmProofIterLib;

impl Library for VmProofIterLib {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        None
    }

    fn get_method_name(
        &self,
        method_name: &str,
        receiver_type: &crate::ast_types::DataType,
    ) -> Option<String> {
        None
    }

    fn method_name_to_signature(
        &self,
        fn_name: &str,
        receiver_type: &crate::ast_types::DataType,
        args: &[ast::Expr<super::Annotation>],
        type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature {
        todo!()
    }

    fn function_name_to_signature(
        &self,
        fn_name: &str,
        type_parameter: Option<crate::ast_types::DataType>,
        args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        todo!()
    }

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &crate::ast_types::DataType,
        args: &[ast::Expr<super::Annotation>],
        state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<tasm_lib::prelude::triton_vm::prelude::LabelledInstruction> {
        todo!()
    }

    fn call_function(
        &self,
        fn_name: &str,
        type_parameter: Option<crate::ast_types::DataType>,
        args: &[ast::Expr<super::Annotation>],
        state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<tasm_lib::prelude::triton_vm::prelude::LabelledInstruction> {
        todo!()
    }

    fn get_graft_function_name(&self, full_name: &str) -> Option<String> {
        None
    }

    fn graft_function(
        &self,
        graft_config: &mut Graft,
        fn_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        type_parameter: Option<crate::ast_types::DataType>,
    ) -> Option<ast::Expr<super::Annotation>> {
        todo!()
    }

    fn graft_method_call(
        &self,
        graft_config: &mut Graft,
        rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        let method_name = rust_method_call.method.to_string();
        if !method_name.starts_with(NEXT_AS_METHOD_NAMES_PREFIX) {
            return None;
        }

        let all_method_names = all_next_as_method_names();
        if !all_method_names.contains(&method_name) {
            return None;
        }

        // If argument has already been stripped, do nothing. The below code strips the
        // argument which is assumed to be `&mut sponge_hasher`.
        if rust_method_call.args.is_empty() {
            return None;
        }

        // Verify that `args` looks as expected
        let [_arg] = rust_method_call.args.iter().collect_vec()[..] else {
            panic!(
                "{method_name} expects exactly one argument in addition to its receiver. Got: {:?}\nmethod call was:\n{:#?}",
                rust_method_call.args,
                rust_method_call
            );
        };

        let mut method_call_without_spongehasher_arg = rust_method_call.clone();
        method_call_without_spongehasher_arg.args.clear();

        Some(graft_config.graft_method_call(&method_call_without_spongehasher_arg))
    }
}

impl VmProofIterLib {
    pub(crate) fn vm_proof_iter_type(graft_config: &mut Graft) -> TypeContext {
        let struct_type = vm_proof_iter_as_struct_type(graft_config);

        // List all methods
        let all_dequeue_methods = all_next_as_methods(graft_config);
        TypeContext {
            composite_type: struct_type.into(),
            methods: all_dequeue_methods,
            associated_functions: vec![],
        }
    }

    pub(crate) fn fri_response_type(graft_config: &mut Graft) -> TypeContext {
        let struct_type = fri_response_as_struct_type(graft_config);

        TypeContext {
            composite_type: struct_type.into(),
            methods: vec![],
            associated_functions: vec![],
        }
    }
}

fn vm_proof_iter_as_struct_type(graft_config: &mut Graft) -> StructType {
    let tokens: syn::Item = parse_quote! {
        struct VmProofIter {
            current_item_pointer: BFieldElement,
        }
    };
    let syn::Item::Struct(item_struct) = tokens else {
        panic!()
    };

    graft_config.graft_struct_type(&item_struct)
}

fn fri_response_as_struct_type(graft_config: &mut Graft) -> StructType {
    let tokens: syn::Item = parse_quote! {
        struct FriResponse {
            /// The authentication structure of the Merkle tree.
            pub auth_structure: Vec<Digest>,
            /// The values of the opened leaves of the Merkle tree.
            pub revealed_leaves: Vec<XFieldElement>,
        }
    };
    let syn::Item::Struct(item_struct) = tokens else {
        panic!()
    };

    graft_config.graft_struct_type(&item_struct)
}

fn all_next_as_method_names() -> Vec<String> {
    ProofItemVariant::iter()
        .map(|x| method_name_for_next_as(&x))
        .collect()
}

fn method_name_for_next_as(variant: &ProofItemVariant) -> String {
    format!(
        "{NEXT_AS_METHOD_NAMES_PREFIX}{}",
        variant.to_string().to_lowercase()
    )
}

fn all_next_as_methods(graft_config: &mut Graft) -> Vec<ast::Method<Typing>> {
    let mut methods = vec![];
    let receiver_type: DataType = vm_proof_iter_as_struct_type(graft_config).into();
    let receiver_type = DataType::Boxed(Box::new(receiver_type));

    for variant in ProofItemVariant::iter() {
        let snippet = tasm_lib::recufier::proof_stream::dequeue_next_as::DequeueNextAs {
            proof_item: variant,
        };
        let method_output = variant.payload_type();
        let method_output =
            DataType::try_from_string(method_output, graft_config.list_type).unwrap();
        let method_output = DataType::Boxed(Box::new(method_output));
        let snippet_label = snippet.entrypoint();
        let payload_size_to_payload_pointer = if variant.payload_static_length().is_some() {
            triton_asm!()
        } else {
            triton_asm!(push 1 add)
        };
        let code = triton_asm!(
            call { snippet_label }
            {&payload_size_to_payload_pointer}
        );

        let method_signature = ast::FnSignature::value_function_with_mutable_args(
            &method_name_for_next_as(&variant),
            vec![("self", receiver_type.clone())],
            method_output,
        );
        let method = ast::Method {
            signature: method_signature,
            body: RoutineBody::Instructions(ast::AsmDefinedBody {
                dependencies: vec![snippet_label],
                instructions: code,
            }),
        };

        methods.push(method);
    }

    methods
}
