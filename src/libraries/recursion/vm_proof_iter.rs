use strum::IntoEnumIterator;
use syn::parse_quote;
use tasm_lib::traits::basic_snippet::BasicSnippet;
use tasm_lib::triton_vm::proof_item::ProofItemVariant;

use crate::ast_types::StructType;
use crate::triton_vm::prelude::*;
use crate::{
    ast::{self, RoutineBody},
    ast_types::DataType,
    composite_types::TypeContext,
    graft::Graft,
    type_checker::Typing,
};

use super::RecursionLib;

pub(super) const NEXT_AS_METHOD_NAMES_PREFIX: &str = "next_as_";
pub(super) const VM_PROOF_ITER_TYPE_NAME: &str = "VmProofIter";

pub(super) fn graft_vm_proof_iter(graft: &mut Graft) -> DataType {
    let vm_proof_iter = vm_proof_iter_type(graft);
    graft
        .imported_custom_types
        .add_type_context_if_new(vm_proof_iter.clone());
    let fri_response = RecursionLib::fri_response_type(graft);
    graft
        .imported_custom_types
        .add_type_context_if_new(fri_response);
    vm_proof_iter.into()
}

fn vm_proof_iter_type(graft: &mut Graft) -> TypeContext {
    let struct_type = vm_proof_iter_as_struct_type(graft);

    // List all methods
    let all_dequeue_methods = all_next_as_methods(graft);
    let new_function_constructor = vm_proof_iter_new_constructor(graft);
    TypeContext {
        composite_type: struct_type.into(),
        methods: all_dequeue_methods,
        associated_functions: vec![new_function_constructor],
    }
}

/// Return the `new` function that acts as constructor for this type
fn vm_proof_iter_new_constructor(graft_config: &mut Graft) -> ast::Fn<Typing> {
    let constructor_return_type: DataType = vm_proof_iter_as_struct_type(graft_config).into();
    ast::Fn {
        signature: ast::FnSignature::value_function_immutable_args(
            "new",
            vec![],
            constructor_return_type,
        ),
        body: RoutineBody::Instructions(ast::AsmDefinedBody {
            dependencies: vec![],
            instructions: triton_asm!(push 2),
        }),
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
        let method_output = DataType::try_from_string(method_output).unwrap();
        let method_output = DataType::Boxed(Box::new(method_output));
        let snippet_label = snippet.entrypoint();
        let code = triton_asm!(call { snippet_label });

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
