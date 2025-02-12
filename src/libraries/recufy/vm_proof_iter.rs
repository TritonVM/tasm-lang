use strum::IntoEnumIterator;
use tasm_lib::traits::basic_snippet::BasicSnippet;
use tasm_lib::triton_vm::proof_item::ProofItemVariant;
use tasm_lib::verifier::vm_proof_iter::shared::vm_proof_iter_type;

use super::RecufyLib;
use crate::ast;
use crate::ast::RoutineBody;
use crate::ast_types::DataType;
use crate::composite_types::TypeContext;
use crate::graft::Graft;
use crate::libraries::polynomial::PolynomialLib;
use crate::triton_vm::prelude::*;
use crate::type_checker::Typing;

pub(super) const NEXT_AS_METHOD_NAMES_PREFIX: &str = "next_as_";
pub(super) const VM_PROOF_ITER_TYPE_NAME: &str = "VmProofIter";

pub(super) fn graft_vm_proof_iter(graft: &mut Graft) -> DataType {
    let vm_proof_iter = vm_proof_iter_type_context(graft);
    graft
        .imported_custom_types
        .add_type_context_if_new(vm_proof_iter.clone());
    let fri_response = RecufyLib::fri_response_type(graft);
    graft
        .imported_custom_types
        .add_type_context_if_new(fri_response);
    vm_proof_iter.into()
}

fn vm_proof_iter_type_context(graft: &mut Graft) -> TypeContext {
    let struct_type = vm_proof_iter_lang_struct();

    // List all methods
    let all_dequeue_methods = all_next_as_methods(graft);
    let new_function_constructor = vm_proof_iter_new_constructor();
    TypeContext {
        composite_type: struct_type.try_into().unwrap(),
        methods: all_dequeue_methods,
        associated_functions: vec![new_function_constructor],
    }
}

/// Return the `new` function that acts as constructor for this type. Returns
/// the code that puts the `VmProofIter` on top of the stack.
fn vm_proof_iter_new_constructor() -> ast::Fn<Typing> {
    let struct_type = vm_proof_iter_lang_struct();
    ast::Fn {
        signature: ast::FnSignature::value_function_immutable_args("new", vec![], struct_type),
        body: RoutineBody::Instructions(ast::AsmDefinedBody {
            dependencies: vec![],
            // Some of these values are mocked. The only one that matters, is
            // the last value, unless some internal integrity checks are being
            // performed as is the case in the `tasm-lib` verifier.
            instructions: triton_asm!(
                // current item count
                push 0

                // total item count (mocked)
                push 0

                // proof start pointer
                push 2

                // proof length (mocked)
                push 0

                // current item pointer
                push 2
            ),
        }),
    }
}

fn vm_proof_iter_lang_struct() -> DataType {
    let struct_type = vm_proof_iter_type();
    DataType::tasm_lib_struct_to_lang_struct(struct_type)
}

fn method_name_for_next_as(variant: &ProofItemVariant) -> String {
    format!(
        "{NEXT_AS_METHOD_NAMES_PREFIX}{}",
        variant.to_string().to_lowercase()
    )
}

fn all_next_as_methods(graft_config: &mut Graft) -> Vec<ast::Method<Typing>> {
    let mut methods = vec![];
    let receiver_type = vm_proof_iter_lang_struct();
    let receiver_type = DataType::Boxed(Box::new(receiver_type));

    for variant in ProofItemVariant::iter() {
        let snippet = tasm_lib::verifier::vm_proof_iter::dequeue_next_as::DequeueNextAs {
            proof_item: variant,
        };
        let method_output = variant.payload_type();

        // TODO: Handle Polyonomial<T> through polynomial library here, and add it to custom
        // types in `graft_config`.
        println!("{method_output}");

        let method_output = if let Ok(poly_type) = PolynomialLib::try_from_string(method_output) {
            graft_config
                .imported_custom_types
                .add_type_context_if_new(poly_type.clone());
            poly_type.into()
        } else {
            DataType::try_from_string(method_output).unwrap()
        };

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
