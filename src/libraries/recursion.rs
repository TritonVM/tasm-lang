use strum::IntoEnumIterator;
use syn::parse_quote;
use syn::PathArguments;
use tasm_lib::traits::basic_snippet::BasicSnippet;
use tasm_lib::triton_vm::proof_item::ProofItemVariant;
use tasm_lib::triton_vm::table::NUM_BASE_COLUMNS;
use tasm_lib::triton_vm::triton_asm;

use crate::ast;
use crate::ast::RoutineBody;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::ast_types::StructType;
use crate::composite_types::TypeContext;
use crate::graft::Graft;
use crate::type_checker::Typing;

const NEXT_AS_METHOD_NAMES_PREFIX: &str = "next_as_";

#[derive(Debug)]
pub(crate) struct RecursionLib;

impl RecursionLib {
    pub(crate) fn vm_proof_iter_type(graft_config: &mut Graft) -> TypeContext {
        let struct_type = vm_proof_iter_as_struct_type(graft_config);

        // List all methods
        let all_dequeue_methods = all_next_as_methods(graft_config);
        let new_function_constructor = Self::vm_proof_iter_new_constructor(graft_config);
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

    pub(crate) fn fri_response_type(graft_config: &mut Graft) -> TypeContext {
        let struct_type = fri_response_as_struct_type(graft_config);

        TypeContext {
            composite_type: struct_type.into(),
            methods: vec![],
            associated_functions: vec![],
        }
    }

    pub(crate) fn graft_base_row(
        arguments: &PathArguments,
        graft: &mut Graft,
    ) -> ast_types::DataType {
        match arguments {
            syn::PathArguments::AngleBracketed(ab) => {
                assert_eq!(1, ab.args.len(), "Must be BaseRow<T> for *one* generic T.");
                match &ab.args[0] {
                    syn::GenericArgument::Type(element_type) => {
                        let inner = graft.syn_type_to_ast_type(element_type);
                        assert!(
                            matches!(inner, DataType::Bfe | DataType::Xfe),
                            "T in BaseRow<T> must be XFE or BFE"
                        );
                        ast_types::DataType::Array(ast_types::ArrayType {
                            element_type: Box::new(inner),
                            length: NUM_BASE_COLUMNS,
                        })
                    }
                    other => panic!("Unsupported type {other:#?}"),
                }
            }
            other => panic!("Unsupported type {other:#?}"),
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
