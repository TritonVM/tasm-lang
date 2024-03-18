mod vm_proof_iter;

use syn::parse_quote;
use syn::PathArguments;
use tasm_lib::triton_vm::table::NUM_BASE_COLUMNS;
use tasm_lib::triton_vm::table::NUM_EXT_COLUMNS;
use tasm_lib::triton_vm::table::NUM_QUOTIENT_SEGMENTS;

use crate::ast;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::ast_types::StructType;
use crate::composite_types::CompositeTypes;
use crate::composite_types::TypeContext;
use crate::graft::Graft;

use self::vm_proof_iter::graft_vm_proof_iter;
use self::vm_proof_iter::VM_PROOF_ITER_TYPE_NAME;

use super::Library;

const BASE_ROW_TYPE_NAME: &str = "BaseRow";
const EXT_ROW_TYPE_NAME: &str = "ExtensionRow";
const QUOT_SEGMENTS_TYPE_NAME: &str = "QuotientSegments";

#[derive(Debug)]
pub(crate) struct RecufyLib;

impl Library for RecufyLib {
    fn graft_type(
        &self,
        graft: &mut Graft,
        rust_type_as_string: &str,
        path_args: &syn::PathArguments,
    ) -> Option<ast_types::DataType> {
        match rust_type_as_string {
            VM_PROOF_ITER_TYPE_NAME => Some(graft_vm_proof_iter(graft)),
            BASE_ROW_TYPE_NAME => Some(Self::graft_base_row(path_args, graft)),
            EXT_ROW_TYPE_NAME => Some(Self::graft_ext_row(path_args)),
            QUOT_SEGMENTS_TYPE_NAME => Some(Self::graft_quot_segments(path_args)),
            _ => None,
        }
    }

    fn handle_function_call(
        &self,
        _full_name: &str,
        _qualified_self_type: &Option<DataType>,
    ) -> bool {
        false
    }

    fn handle_method_call(&self, _method_name: &str, _receiver_type: &ast_types::DataType) -> bool {
        false
    }

    fn method_name_to_signature(
        &self,
        _fn_name: &str,
        _receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature {
        panic!()
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        _qualified_self_type: &Option<DataType>,
        _composite_types: &mut CompositeTypes,
    ) -> ast::FnSignature {
        panic!()
    }

    fn call_method(
        &self,
        _method_name: &str,
        _receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<tasm_lib::prelude::triton_vm::prelude::LabelledInstruction> {
        panic!()
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
        _qualified_self_type: &Option<DataType>,
    ) -> Vec<tasm_lib::prelude::triton_vm::prelude::LabelledInstruction> {
        panic!()
    }

    fn get_graft_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn graft_function(
        &self,
        _graft_config: &mut Graft,
        _fn_name: &str,
        _args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        _type_parameter: Option<ast_types::DataType>,
    ) -> Option<ast::Expr<super::Annotation>> {
        None
    }

    fn graft_method_call(
        &self,
        _graft_config: &mut Graft,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        None
    }
}

impl RecufyLib {
    fn fri_response_type(graft_config: &mut Graft) -> TypeContext {
        let struct_type = Self::fri_response_as_struct_type(graft_config);

        TypeContext {
            composite_type: struct_type.into(),
            methods: vec![],
            associated_functions: vec![],
        }
    }

    fn graft_ext_row(arguments: &PathArguments) -> ast_types::DataType {
        assert!(matches!(arguments, PathArguments::None));
        ast_types::DataType::Array(ast_types::ArrayType {
            element_type: Box::new(ast_types::DataType::Xfe),
            length: NUM_EXT_COLUMNS,
        })
    }

    fn graft_quot_segments(arguments: &PathArguments) -> ast_types::DataType {
        assert!(matches!(arguments, PathArguments::None));
        ast_types::DataType::Array(ast_types::ArrayType {
            element_type: Box::new(ast_types::DataType::Xfe),
            length: NUM_QUOTIENT_SEGMENTS,
        })
    }

    fn graft_base_row(arguments: &PathArguments, graft: &mut Graft) -> ast_types::DataType {
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
}
