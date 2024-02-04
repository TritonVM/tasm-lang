use strum::IntoEnumIterator;
use syn::parse_quote;
use tasm_lib::traits::basic_snippet::BasicSnippet;
use tasm_lib::triton_vm::proof_item::ProofItemVariant;
use tasm_lib::triton_vm::triton_asm;

use crate::ast;
use crate::ast::RoutineBody;
use crate::ast_types::StructType;
use crate::composite_types::TypeContext;
use crate::graft::Graft;
use crate::type_checker::Typing;

use super::Library;

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

    fn graft_method(
        &self,
        graft_config: &mut Graft,
        rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        // TODO: Add all `next_as_...` method calls here
        // to remove the `Tip5State` argument from the method call.
        // It might be enough to simply remove *that* argument. Alternatively
        // one could handle that removal in the hasher library.
        None
    }
}

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

fn vm_proof_iter_as_struct_type(graft_config: &mut Graft) -> StructType {
    let tokens: syn::Item = parse_quote! {
        struct VmProofIter {
            proof_iter_pointer: BFieldElement,
        }
    };
    let syn::Item::Struct(item_struct) = tokens else {
        panic!()
    };

    graft_config.graft_struct_type(&item_struct)
}

fn all_next_as_methods(graft_config: &mut Graft) -> Vec<ast::Method<Typing>> {
    let mut methods = vec![];

    for variant in ProofItemVariant::iter() {
        let snippet = tasm_lib::recufier::proof_stream::dequeue_next_as::DequeueNextAs {
            proof_item: variant,
        };
        let snippet_label = snippet.entrypoint();
        let code = triton_asm!(call { snippet_label });
        let method = ast::Method {
            signature: ast::FnSignature::from_basic_snippet(
                Box::new(snippet),
                graft_config.list_type,
            ),
            body: RoutineBody::Instructions(code),
        };

        methods.push(method);
    }

    methods
}
