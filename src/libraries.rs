use std::fmt::Debug;

use crate::ast;
use crate::ast::FnSignature;
use crate::ast_types;
use crate::graft::Graft;
use crate::tasm_code_generator::CompilerState;
use crate::type_checker;
use tasm_lib::triton_vm::prelude::*;

pub(crate) mod bfe;
pub(crate) mod bfield_codec;
pub(crate) mod boxed;
pub(crate) mod core;
pub(crate) mod hasher;
pub(crate) mod recursion;
pub(crate) mod tasm;
pub(crate) mod unsigned_integers;
pub(crate) mod vector;
pub(crate) mod xfe;

type Annotation = type_checker::Typing;

#[derive(Debug)]
pub(crate) struct LibraryFunction {
    pub(crate) signature: FnSignature,

    /// Does not include an entrypoint label or the `return` instruction.
    pub(crate) body: Vec<LabelledInstruction>,
}

pub(crate) fn all_libraries() -> Box<[Box<dyn Library>]> {
    Box::new([
        Box::new(boxed::Boxed),
        Box::new(bfe::BfeLibrary),
        Box::new(core::Core),
        Box::new(bfield_codec::BFieldCodecLib),
        Box::new(hasher::HasherLib),
        Box::new(tasm::TasmLibrary),
        Box::new(unsigned_integers::UnsignedIntegersLib),
        Box::new(vector::VectorLib),
        Box::new(xfe::XfeLibrary),
        Box::new(recursion::RecursionLib),
    ])
}

pub(crate) trait Library: Debug {
    fn graft_type(
        &self,
        graft: &mut Graft,
        rust_type_as_string: &str,
        path_args: &syn::PathArguments,
    ) -> Option<ast_types::DataType>;

    /// Return full function name iff library knows this function
    fn get_function_name(&self, full_name: &str) -> Option<String>;

    /// Return method_name iff library knows this method
    fn get_method_name(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
    ) -> Option<String>;

    /// Return function signature of method, if method is known.
    fn method_name_to_signature(
        &self,
        fn_name: &str,
        receiver_type: &ast_types::DataType,
        args: &[ast::Expr<Annotation>],
        type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature;

    /// Return function signature of function, if function is known.
    fn function_name_to_signature(
        &self,
        fn_name: &str,
        type_parameter: Option<ast_types::DataType>,
        args: &[ast::Expr<Annotation>],
    ) -> ast::FnSignature;

    /// Return the instructions to call the method, and imports snippets into `state` if needed.
    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
        args: &[ast::Expr<Annotation>],
        state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<LabelledInstruction>;

    /// Return the instructions to call the function, and imports snippets into `state` if needed.
    fn call_function(
        &self,
        fn_name: &str,
        type_parameter: Option<ast_types::DataType>,
        args: &[ast::Expr<Annotation>],
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction>;

    /// Return full function name iff grafting should be handled by
    /// library and not the generic grafter.
    fn get_graft_function_name(&self, full_name: &str) -> Option<String>;

    fn graft_function(
        &self,
        graft_config: &mut Graft,
        fn_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        type_parameter: Option<ast_types::DataType>,
    ) -> Option<ast::Expr<Annotation>>;

    fn graft_method_call(
        &self,
        graft_config: &mut Graft,
        rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<Annotation>>;
}
