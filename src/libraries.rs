use crate::{
    ast::{self, FnSignature},
    tasm_code_generator::CompilerState,
    types,
};
use std::fmt::Debug;

pub mod bfe;
pub mod bfield_codec;
pub mod hasher;
pub mod tasm;
pub mod unsigned_integers;
pub mod vector;
pub mod xfe;

type Annotation = types::Typing;

pub struct LibraryFunction {
    pub signature: FnSignature,
    pub body: Vec<triton_vm::instruction::LabelledInstruction>,
}

pub fn all_libraries() -> Vec<Box<dyn Library>> {
    vec![
        Box::new(bfe::BfeLibrary),
        Box::new(bfield_codec::BFieldCodecLib),
        Box::new(hasher::HasherLib),
        Box::new(tasm::TasmLibrary),
        Box::new(unsigned_integers::UnsignedIntegersLib),
        Box::new(vector::VectorLib),
        Box::new(xfe::XfeLibrary),
    ]
}

pub trait Library: Debug {
    /// Return full function name iff library knows this function
    fn get_function_name(&self, full_name: &str) -> Option<String>;

    /// Return method_name iff library knows this method
    fn get_method_name(&self, method_name: &str, receiver_type: &ast::DataType) -> Option<String>;

    /// Return function signature of method, if method is known.
    fn method_name_to_signature(
        &self,
        fn_name: &str,
        receiver_type: &ast::DataType,
        args: &[ast::Expr<Annotation>],
    ) -> ast::FnSignature;

    /// Return function signature of function, if function is known.
    fn function_name_to_signature(
        &self,
        fn_name: &str,
        type_parameter: Option<ast::DataType>,
        args: &[ast::Expr<Annotation>],
    ) -> ast::FnSignature;

    /// Return the instructions to call the method, and imports snippets into `state` if needed.
    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &ast::DataType,
        args: &[ast::Expr<Annotation>],
        state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction>;

    /// Return the instructions to call the function, and imports snippets into `state` if needed.
    fn call_function(
        &self,
        fn_name: &str,
        type_parameter: Option<ast::DataType>,
        args: &[ast::Expr<Annotation>],
        state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction>;

    /// Return full function name iff grafting should be handled by
    /// library and not the generic grafter.
    fn get_graft_function_name(&self, full_name: &str) -> Option<String>;

    fn graft_function(
        &self,
        fn_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
    ) -> Option<ast::Expr<Annotation>>;

    fn graft_method(&self, rust_method_call: &syn::ExprMethodCall)
        -> Option<ast::Expr<Annotation>>;
}
