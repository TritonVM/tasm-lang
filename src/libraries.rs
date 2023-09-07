use crate::{
    ast::{self, FnSignature},
    ast_types::{self},
    graft::Graft,
    tasm_code_generator::CompilerState,
    type_checker,
};
use std::fmt::Debug;

pub mod bfe;
pub mod bfield_codec;
pub mod hasher;
pub mod tasm;
pub mod unsigned_integers;
pub mod vector;
pub mod xfe;

type Annotation = type_checker::Typing;

pub struct LibraryFunction {
    pub signature: FnSignature,
    pub body: Vec<triton_vm::instruction::LabelledInstruction>,
}

pub struct LibraryConfig {
    pub list_type: ast_types::ListType,
}

pub fn all_libraries<'a>(config: LibraryConfig) -> Vec<Box<dyn Library + 'a>> {
    vec![
        Box::new(bfe::BfeLibrary),
        Box::new(bfield_codec::BFieldCodecLib {
            list_type: config.list_type,
        }),
        Box::new(hasher::HasherLib {
            list_type: config.list_type,
        }),
        Box::new(tasm::TasmLibrary {
            list_type: config.list_type,
        }),
        Box::new(unsigned_integers::UnsignedIntegersLib {
            list_type: config.list_type,
        }),
        Box::new(vector::VectorLib {
            list_type: config.list_type,
        }),
        Box::new(xfe::XfeLibrary),
    ]
}

pub trait Library: Debug {
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
    ) -> Vec<triton_vm::instruction::LabelledInstruction>;

    /// Return the instructions to call the function, and imports snippets into `state` if needed.
    fn call_function(
        &self,
        fn_name: &str,
        type_parameter: Option<ast_types::DataType>,
        args: &[ast::Expr<Annotation>],
        state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction>;

    /// Return full function name iff grafting should be handled by
    /// library and not the generic grafter.
    fn get_graft_function_name(&self, full_name: &str) -> Option<String>;

    fn graft_function(
        &self,
        graft_config: &Graft,
        fn_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        type_parameter: Option<ast_types::DataType>,
    ) -> Option<ast::Expr<Annotation>>;

    fn graft_method(
        &self,
        graft_config: &Graft,
        rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<Annotation>>;
}
