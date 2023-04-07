use crate::{
    ast::{self, FnSignature},
    tasm_code_generator::CompilerState,
    types,
};
use triton_opcodes::instruction::LabelledInstruction;

pub mod bfe;
pub mod tasm;
pub mod unsigned_integers;
pub mod vector;
pub mod xfe;

type Annotation = types::Typing;

pub struct CompiledFunction {
    signature: FnSignature,
    body: Vec<LabelledInstruction>,
}

pub trait Library {
    /// Return stripped function name iff library knows this function
    fn get_function_name(full_name: &str) -> Option<String>;

    /// Return method_name iff library knows this method
    fn get_method_name(method_name: &str, receiver_type: &ast::DataType) -> Option<String>;

    /// Return function signature of method, if method is known.
    fn method_name_to_signature(fn_name: &str, receiver_type: &ast::DataType) -> ast::FnSignature;

    /// Return function signature of function, if function is known.
    fn function_name_to_signature(
        fn_name: &str,
        type_parameter: Option<ast::DataType>,
    ) -> ast::FnSignature;

    /// Return the instructions to call the method, and imports snippets into `state` if needed.
    fn call_method(
        method_name: &str,
        receiver_type: &ast::DataType,
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction>;

    /// Return the instructions to call the function, and imports snippets into `state` if needed.
    fn call_function(
        fn_name: &str,
        type_parameter: Option<ast::DataType>,
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction>;

    /// Return stripped function name iff grafting should be handled by
    /// library and not the generic grafter.
    fn get_graft_function_name(full_name: &str) -> Option<String>;

    fn graft_function(
        fn_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
    ) -> Option<ast::Expr<Annotation>>;
}
