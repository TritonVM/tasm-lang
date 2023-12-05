use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::ExprMethodCall;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;

use crate::ast::{self, Expr, FnSignature};
use crate::ast_types::DataType;
use crate::ast_types::{self, AbstractArgument, AbstractValueArg};
use crate::graft::Graft;
use crate::libraries::{Annotation, Library};
use crate::tasm_code_generator::CompilerState;
use crate::type_checker::CheckState;
use crate::type_checker::Typing;

/// Everything that lives in the Rust `core` module
/// belongs in here.
#[derive(Debug)]
pub struct Core {}

pub(crate) fn result_type(ok_type: ast_types::DataType) -> crate::composite_types::TypeContext {
    let dtype = ast_types::EnumType {
        is_copy: ok_type.is_copy(),
        name: "Result".to_owned(),
        variants: vec![
            ("Ok".to_owned(), ok_type.clone()),
            ("Err".to_owned(), ast_types::DataType::unit()),
        ],
        is_prelude: true,
        type_parameter: Some(ok_type),
    };
    let dtype = ast_types::DataType::Enum(Box::new(dtype));
    let is_ok_method_label = "is_ok";
    let stack_size = dtype.stack_size();
    let swap_to_bottom = match stack_size {
        0 => unreachable!(),
        1 => triton_asm!(),
        2..=16 => triton_asm!(swap { stack_size - 1 }),
        _ => panic!("Can't handle this yet"), // This should work with spilling
    };
    let remove_data = match stack_size {
        0 => unreachable!(),
        1 => triton_asm!(pop),
        2..=16 => {
            let as_str = "pop\n".repeat(stack_size - 1);
            triton_asm!({ as_str })
        }
        _ => panic!("Can't handle this yet"),
    };
    let is_ok_input_data_type = ast_types::DataType::Reference(Box::new(dtype.clone()));
    let method_signature = ast::FnSignature {
        name: is_ok_method_label.to_owned(),
        args: vec![AbstractArgument::ValueArgument(AbstractValueArg {
            name: "x".to_owned(),
            data_type: is_ok_input_data_type,
            mutable: false,
        })],
        output: ast_types::DataType::Bool,
        arg_evaluation_order: Default::default(),
    };
    let is_ok_method = ast::Method {
        body: crate::ast::RoutineBody::<Typing>::Instructions(triton_asm!(
                // _ [ok_type] discriminant
                {&swap_to_bottom}
                // _ discriminant [ok_type']

                {&remove_data}
                // _ discriminant

                push 0
                eq
                // _ (discriminant == 0)
        )),
        signature: method_signature,
    };
    crate::composite_types::TypeContext {
        composite_type: dtype.try_into().unwrap(),
        methods: vec![is_ok_method],
        associated_functions: vec![],
    }
}

impl Library for Core {
    fn get_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn get_method_name(&self, _method_name: &str, _receiver_type: &DataType) -> Option<String> {
        None
    }

    fn method_name_to_signature(
        &self,
        _fn_name: &str,
        _receiver_type: &DataType,
        _args: &[Expr<Annotation>],
        _type_checker_state: &CheckState,
    ) -> FnSignature {
        panic!()
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<DataType>,
        _args: &[Expr<Annotation>],
    ) -> FnSignature {
        todo!()
    }

    fn call_method(
        &self,
        _method_name: &str,
        _receiver_type: &DataType,
        _args: &[Expr<Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<DataType>,
        _args: &[Expr<Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        panic!()
    }

    fn get_graft_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn graft_function(
        &self,
        _graft_config: &mut Graft,
        _fn_name: &str,
        _args: &Punctuated<syn::Expr, Comma>,
        _type_parameter: Option<DataType>,
    ) -> Option<Expr<Annotation>> {
        panic!()
    }

    fn graft_method(
        &self,
        _graft_config: &mut Graft,
        _rust_method_call: &ExprMethodCall,
    ) -> Option<Expr<Annotation>> {
        None
    }
}
