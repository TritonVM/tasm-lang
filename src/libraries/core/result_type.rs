use triton_vm::triton_asm;

use crate::ast::{self, FnSignature};
use crate::ast_types::DataType;
use crate::ast_types::{self, AbstractArgument, AbstractValueArg};
use crate::tasm_code_generator;
use crate::type_checker::Typing;

pub(crate) fn result_type(ok_type: DataType) -> crate::composite_types::TypeContext {
    let enum_type = ast_types::EnumType {
        is_copy: ok_type.is_copy(),
        name: "Result".to_owned(),
        variants: vec![
            (
                "Err".to_owned(),
                DataType::Tuple(vec![DataType::unit()].into()),
            ),
            ("Ok".to_owned(), ok_type.clone()),
        ],
        is_prelude: true,
        type_parameter: Some(ok_type.clone()),
    };
    let is_ok_method = result_is_ok_method(&enum_type);
    let is_err_method = result_is_err_method(&enum_type);
    let unwrap_method = result_unwrap_method(&enum_type);

    crate::composite_types::TypeContext {
        composite_type: enum_type.into(),
        methods: vec![is_ok_method, is_err_method, unwrap_method],
        associated_functions: vec![],
    }
}

fn result_unwrap_method(enum_type: &ast_types::EnumType) -> ast::Method<Typing> {
    let method_signature = FnSignature {
        name: "unwrap".to_owned(),
        args: vec![AbstractArgument::ValueArgument(AbstractValueArg {
            name: "self".to_owned(),
            data_type: enum_type.into(),
            mutable: false,
        })],
        output: enum_type.variant_data_type("Ok"),
        arg_evaluation_order: Default::default(),
    };

    ast::Method {
        body: ast::RoutineBody::<Typing>::Instructions(triton_asm!(
            // _ [ok_type] discriminant
            assert // _ [ok_type]
        )),
        signature: method_signature,
    }
}

fn result_is_err_method(enum_type: &ast_types::EnumType) -> ast::Method<Typing> {
    let stack_size = enum_type.stack_size();
    let swap_to_bottom = match stack_size {
        0 => unreachable!(),
        1 => triton_asm!(),
        2..=16 => triton_asm!(swap { stack_size - 1 }),
        _ => panic!("Can't handle this yet"), // This should work with spilling
    };
    let remove_data = match stack_size {
        0 => unreachable!(),
        1 => triton_asm!(pop 1),
        2..=16 => tasm_code_generator::pop_n(stack_size - 1),
        _ => panic!("Can't handle this yet"),
    };
    let is_ok_input_data_type = DataType::Boxed(Box::new(enum_type.into()));
    let method_signature = FnSignature {
        name: "is_err".to_owned(),
        args: vec![AbstractArgument::ValueArgument(AbstractValueArg {
            name: "self".to_owned(),
            data_type: is_ok_input_data_type,
            mutable: false,
        })],
        output: DataType::Bool,
        arg_evaluation_order: Default::default(),
    };

    ast::Method {
        body: ast::RoutineBody::<Typing>::Instructions(triton_asm!(
                // _ [ok_type] discriminant
                {&swap_to_bottom}
                // _ discriminant [ok_type']

                {&remove_data}
                // _ discriminant

                push 0
                eq
                // _ (discriminant == 0 :== variant is 'Err')
        )),
        signature: method_signature,
    }
}

fn result_is_ok_method(enum_type: &ast_types::EnumType) -> ast::Method<Typing> {
    let stack_size = enum_type.stack_size();
    let swap_to_bottom = match stack_size {
        0 => unreachable!(),
        1 => triton_asm!(),
        2..=16 => triton_asm!(swap { stack_size - 1 }),
        _ => panic!("Can't handle this yet"), // This should work with spilling
    };
    let remove_data = match stack_size {
        0 => unreachable!(),
        1 => triton_asm!(pop 1),
        2..=16 => tasm_code_generator::pop_n(stack_size - 1),
        _ => panic!("Can't handle this yet"),
    };
    let is_ok_input_data_type = DataType::Boxed(Box::new(enum_type.into()));
    let method_signature = FnSignature {
        name: "is_ok".to_owned(),
        args: vec![AbstractArgument::ValueArgument(AbstractValueArg {
            name: "self".to_owned(),
            data_type: is_ok_input_data_type,
            mutable: false,
        })],
        output: DataType::Bool,
        arg_evaluation_order: Default::default(),
    };

    ast::Method {
        body: ast::RoutineBody::<Typing>::Instructions(triton_asm!(
                // _ [ok_type] discriminant
                {&swap_to_bottom}
                // _ discriminant [ok_type']

                {&remove_data}
                // _ discriminant

                push 1
                eq
                // _ (discriminant == 1 :== variant is 'Ok')
        )),
        signature: method_signature,
    }
}
