use triton_vm::triton_asm;

use crate::ast::{self, FnSignature};
use crate::ast_types::DataType;
use crate::ast_types::{self, AbstractArgument, AbstractValueArg};
use crate::tasm_code_generator;
use crate::type_checker::Typing;

pub(crate) fn option_type(payload_type: DataType) -> crate::composite_types::TypeContext {
    let enum_type = ast_types::EnumType {
        is_copy: payload_type.is_copy(),
        name: "Option".to_owned(),
        variants: vec![
            (
                "None".to_owned(),
                DataType::Tuple(vec![DataType::unit()].into()),
            ),
            ("Some".to_owned(), payload_type.clone()),
        ],
        is_prelude: true,
        type_parameter: Some(payload_type.clone()),
    };
    let is_some_method = option_is_some_method(&enum_type);
    let is_none_method = option_is_none_method(&enum_type);
    let unwrap_method = option_unwrap_method(&enum_type);

    crate::composite_types::TypeContext {
        composite_type: enum_type.into(),
        methods: vec![is_some_method, is_none_method, unwrap_method],
        associated_functions: vec![],
    }
}

fn option_unwrap_method(enum_type: &ast_types::EnumType) -> ast::Method<Typing> {
    let method_signature = FnSignature {
        name: "unwrap".to_owned(),
        args: vec![AbstractArgument::ValueArgument(AbstractValueArg {
            name: "self".to_owned(),
            data_type: enum_type.into(),
            mutable: false,
        })],
        output: enum_type.variant_data_type("Some"),
        arg_evaluation_order: Default::default(),
    };

    ast::Method {
        body: ast::RoutineBody::<Typing>::Instructions(triton_asm!(
            // _ [some_type] discriminant
            assert // _ [some_type]
        )),
        signature: method_signature,
    }
}

fn option_is_none_method(enum_type: &ast_types::EnumType) -> ast::Method<Typing> {
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
    let argument_type = DataType::Reference(Box::new(enum_type.into()));
    let method_signature = FnSignature {
        name: "is_err".to_owned(),
        args: vec![AbstractArgument::ValueArgument(AbstractValueArg {
            name: "self".to_owned(),
            data_type: argument_type,
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
                // _ (discriminant == 0 :== variant is 'None')
        )),
        signature: method_signature,
    }
}

fn option_is_some_method(enum_type: &ast_types::EnumType) -> ast::Method<Typing> {
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
    let argument_type = DataType::Reference(Box::new(enum_type.into()));
    let method_signature = FnSignature {
        name: "is_some".to_owned(),
        args: vec![AbstractArgument::ValueArgument(AbstractValueArg {
            name: "self".to_owned(),
            data_type: argument_type,
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
                // _ (discriminant == 1 :== variant is 'Some')
        )),
        signature: method_signature,
    }
}
