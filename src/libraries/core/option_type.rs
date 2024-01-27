use crate::triton_vm::triton_asm;

use crate::ast;
use crate::ast::FnSignature;
use crate::ast_types;
use crate::ast_types::AbstractArgument;
use crate::ast_types::AbstractValueArg;
use crate::ast_types::DataType;
use crate::type_checker::Typing;

pub(crate) fn option_type(payload_type: DataType) -> crate::composite_types::TypeContext {
    let enum_type = ast_types::EnumType {
        is_copy: payload_type.is_copy(),
        name: "Option".to_owned(),
        variants: vec![
            ("None".to_owned(), DataType::unit()),
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
    let argument_type = DataType::Boxed(Box::new(enum_type.into()));
    let method_signature = FnSignature {
        name: "is_none".to_owned(),
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
                // _ *discriminant

                read_mem 1
                // _ discriminant (*discriminant - 1)

                pop 1
                // _ discriminant

                push 0
                eq
                // _ (discriminant == 0 :== variant is 'None')
        )),
        signature: method_signature,
    }
}

fn option_is_some_method(enum_type: &ast_types::EnumType) -> ast::Method<Typing> {
    let argument_type = DataType::Boxed(Box::new(enum_type.into()));
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
                // _ *discriminant

                read_mem 1
                // _ discriminant (*discriminant - 1)

                pop 1
                // _ discriminant

                push 1
                eq
                // _ (discriminant == 1 :== variant is 'Some')
        )),
        signature: method_signature,
    }
}
