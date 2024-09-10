use syn::PathArguments;
use tasm_lib::triton_vm::prelude::triton_asm;

use crate::ast;
use crate::ast::FnSignature;
use crate::ast_types;
use crate::ast_types::AbstractArgument;
use crate::ast_types::AbstractValueArg;
use crate::ast_types::DataType;
use crate::composite_types::CompositeTypes;
use crate::graft::Graft;
use crate::type_checker::Typing;

pub(super) const RESULT_TYPE_NAME: &str = "Result";

pub(super) fn rust_result_type_to_data_type(
    graft: &mut Graft,
    path_args: &PathArguments,
) -> DataType {
    let PathArguments::AngleBracketed(generics) = path_args else {
        panic!("Unsupported path argument {path_args:#?}");
    };
    assert_eq!(2, generics.args.len(), "`Result` must have two generics");

    let ok_type_arg = &generics.args[0];
    let syn::GenericArgument::Type(ok_type) = ok_type_arg else {
        panic!("Unsupported type {ok_type_arg:#?}");
    };
    let ok_type = graft.syn_type_to_ast_type(ok_type);
    wrap_and_import_result_type(ok_type, &mut graft.imported_custom_types)
}

pub(crate) fn wrap_and_import_result_type(
    ok_type: DataType,
    composite_types: &mut CompositeTypes,
) -> DataType {
    let resolved_type = result_type(ok_type);
    composite_types.add_type_context_if_new(resolved_type.clone());
    ast_types::DataType::Enum(Box::new(resolved_type.composite_type.try_into().unwrap()))
}

fn result_type(ok_type: DataType) -> crate::composite_types::TypeContext {
    let enum_type = ast_types::EnumType {
        is_copy: ok_type.is_copy(),
        name: RESULT_TYPE_NAME.to_owned(),
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

    let code = triton_asm!(
        // _ [ok_type] discriminant
        assert // _ [ok_type]
    );
    ast::Method {
        body: ast::RoutineBody::<Typing>::Instructions(ast::AsmDefinedBody {
            dependencies: vec![],
            instructions: code,
        }),
        signature: method_signature,
    }
}

fn result_is_err_method(enum_type: &ast_types::EnumType) -> ast::Method<Typing> {
    let method_signature = FnSignature {
        name: "is_err".to_owned(),
        args: vec![AbstractArgument::ValueArgument(AbstractValueArg {
            name: "self".to_owned(),
            data_type: DataType::Boxed(Box::new(enum_type.into())),
            mutable: false,
        })],
        output: DataType::Bool,
        arg_evaluation_order: Default::default(),
    };

    let code = triton_asm!(
            // _ *discriminant
            read_mem 1 pop 1
            // _ discriminant

            push 0
            eq
            // _ (discriminant == 0 :== variant is 'Err')
    );
    ast::Method {
        body: ast::RoutineBody::<Typing>::Instructions(ast::AsmDefinedBody {
            dependencies: vec![],
            instructions: code,
        }),
        signature: method_signature,
    }
}

fn result_is_ok_method(enum_type: &ast_types::EnumType) -> ast::Method<Typing> {
    let method_signature = FnSignature {
        name: "is_ok".to_owned(),
        args: vec![AbstractArgument::ValueArgument(AbstractValueArg {
            name: "self".to_owned(),
            data_type: DataType::Boxed(Box::new(enum_type.into())),
            mutable: false,
        })],
        output: DataType::Bool,
        arg_evaluation_order: Default::default(),
    };

    let code = triton_asm!(
            // *discriminant
            read_mem 1 pop 1
            // discriminant

            push 1
            eq
            // _ (discriminant == 1 :== variant is 'Ok')
    );
    ast::Method {
        body: ast::RoutineBody::<Typing>::Instructions(ast::AsmDefinedBody {
            dependencies: vec![],
            instructions: code,
        }),
        signature: method_signature,
    }
}
