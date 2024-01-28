use tasm_lib::triton_vm::prelude::*;

use crate::ast;
use crate::ast_types::{AbstractArgument, AbstractValueArg, ArrayType, DataType, ListType};
use crate::tasm_code_generator::CompilerState;

pub(super) const SPONGE_HASHER_INDICATOR: &str = "Tip5::";
const INIT_NAME: &str = "Tip5::init";
const ABSORB_NAME: &str = "Tip5::absorb";
const PAD_AND_ABSORB_REPEATEDLY_NAME: &str = "Tip5::pad_and_absorb_all";

pub(super) fn function_names() -> Vec<&'static str> {
    vec![INIT_NAME, PAD_AND_ABSORB_REPEATEDLY_NAME, ABSORB_NAME]
}

pub(super) fn function_signature(function_name: &str) -> ast::FnSignature {
    match function_name {
        INIT_NAME => init_function_signature(),
        ABSORB_NAME => absorb_function_signature(),
        PAD_AND_ABSORB_REPEATEDLY_NAME => todo!(),
        _ => panic!(),
    }
}

pub(super) fn call_function(function_name: &str) -> Vec<LabelledInstruction> {
    match function_name {
        INIT_NAME => init_function_body(),
        ABSORB_NAME => absorb_function_body(),
        PAD_AND_ABSORB_REPEATEDLY_NAME => todo!(),
        _ => panic!(),
    }
}

fn init_function_signature() -> ast::FnSignature {
    ast::FnSignature {
        name: INIT_NAME.to_owned(),
        args: vec![],
        output: DataType::SpongeState,
        arg_evaluation_order: Default::default(),
    }
}

fn init_function_body() -> Vec<LabelledInstruction> {
    triton_asm!(sponge_init)
}

fn absorb_function_signature() -> ast::FnSignature {
    let sponge_argument = AbstractArgument::ValueArgument(AbstractValueArg {
        name: "sponge".to_owned(),
        data_type: DataType::Boxed(Box::new(DataType::SpongeState)),
        mutable: true,
    });
    let input_argument = AbstractArgument::ValueArgument(AbstractValueArg {
        name: "input".to_owned(),
        data_type: DataType::Boxed(Box::new(DataType::Array(ArrayType {
            element_type: Box::new(DataType::Bfe),
            length: 10,
        }))),
        mutable: false,
    });
    ast::FnSignature {
        name: ABSORB_NAME.to_owned(),
        args: vec![sponge_argument, input_argument],
        output: DataType::unit(),
        arg_evaluation_order: Default::default(),
    }
}

fn absorb_function_body() -> Vec<LabelledInstruction> {
    triton_asm!(
        // _ *array_bfe_10
        push 9 add

        // _ *last_bfe
        read_mem 5
        read_mem 5

        // _ [word_9..word_0] (*first_elem - 1)
        pop 1
        // _ [word_9..word_0]

        sponge_absorb
        // _
    )
}

fn pad_and_absorb_all_signature(list_type: ListType) -> ast::FnSignature {
    let sponge_argument = AbstractArgument::ValueArgument(AbstractValueArg {
        name: "sponge".to_owned(),
        data_type: DataType::Boxed(Box::new(DataType::SpongeState)),
        mutable: true,
    });
    let input_argument = AbstractArgument::ValueArgument(AbstractValueArg {
        name: "input".to_owned(),
        data_type: DataType::Boxed(Box::new(DataType::List(Box::new(DataType::Bfe), list_type))),
        mutable: true,
    });
    ast::FnSignature {
        name: PAD_AND_ABSORB_REPEATEDLY_NAME.to_owned(),
        args: vec![sponge_argument, input_argument],
        output: DataType::SpongeState,
        arg_evaluation_order: Default::default(),
    }
}

fn pad_and_absorb_all_body(
    list_type: ListType,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    let absorb_and_pad_snippet_label =
        state.import_snippet(Box::new(tasm_lib::hashing::absorb::Absorb));
    let list_metadata_size_plus_one = list_type.metadata_size() + 1;

    triton_asm!(
        // _ *bfes
        read_mem 1

        // _ length (*bfes - 1)

        push {list_metadata_size_plus_one}
        add
        // _ length *first_bfe

        swap 1
        // _ *first_bfe length

        call {absorb_and_pad_snippet_label}
        // _
    )
}
