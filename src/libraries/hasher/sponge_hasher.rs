use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::util_types::algebraic_hasher::SpongeHasher;

use crate::ast;
use crate::ast_types::{AbstractArgument, AbstractValueArg, ArrayType, DataType, ListType};
use crate::subroutine::SubRoutine;
use crate::tasm_code_generator::CompilerState;

pub(super) const SPONGE_HASHER_INDICATOR: &str = "Tip5::";
const INIT_NAME: &str = "Tip5::init";
const ABSORB_NAME: &str = "Tip5::absorb";
const SQUEEZE_NAME: &str = "Tip5::squeeze";
const PAD_AND_ABSORB_REPEATEDLY_NAME: &str = "Tip5::pad_and_absorb_all";

pub(super) fn function_names() -> Vec<&'static str> {
    vec![
        INIT_NAME,
        ABSORB_NAME,
        SQUEEZE_NAME,
        PAD_AND_ABSORB_REPEATEDLY_NAME,
    ]
}

pub(super) fn function_signature(function_name: &str) -> ast::FnSignature {
    match function_name {
        INIT_NAME => init_function_signature(),
        ABSORB_NAME => absorb_function_signature(),
        SQUEEZE_NAME => squeeze_function_signature(),
        PAD_AND_ABSORB_REPEATEDLY_NAME => todo!(),
        _ => panic!(),
    }
}

pub(super) fn call_function(
    function_name: &str,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    match function_name {
        INIT_NAME => init_function_body(),
        ABSORB_NAME => absorb_function_body(),
        SQUEEZE_NAME => squeeze_function_body(state),
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

fn squeeze_function_signature() -> ast::FnSignature {
    let sponge_argument = AbstractArgument::ValueArgument(AbstractValueArg {
        name: "sponge".to_owned(),
        data_type: DataType::Boxed(Box::new(DataType::SpongeState)),
        mutable: true,
    });
    ast::FnSignature {
        name: ABSORB_NAME.to_owned(),
        args: vec![sponge_argument],
        output: DataType::Array(ArrayType {
            element_type: Box::new(DataType::Bfe),
            length: Tip5::RATE,
        }),
        arg_evaluation_order: Default::default(),
    }
}

fn squeeze_function_body(state: &mut CompilerState) -> Vec<LabelledInstruction> {
    let dyn_malloc_label = state.import_snippet(Box::new(tasm_lib::memory::dyn_malloc::DynMalloc));
    assert_eq!(
        10,
        Tip5::RATE,
        "Implementation assumes hash function's RATE is 10"
    );
    let subroutine = triton_asm!(
        __squeeze_once:
            // _
            sponge_squeeze
            // _ [word_9..word_0]


            // Allocate memory for the returned array
            push {Tip5::RATE}
            call {dyn_malloc_label}
            // _ [word_9..word_0] *array

            // Write words to array
            write_mem 5
            write_mem 5
            // _ (*array + 10)

            push -10
            add
            // _ *array

            return
    );
    let subroutine: SubRoutine = subroutine.try_into().unwrap();
    let subroutine_label = subroutine.get_label();
    state.add_library_function(subroutine);

    triton_asm!(call { subroutine_label })
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
