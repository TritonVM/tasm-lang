use tasm_lib::prelude::DynMalloc;
use tasm_lib::prelude::MemCpy;

use crate::ast::FnSignature;
use crate::ast_types::ArrayType;
use crate::ast_types::DataType;
use crate::composite_types::CompositeTypes;
use crate::libraries::core::result_type;
use crate::subroutine::SubRoutine;
use crate::tasm_code_generator::CompilerState;
use crate::triton_vm::prelude::LabelledInstruction;
use crate::triton_vm::triton_asm;

pub(crate) fn len_method_signature(array_type: &ArrayType) -> FnSignature {
    let value_args = vec![("self", array_type.into())];
    FnSignature::value_function_immutable_args("len", value_args, DataType::U32)
}

pub(crate) fn len_method_body(array_type: &ArrayType) -> Vec<LabelledInstruction> {
    triton_asm!(pop 1 push {array_type.length})
}

pub(crate) fn to_vec_method_signature(array_type: &ArrayType) -> FnSignature {
    let value_args = vec![("self", array_type.into())];
    let output_type = DataType::List(array_type.element_type.clone());
    FnSignature::value_function_immutable_args("to_vec", value_args, output_type)
}

pub(crate) fn vec_to_array_function_signature(
    array_type: &ArrayType,
    arg_type: &DataType,
    composite_types: &mut CompositeTypes,
) -> FnSignature {
    let output_type = result_type::wrap_and_import_result_type(array_type.into(), composite_types);
    FnSignature::value_function_immutable_args(
        "try_from",
        vec![("vec", arg_type.to_owned())],
        output_type,
    )
}

pub(crate) fn vec_to_array_function_code(array_type: &ArrayType) -> Vec<LabelledInstruction> {
    let expected_vec_length = array_type.length;
    triton_asm!(
        // _ *len

        read_mem 1
        push 2
        add
        // _ vec_len *word_0

        swap 1
        // _ *word_0 vec_len

        push {expected_vec_length}
        eq
        // _ *word_0 (vec_len == expected_vec_length)

        // _ *word_0 result_discriminant
    )
}

fn to_vec_method_code(compiler_state: &mut CompilerState, array_type: &ArrayType) -> SubRoutine {
    let dyn_malloc = compiler_state.import_snippet(Box::new(DynMalloc));
    let mem_cpy = compiler_state.import_snippet(Box::new(MemCpy));

    let entrypoint = format!(
        "array_to_vec_with_len_{}_element_size_{}",
        array_type.length,
        array_type.element_type.stack_size()
    );

    triton_asm!(
        // BEFORE: _ *array
        // AFTER:  _ *vec
        {entrypoint}:
            call {dyn_malloc}   // _ *array *vec
            swap 1              // _ *vec *array
            push {array_type.length}
            dup 2               // _ *vec *array len *vec
            write_mem 1         // _ *vec *array (*vec+1)

            push {array_type.length * array_type.element_type.stack_size()}
                                // _ *vec *array (*vec+1) size
            call {mem_cpy}      // _ *vec
            return
    )
    .try_into()
    .unwrap()
}

pub(crate) fn import_and_call_to_vec(
    state: &mut CompilerState,
    array_type: &ArrayType,
) -> Vec<LabelledInstruction> {
    let to_vec_routine = to_vec_method_code(state, array_type);
    let to_vec = to_vec_routine.get_label();
    state.add_subroutine(to_vec_routine);
    triton_asm!(call { to_vec })
}
