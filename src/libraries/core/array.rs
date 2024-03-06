use crate::ast::FnSignature;
use crate::ast_types::ArrayType;
use crate::ast_types::DataType;
use crate::triton_vm::prelude::LabelledInstruction;
use crate::triton_vm::triton_asm;

pub(crate) fn len_method_signature(array_type: &ArrayType) -> FnSignature {
    let value_args = vec![("self", array_type.into())];
    FnSignature::value_function_immutable_args("len", value_args, DataType::U32)
}

pub(crate) fn len_method_body(array_type: &ArrayType) -> Vec<LabelledInstruction> {
    triton_asm!(pop 1 push {array_type.length})
}
