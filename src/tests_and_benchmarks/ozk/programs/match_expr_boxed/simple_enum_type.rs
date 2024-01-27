use tasm_lib::triton_vm::prelude::*;

#[derive(BFieldCodec, Clone, Debug)]
pub(super) enum SimpleEnum {
    A,
    B(BFieldElement),
}
