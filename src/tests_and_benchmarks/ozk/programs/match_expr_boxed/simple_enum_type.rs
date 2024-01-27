use crate::triton_vm::prelude::*;
use crate::twenty_first::shared_math::bfield_codec::BFieldCodec;

#[derive(BFieldCodec, Clone, Debug)]
pub(super) enum SimpleEnum {
    A,
    B(BFieldElement),
}
