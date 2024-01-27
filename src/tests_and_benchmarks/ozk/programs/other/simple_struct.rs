use crate::triton_vm::prelude::*;
use crate::triton_vm::twenty_first::shared_math::bfield_codec::BFieldCodec;

use arbitrary::Arbitrary;
use tasm_lib::structure::tasm_object::TasmObject;

#[derive(TasmObject, BFieldCodec, Clone, Arbitrary)]
pub(super) struct SimpleStruct {
    pub(crate) a: u128,
    pub(crate) b: BFieldElement,
    pub(crate) c: bool,
    pub(crate) d: Vec<Digest>,
    pub(crate) e: Digest,
}
