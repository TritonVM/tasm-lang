use arbitrary::Arbitrary;
use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::{BFieldElement, Digest};
use twenty_first::shared_math::bfield_codec::BFieldCodec;

#[derive(TasmObject, BFieldCodec, Clone, Arbitrary)]
pub(super) struct SimpleStruct {
    pub a: u128,
    pub b: BFieldElement,
    pub c: bool,
    pub d: Vec<Digest>,
    pub e: Digest,
}
