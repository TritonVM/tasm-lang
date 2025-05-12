use arbitrary::Arbitrary;
use tasm_lib::prelude::TasmObject;
use tasm_lib::triton_vm::prelude::*;

#[derive(TasmObject, BFieldCodec, Clone, Arbitrary)]
pub(super) struct SimpleStruct {
    pub(crate) a: u128,
    pub(crate) b: BFieldElement,
    pub(crate) c: bool,
    pub(crate) d: Vec<Digest>,
    pub(crate) e: Digest,
}
