use triton_vm::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

#[derive(BFieldCodec, Clone, Debug)]
pub(super) enum SimpleEnum {
    A,
    B(BFieldElement),
}
