use tasm_lib::twenty_first::bfieldcodec_derive::BFieldCodec;
use tasm_lib::twenty_first::shared_math::b_field_element::BFieldElement;
use tasm_lib::Digest;

#[derive(Clone, Debug, BFieldCodec)]
pub(crate) struct Claim {
    pub program_digest: Digest,
    pub input: Vec<BFieldElement>,
    pub output: Vec<BFieldElement>,
}
