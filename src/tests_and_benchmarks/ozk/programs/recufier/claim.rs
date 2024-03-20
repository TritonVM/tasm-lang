use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use tasm_lib::twenty_first::bfieldcodec_derive::BFieldCodec;
use tasm_lib::twenty_first::shared_math::b_field_element::BFieldElement;
use tasm_lib::Digest;

#[derive(Clone, Debug, BFieldCodec)]
pub(crate) struct Claim {
    program_digest: Digest,
    input: Vec<BFieldElement>,
    output: Vec<BFieldElement>,
}

impl Claim {
    // fn fiat_shamir(&self) {
    //     tasm::fiat_shamir_claim(self);
    //     return;
    // }
}
