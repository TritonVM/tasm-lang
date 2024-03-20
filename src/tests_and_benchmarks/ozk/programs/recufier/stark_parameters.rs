use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::{tests_and_benchmarks::ozk::rust_shadows::VmProofIter, triton_vm::prelude::*};
use serde_derive::Serialize;
use tasm_lib::twenty_first::shared_math::traits::PrimitiveRootOfUnity;

/// See [StarkParameters][params].
///
/// [params]: crate::triton_vm::stark::StarkParameters
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize)]
pub(crate) struct StarkParameters {
    pub security_level: usize,
    pub fri_expansion_factor: usize,
    pub num_trace_randomizers: usize,
    pub num_collinearity_checks: usize,
    pub num_combination_codeword_checks: usize,
}

impl StarkParameters {
    pub fn default() -> StarkParameters {
        return StarkParameters {
            security_level: 160,
            fri_expansion_factor: 4,
            num_trace_randomizers: 166,
            num_collinearity_checks: 80,
            num_combination_codeword_checks: 160,
        };
    }

    pub fn _small() -> StarkParameters {
        return StarkParameters {
            security_level: 60,
            fri_expansion_factor: 4,
            num_trace_randomizers: 166,
            num_collinearity_checks: 20,
            num_combination_codeword_checks: 60,
        };
    }

    pub fn derive_fri(&self, padded_height: u32) -> FriVerify {
        let interpolant_codeword_length: u32 =
            (padded_height + self.num_trace_randomizers as u32).next_power_of_two();
        let fri_domain_length: usize =
            self.fri_expansion_factor * interpolant_codeword_length as usize;
        let generator: BFieldElement =
            BFieldElement::primitive_root_of_unity(fri_domain_length as u64).unwrap();

        return FriVerify {
            expansion_factor: self.fri_expansion_factor as u32,
            num_colinearity_checks: self.num_collinearity_checks as u32,
            domain_length: fri_domain_length as u32,
            domain_offset: BFieldElement::generator(),
            domain_generator: generator,
        };
    }
}

pub(crate) struct FriVerify {
    // expansion factor = 1 / rate
    pub expansion_factor: u32,
    pub num_colinearity_checks: u32,
    pub domain_length: u32,
    pub domain_offset: BFieldElement,
    pub domain_generator: BFieldElement,
}

impl FriVerify {
    // This wrapper is probably not necessary; consider removing.
    pub fn verify(&self, proof_iter: &mut VmProofIter) -> Vec<(u32, XFieldElement)> {
        return tasm::tasm_recufier_fri_verify(proof_iter, self);
    }
}
