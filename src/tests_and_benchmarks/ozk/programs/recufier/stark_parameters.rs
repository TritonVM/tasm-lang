use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::{tests_and_benchmarks::ozk::rust_shadows::VmProofIter, triton_vm::prelude::*};
use serde_derive::Serialize;
use tasm_lib::twenty_first::math::traits::PrimitiveRootOfUnity;

/// See [StarkParameters][params].
///
/// [params]: crate::triton_vm::stark::Stark
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

        // This runtime type-conversion prevents a FRI domain of length 2^32 from being created.
        assert!(fri_domain_length <= 1 << 31);

        let generator: BFieldElement =
            BFieldElement::primitive_root_of_unity(fri_domain_length as u64).unwrap();

        return FriVerify {
            expansion_factor: self.fri_expansion_factor as u32,
            num_collinearity_checks: self.num_collinearity_checks as u32,

            // This runtime type-conversion prevents a FRI domain of length 2^32 from being created.
            domain_length: fri_domain_length as u32,
            domain_offset: BFieldElement::generator(),
            domain_generator: generator,
        };
    }
}

pub(crate) struct FriVerify {
    // expansion factor = 1 / rate
    pub expansion_factor: u32,
    pub num_collinearity_checks: u32,
    pub domain_length: u32,
    pub domain_offset: BFieldElement,
    pub domain_generator: BFieldElement,
}

impl FriVerify {
    // This wrapper is probably not necessary; consider removing.
    pub fn verify(&self, proof_iter: &mut VmProofIter) -> Vec<(u32, XFieldElement)> {
        return tasm::tasmlib_verifier_fri_verify(proof_iter, self);
    }
}

#[cfg(test)]
mod test {
    use tasm_lib::triton_vm;

    use super::*;

    #[test]
    fn tvm_agreement() {
        let default_local = StarkParameters::default();
        let default_tvm = triton_vm::stark::Stark::default();

        assert_eq!(default_local.security_level, default_tvm.security_level);
        assert_eq!(
            default_local.fri_expansion_factor,
            default_tvm.fri_expansion_factor
        );
        assert_eq!(
            default_local.num_trace_randomizers,
            default_tvm.num_trace_randomizers
        );
        assert_eq!(
            default_local.num_collinearity_checks,
            default_tvm.num_collinearity_checks
        );
        assert_eq!(
            default_local.num_combination_codeword_checks,
            default_tvm.num_combination_codeword_checks
        );

        let padded_height = 1 << 20;
        let derived_fri_local = default_local.derive_fri(padded_height);
        let derived_fri_tvm = default_tvm.derive_fri(padded_height as usize).unwrap();

        assert_eq!(
            derived_fri_local.expansion_factor as usize,
            derived_fri_tvm.expansion_factor
        );
        assert_eq!(
            derived_fri_local.num_collinearity_checks as usize,
            derived_fri_tvm.num_collinearity_checks
        );
        assert_eq!(
            derived_fri_local.domain_length as usize,
            derived_fri_tvm.domain.length
        );
        assert_eq!(
            derived_fri_local.domain_offset,
            derived_fri_tvm.domain.offset
        );
        assert_eq!(
            derived_fri_local.domain_generator,
            derived_fri_tvm.domain.generator
        );
    }
}
