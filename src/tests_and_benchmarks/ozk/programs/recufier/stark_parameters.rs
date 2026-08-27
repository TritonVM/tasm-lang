use serde_derive::Serialize;
use tasm_lib::twenty_first::math::traits::PrimitiveRootOfUnity;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::tests_and_benchmarks::ozk::rust_shadows::VmProofIter;
use crate::triton_vm::prelude::*;

/// See [StarkParameters][params].
///
/// [params]: crate::triton_vm::stark::Stark
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize)]
pub(crate) struct StarkParameters {
    pub security_level: usize,
    pub fri_expansion_factor: usize,
    pub num_trace_randomizers: usize,
    pub num_collinearity_checks: usize,
}

impl StarkParameters {
    pub fn default() -> StarkParameters {
        return StarkParameters {
            security_level: 160,
            fri_expansion_factor: 4,
            num_trace_randomizers: 105,
            num_collinearity_checks: 80,
        };
    }

    pub fn _small() -> StarkParameters {
        return StarkParameters {
            security_level: 60,
            fri_expansion_factor: 4,
            num_trace_randomizers: 105,
            num_collinearity_checks: 20,
        };
    }

    /// The length of the randomized trace domain. See `Stark::randomized_trace_len`
    /// in Triton VM.
    pub fn randomized_trace_len(&self, padded_height: u32) -> u32 {
        let num_trace_randomizers: u32 = self.num_trace_randomizers as u32;
        let mut randomized_trace_len: u32 = padded_height + num_trace_randomizers;

        // padded-height independent lower bounds on the randomized trace length
        let min_len_from_randomizers: u32 = 2 * num_trace_randomizers + 1;
        if randomized_trace_len < min_len_from_randomizers {
            randomized_trace_len = min_len_from_randomizers;
        }
        let min_len_from_quotient_randomizers: u32 =
            (num_trace_randomizers + 1) * StarkParameters::num_randomized_quotient_segments();
        if randomized_trace_len < min_len_from_quotient_randomizers {
            randomized_trace_len = min_len_from_quotient_randomizers;
        }

        return randomized_trace_len.next_power_of_two();
    }

    /// The length of the trace domain. This is exactly half the length of the
    /// randomized trace domain, and can be longer than the padded height.
    pub fn trace_domain_len(&self, padded_height: u32) -> u32 {
        return self.randomized_trace_len(padded_height) >> 1;
    }

    fn num_randomized_quotient_segments() -> u32 {
        return 5;
    }

    pub fn derive_fri(&self, padded_height: u32) -> FriVerify {
        let interpolant_codeword_length: u32 = self.randomized_trace_len(padded_height);
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

        let padded_height = 1 << 20;
        let derived_fri_local = default_local.derive_fri(padded_height);
        let derived_fri_tvm = default_tvm.fri(padded_height as usize).unwrap();

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
            derived_fri_tvm.domain.len()
        );
        assert_eq!(
            derived_fri_local.domain_offset,
            derived_fri_tvm.domain.offset()
        );
        assert_eq!(
            derived_fri_local.domain_generator,
            derived_fri_tvm.domain.generator()
        );
    }

    #[test]
    fn num_randomized_quotient_segments_agrees_with_tvm() {
        assert_eq!(
            triton_vm::table::NUM_RANDOMIZED_QUOTIENT_SEGMENTS,
            StarkParameters::num_randomized_quotient_segments() as usize
        );
    }

    #[test]
    fn trace_domain_len_agrees_with_tvm() {
        let default_local = StarkParameters::default();
        let default_tvm = triton_vm::stark::Stark::default();
        for log2_padded_height in 8..20 {
            let padded_height = 1 << log2_padded_height;
            let fri = default_tvm.fri(padded_height).unwrap();
            let expected_trace_domain_len =
                fri.domain.len() / (2 * default_tvm.fri_expansion_factor);
            assert_eq!(
                expected_trace_domain_len,
                default_local.trace_domain_len(padded_height as u32) as usize
            );
        }
    }
}
