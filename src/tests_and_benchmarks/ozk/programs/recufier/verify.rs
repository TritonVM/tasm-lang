use num::One;
use num::Zero;
use serde_derive::Serialize;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::tests_and_benchmarks::ozk::rust_shadows::VmProofIter;
use crate::triton_vm::prelude::tip5::Tip5State;
use crate::twenty_first::prelude::*;
use crate::twenty_first::shared_math::traits::PrimitiveRootOfUnity;

/// See [StarkParameters][params].
///
/// [params]: crate::triton_vm::stark::StarkParameters
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize)]
struct StarkParameters {
    pub security_level: usize,
    pub fri_expansion_factor: usize,
    pub num_trace_randomizers: usize,
    pub num_randomizer_polynomials: usize,
    pub num_collinearity_checks: usize,
    pub num_combination_codeword_checks: usize,
}

impl StarkParameters {
    pub fn default() -> StarkParameters {
        return StarkParameters {
            security_level: 160,
            fri_expansion_factor: 4,
            num_trace_randomizers: 166,
            num_randomizer_polynomials: 1,
            num_collinearity_checks: 80,
            num_combination_codeword_checks: 160,
        };
    }

    pub fn small() -> StarkParameters {
        return StarkParameters {
            security_level: 60,
            fri_expansion_factor: 4,
            num_trace_randomizers: 166,
            num_randomizer_polynomials: 1,
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Recufier;

impl Recufier {
    /// Manual encoding of a [`Claim`][claim] containing only a program digest.
    ///
    /// [claim]: crate::triton_vm::prelude::Claim
    pub fn encode_claim(program_digest: Digest) -> Vec<BFieldElement> {
        let mut encoding: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(9);
        encoding.push(BFieldElement::one());
        encoding.push(BFieldElement::zero());
        encoding.push(BFieldElement::one());
        encoding.push(BFieldElement::zero());

        let Digest([elt_0, elt_1, elt_2, elt_3, elt_4]) = program_digest;

        encoding.push(elt_0);
        encoding.push(elt_1);
        encoding.push(elt_2);
        encoding.push(elt_3);
        encoding.push(elt_4);

        return encoding;
    }
}

/// Gives statements only intended for debugging its own scope.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct RecufyDebug;

impl RecufyDebug {
    pub fn dump_u32(thing: u32) {
        tasm::tasm_io_write_to_stdout___u32(thing);
        return;
    }

    pub fn dump_xfe(thing: XFieldElement) {
        tasm::tasm_io_write_to_stdout___xfe(thing);
        return;
    }

    pub fn sponge_state(rate: [BFieldElement; 10]) {
        let mut i: usize = 0;
        while i < 10 {
            tasm::tasm_io_write_to_stdout___bfe(rate[i]);
            i += 1;
        }
        return;
    }
}

pub(crate) struct FriVerify {
    // expansion factor = 1 / rate
    pub expansion_factor: u32,
    pub num_colinearity_checks: u32,
    pub domain_length: u32,
    pub domain_offset: BFieldElement,
    domain_generator: BFieldElement,
}

pub fn recufy() {
    let parameters: Box<StarkParameters> = Box::<StarkParameters>::new(StarkParameters::small());
    let own_digest: Digest = tasm::tasm_recufier_read_and_verify_own_program_digest_from_std_in();

    let mut sponge_state: Tip5State = Tip5::init();
    let encoded_claim: Vec<BFieldElement> = Recufier::encode_claim(own_digest);
    Tip5::pad_and_absorb_all(&mut sponge_state, &encoded_claim);

    let inner_proof_iter: VmProofIter = VmProofIter::new();
    let mut proof_iter: Box<VmProofIter> = Box::<VmProofIter>::new(inner_proof_iter);
    let log_2_padded_height: Box<u32> = proof_iter.next_as_log2paddedheight(&mut sponge_state);
    let padded_height: u32 = 1 << *log_2_padded_height;
    RecufyDebug::dump_u32(padded_height);

    let fri: Box<FriVerify> = Box::<FriVerify>::new(parameters.derive_fri(padded_height));
    let revealed_indexed_leaves: Vec<(u32, XFieldElement)> =
        tasm::tasm_recufier_fri_verify(&mut proof_iter, fri, &mut sponge_state);

    let out_of_domain_base_row: Box<Vec<XFieldElement>> =
        proof_iter.next_as_outofdomainbaserow(&mut sponge_state);
    RecufyDebug::dump_xfe(out_of_domain_base_row[0]);

    RecufyDebug::sponge_state(Tip5::squeeze(&mut sponge_state));
    return;
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use proptest::prelude::*;
    use proptest_arbitrary_interop::arb;
    use test_strategy::proptest;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
    use crate::triton_vm;
    use crate::triton_vm::prelude::Claim;
    use crate::triton_vm::prelude::NonDeterminism;
    use crate::triton_vm::prelude::Program;
    use crate::triton_vm::prelude::Proof;
    use crate::triton_vm::prelude::Stark;
    use crate::triton_vm::proof_item::ProofItem;
    use crate::triton_vm::proof_stream::ProofStream;

    use super::*;

    fn recufier_std_in(test_case: &TritonVMTestCase) -> Vec<BFieldElement> {
        let code = test_case.compile();
        let program = Program::new(&code);
        let program_digest = program.hash::<Tip5>();

        program_digest.reversed().values().to_vec()
    }

    fn proof() -> Proof {
        let to_xfe = |n| XFieldElement::new_u64([n; 3]);
        let dummy_ood_base_row = (1001..1005).map(to_xfe).collect_vec();

        let mut proof_stream = ProofStream::<Tip5>::new();
        proof_stream.enqueue(ProofItem::Log2PaddedHeight(22));
        proof_stream.enqueue(ProofItem::OutOfDomainBaseRow(dummy_ood_base_row));
        proof_stream.into()
    }

    fn recufier_non_determinism() -> NonDeterminism<BFieldElement> {
        let Proof(raw_proof) = proof();
        let ram = raw_proof
            .into_iter()
            .enumerate()
            .map(|(k, v)| (BFieldElement::new(k as u64), v))
            .collect();

        NonDeterminism::default().with_ram(ram)
    }

    #[test]
    fn default_stark_parameters_match_triton_vms_defaults() {
        let vm_sp = triton_vm::stark::StarkParameters::default();
        let sp = StarkParameters::default();

        assert_eq!(vm_sp.security_level, sp.security_level);
        assert_eq!(vm_sp.fri_expansion_factor, sp.fri_expansion_factor);
        assert_eq!(vm_sp.num_trace_randomizers, sp.num_trace_randomizers);

        let vm_num_collin_checks = vm_sp.num_collinearity_checks;
        let num_collin_checks = sp.num_collinearity_checks;
        assert_eq!(vm_num_collin_checks, num_collin_checks);

        let vm_num_rand_polys = vm_sp.num_randomizer_polynomials;
        let num_rand_polys = sp.num_randomizer_polynomials;
        assert_eq!(vm_num_rand_polys, num_rand_polys);

        let vm_num_combi_checks = vm_sp.num_combination_codeword_checks;
        let num_combi_checks = sp.num_combination_codeword_checks;
        assert_eq!(vm_num_combi_checks, num_combi_checks);

        let vm_ps_str = serde_json::to_string(&vm_sp).unwrap();
        let ps_str = serde_json::to_string(&sp).unwrap();
        assert_eq!(vm_ps_str, ps_str);
    }

    #[proptest]
    fn fri_parameter_derivation_using_default_stark_parameters_corresponds_to_triton_vms_derivation(
        #[strategy(0_u32..29)] log_2_padded_height: u32,
    ) {
        let padded_height = 1 << log_2_padded_height;

        let vm_params = triton_vm::stark::StarkParameters::default();
        let vm_fri = Stark::derive_fri(vm_params, padded_height).unwrap();

        let params = StarkParameters::default();
        let fri = params.derive_fri(padded_height as u32);

        prop_assert_eq!(vm_fri.expansion_factor, fri.expansion_factor as usize);
        prop_assert_eq!(
            vm_fri.num_collinearity_checks,
            fri.num_colinearity_checks as usize
        );
        prop_assert_eq!(vm_fri.domain.length as u64, fri.domain_length as u64);
        prop_assert_eq!(vm_fri.domain.offset, fri.domain_offset);
        prop_assert_eq!(vm_fri.domain.generator, fri.domain_generator);
    }

    #[test]
    fn recursive_verification() {
        let entrypoint_location = EntrypointLocation::disk("recufier", "verify", "recufy");
        let test_case = TritonVMTestCase::new(entrypoint_location);
        let std_in = recufier_std_in(&test_case);
        let non_determinism = recufier_non_determinism();

        let native_output =
            rust_shadows::wrap_main_with_io(&recufy)(std_in.clone(), non_determinism.clone());

        let vm_output = test_case
            .with_std_in(std_in)
            .with_non_determinism(non_determinism)
            .execute()
            .unwrap();

        assert_eq!(native_output, vm_output.output);
    }

    #[proptest]
    fn manual_claim_encoding_corresponds_to_actual_encoded_claim_without_input_and_output(
        #[strategy(arb())] program_digest: Digest,
    ) {
        let actual_claim = Claim {
            program_digest,
            input: vec![],
            output: vec![],
        };
        let manual_encoding = Recufier::encode_claim(program_digest);
        prop_assert_eq!(actual_claim.encode(), manual_encoding);
    }
}
