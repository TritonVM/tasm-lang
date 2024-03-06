use num::One;
use num::Zero;
use serde_derive::Serialize;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::tests_and_benchmarks::ozk::rust_shadows::Tip5WithState;
use crate::tests_and_benchmarks::ozk::rust_shadows::VmProofIter;
use crate::triton_vm::table::BaseRow;
use crate::twenty_first::prelude::*;
use crate::twenty_first::shared_math::traits::PrimitiveRootOfUnity;

use super::arithmetic_domain::*;

/// See [StarkParameters][params].
///
/// [params]: crate::triton_vm::stark::StarkParameters
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize)]
struct StarkParameters {
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

    pub fn small() -> StarkParameters {
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

pub(crate) struct Challenges {
    pub challenges: [XFieldElement; 63],
}

impl Challenges {
    const fn count() -> usize {
        return 63;
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Recufier;

impl Recufier {
    /// Manual encoding of a [`Claim`][claim] containing only a program digest.
    ///
    /// [claim]: crate::triton_vm::prelude::Claim
    pub fn encode_claim(program_digest: Digest) -> Vec<BFieldElement> {
        let mut encoding: Vec<BFieldElement> = Vec::<BFieldElement>::default();
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

    const fn num_quotients() -> usize {
        return 591;
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

    pub fn dump_bfe(thing: BFieldElement) {
        tasm::tasm_io_write_to_stdout___bfe(thing);

        return;
    }

    pub fn _dump_xfe(thing: XFieldElement) {
        tasm::tasm_io_write_to_stdout___xfe(thing);

        return;
    }

    pub fn dump_digest(digest: Digest) {
        tasm::tasm_io_write_to_stdout___digest(digest);

        return;
    }

    pub fn dump_xfes(xfes: &Vec<XFieldElement>) {
        let mut i: usize = 0;
        while i < xfes.len() {
            tasm::tasm_io_write_to_stdout___xfe(xfes[i]);
            i += 1;
        }

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
    let own_digest: Digest = tasm::tasm_recufier_read_and_verify_own_program_digest_from_std_in();
    let parameters: Box<StarkParameters> = Box::<StarkParameters>::new(StarkParameters::small());

    Tip5WithState::init();
    let encoded_claim: Vec<BFieldElement> = Recufier::encode_claim(own_digest);
    Tip5WithState::pad_and_absorb_all(&encoded_claim);

    let inner_proof_iter: VmProofIter = VmProofIter::new();
    let mut proof_iter: Box<VmProofIter> = Box::<VmProofIter>::new(inner_proof_iter);
    let log_2_padded_height: Box<u32> = proof_iter.next_as_log2paddedheight();
    let padded_height: u32 = 1 << *log_2_padded_height;
    RecufyDebug::dump_u32(padded_height);

    let fri: Box<FriVerify> = Box::<FriVerify>::new(parameters.derive_fri(padded_height));
    let _merkle_tree_height: usize = fri.domain_length.ilog2() as usize;

    let base_merkle_tree_root: Box<Digest> = proof_iter.next_as_merkleroot();
    RecufyDebug::dump_digest(*base_merkle_tree_root);

    let challenges: Box<Challenges> =
        tasm::tasm_recufier_challenges_new_empty_input_and_output_59_4(own_digest);
    RecufyDebug::dump_xfes(&challenges.challenges.to_vec());

    let extension_tree_merkle_root: Box<Digest> = proof_iter.next_as_merkleroot();
    RecufyDebug::dump_digest(*extension_tree_merkle_root);

    let quot_codeword_weights: Vec<XFieldElement> =
        Tip5WithState::sample_scalars(Recufier::num_quotients());
    RecufyDebug::dump_xfes(&quot_codeword_weights);
    let quotient_codeword_merkle_root: Box<Digest> = proof_iter.next_as_merkleroot();
    RecufyDebug::dump_digest(*quotient_codeword_merkle_root);

    let trace_domain_generator: BFieldElement =
        ArithmeticDomain::generator_for_length(padded_height as u64);
    RecufyDebug::dump_bfe(trace_domain_generator);

    // let revealed_indexed_leaves: Vec<(u32, XFieldElement)> =
    //     tasm::tasm_recufier_fri_verify(&mut proof_iter, fri);

    let out_of_domain_base_row: Box<Box<BaseRow<XFieldElement>>> =
        proof_iter.next_as_outofdomainbaserow();
    RecufyDebug::dump_xfes(&out_of_domain_base_row.to_vec());

    RecufyDebug::sponge_state(Tip5WithState::squeeze());
    return;
}

#[cfg(test)]
mod tests {
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
    use crate::triton_vm::table::NUM_BASE_COLUMNS;

    use super::*;

    fn recufier_std_in(test_case: &TritonVMTestCase) -> Vec<BFieldElement> {
        let code = test_case.compile();
        let program = Program::new(&code);
        let program_digest = program.hash::<Tip5>();

        program_digest.reversed().values().to_vec()
    }

    fn proof() -> Proof {
        let dummy_digest_base_mt = Digest::new([42u64, 43, 44, 45, 46].map(BFieldElement::new));
        let dummy_digest_extension_mt =
            Digest::new([100u64, 101, 102, 103, 104].map(BFieldElement::new));
        let dummy_ood_base_row = [XFieldElement::from(42); NUM_BASE_COLUMNS];
        let dummy_quot_codeword_mt =
            Digest::new([200u64, 201, 202, 203, 204].map(BFieldElement::new));

        let mut proof_stream = ProofStream::<Tip5>::new();
        proof_stream.enqueue(ProofItem::Log2PaddedHeight(22));
        proof_stream.enqueue(ProofItem::MerkleRoot(dummy_digest_base_mt));
        proof_stream.enqueue(ProofItem::MerkleRoot(dummy_digest_extension_mt));
        proof_stream.enqueue(ProofItem::MerkleRoot(dummy_quot_codeword_mt));
        proof_stream.enqueue(ProofItem::OutOfDomainBaseRow(Box::new(dummy_ood_base_row)));
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
    fn local_challenges_count_agrees_with_tvm() {
        assert_eq!(
            triton_vm::table::challenges::Challenges::count(),
            Challenges::count()
        );
    }

    #[test]
    fn default_stark_parameters_match_triton_vms_defaults() {
        let stark = Stark::default();
        let sp = StarkParameters::default();

        assert_eq!(stark.security_level, sp.security_level);
        assert_eq!(stark.fri_expansion_factor, sp.fri_expansion_factor);
        assert_eq!(stark.num_trace_randomizers, sp.num_trace_randomizers);

        let vm_num_collin_checks = stark.num_collinearity_checks;
        let num_collin_checks = sp.num_collinearity_checks;
        assert_eq!(vm_num_collin_checks, num_collin_checks);

        let vm_num_combi_checks = stark.num_combination_codeword_checks;
        let num_combi_checks = sp.num_combination_codeword_checks;
        assert_eq!(vm_num_combi_checks, num_combi_checks);

        let vm_ps_str = serde_json::to_string(&stark).unwrap();
        let ps_str = serde_json::to_string(&sp).unwrap();
        assert_eq!(vm_ps_str, ps_str);
    }

    #[test]
    fn num_quotients_agree_with_tvm_num_quotients() {
        let tvm_num_quotients = triton_vm::table::master_table::num_quotients();
        assert_eq!(tvm_num_quotients, Recufier::num_quotients());
    }

    #[proptest]
    fn fri_parameter_derivation_using_default_stark_parameters_corresponds_to_triton_vms_derivation(
        #[strategy(0_u32..29)] log_2_padded_height: u32,
    ) {
        let padded_height = 1 << log_2_padded_height;

        let stark = Stark::default();
        let vm_fri = stark.derive_fri(padded_height).unwrap();

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
