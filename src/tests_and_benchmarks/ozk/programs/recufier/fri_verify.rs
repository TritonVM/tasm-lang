use tasm_lib::prelude::TasmObject;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::triton_vm::stark::StarkProofStream;
use tasm_lib::triton_vm::table::challenges::Challenges;
use tasm_lib::triton_vm::table::extension_table::Quotientable;
use tasm_lib::triton_vm::table::master_table::MasterExtTable;
use tasm_lib::triton_vm::table::*;

use super::arithmetic_domain::*;

#[derive(Debug, Clone, PartialEq, Eq, BFieldCodec, TasmObject)]
struct FriVerify {
    pub(crate) expansion_factor: u32,
    pub(crate) num_colinearity_checks: u32,
    pub(crate) domain_length: u32,
    pub(crate) domain_offset: BFieldElement,
    domain_generator: BFieldElement,
}

impl FriVerify {
    fn new(
        offset: BFieldElement,
        domain_length: u32,
        expansion_factor: u32,
        num_colinearity_checks: u32,
    ) -> FriVerify {
        let domain: ArithmeticDomain =
            ArithmeticDomain::of_length(domain_length as usize).with_offset(offset);

        return FriVerify {
            expansion_factor,
            num_colinearity_checks,
            domain_length,
            domain_offset: domain.offset,
            domain_generator: domain.generator,
        };
    }
}

fn main() {
    let _a: FriVerify = FriVerify::new(BFieldElement::new(7), 32, 4, 3);

    return;
}

/// Extracts a proof stream that will work for FRI verification from a proof stream that works for
/// the whole STARK verification.
pub(super) fn extract_fri_proof(
    proof_stream: &StarkProofStream,
    claim: &Claim,
) -> StarkProofStream {
    let mut proof_stream = proof_stream.to_owned();
    proof_stream
        .dequeue()
        .unwrap()
        .try_into_log2_padded_height()
        .unwrap();
    proof_stream.alter_fiat_shamir_state_with(claim);

    // Base-table Merkle root
    proof_stream
        .dequeue()
        .unwrap()
        .try_into_merkle_root()
        .unwrap();

    // Extension challenge weights
    proof_stream.sample_scalars(Challenges::SAMPLE_COUNT);

    // Extension-table Merkle root
    proof_stream
        .dequeue()
        .unwrap()
        .try_into_merkle_root()
        .unwrap();

    // Quotient codeword weights
    proof_stream.sample_scalars(MasterExtTable::NUM_CONSTRAINTS);

    // Quotient codeword Merkle root
    proof_stream
        .dequeue()
        .unwrap()
        .try_into_merkle_root()
        .unwrap();

    // Out-of-domain point current row
    proof_stream.sample_scalars(1);

    // Five out-of-domain values
    proof_stream
        .dequeue()
        .unwrap()
        .try_into_out_of_domain_base_row()
        .unwrap();
    proof_stream
        .dequeue()
        .unwrap()
        .try_into_out_of_domain_ext_row()
        .unwrap();
    proof_stream
        .dequeue()
        .unwrap()
        .try_into_out_of_domain_base_row()
        .unwrap();
    proof_stream
        .dequeue()
        .unwrap()
        .try_into_out_of_domain_ext_row()
        .unwrap();
    proof_stream
        .dequeue()
        .unwrap()
        .try_into_out_of_domain_quot_segments()
        .unwrap();

    // `base_and_ext_and_quotient_segment_codeword_weights`
    proof_stream.sample_scalars(NUM_BASE_COLUMNS + NUM_EXT_COLUMNS + NUM_QUOTIENT_SEGMENTS);

    // Deep codeword weights
    const NUM_DEEP_CODEWORD_COMPONENTS: usize = 3;
    proof_stream.sample_scalars(NUM_DEEP_CODEWORD_COMPONENTS);

    proof_stream
}

#[cfg(test)]
mod test {
    use rand::random;
    use tasm_lib::triton_vm;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;

    use super::*;

    #[test]
    fn fri_verify_test() {
        // Rust program on host machine
        let stdin = vec![random(), random(), random(), random()];
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        // Run test on Triton-VM
        let entrypoint_location = EntrypointLocation::disk("recufier", "fri_verify", "main");
        let test_program = ozk_parsing::compile_for_test(&entrypoint_location);

        let expected_stack_diff = 0;
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            Default::default(),
            Default::default(),
            expected_stack_diff,
        )
        .unwrap();

        assert_eq!(native_output, vm_output.output);
    }

    #[test]
    fn extract_fri_proof_works() {
        let simple_program = triton_program!(halt);
        let public_input = [];
        let non_determinism = NonDeterminism::default();
        let (stark, claim, proof) =
            triton_vm::prove_program(&simple_program, &public_input, &non_determinism).unwrap();
        let padded_height = proof.padded_height().unwrap();
        let fri = stark.derive_fri(padded_height).unwrap();

        let proof_stream = StarkProofStream::try_from(&proof).unwrap();
        let mut fri_proof_stream = extract_fri_proof(&proof_stream, &claim);
        assert!(
            fri.verify(&mut fri_proof_stream, &mut None).is_ok(),
            "Proof must verify"
        );
    }
}
