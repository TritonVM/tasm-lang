use itertools::Itertools;
use num::One;
use num::Zero;
use tasm_lib::triton_vm::table::extension_table::Quotientable;
use tasm_lib::triton_vm::table::master_table::MasterExtTable;
use tasm_lib::triton_vm::table::ExtensionRow;
use tasm_lib::triton_vm::table::QuotientSegments;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::tests_and_benchmarks::ozk::rust_shadows::Tip5WithState;
use crate::tests_and_benchmarks::ozk::rust_shadows::VmProofIter;
use crate::triton_vm::table::BaseRow;
use crate::twenty_first::prelude::*;

use super::arithmetic_domain::*;
use super::challenges::*;
use super::claim::*;
use super::stark_parameters::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Recufier;

impl Recufier {
    fn verify(claim: &Claim) {
        let parameters: Box<StarkParameters> =
            Box::<StarkParameters>::new(StarkParameters::default());

        Tip5WithState::init();

        tasm::tasm_recufier_claim_instantiate_fiat_shamir_with_claim(claim);

        // For `tasm-lang` `VmProofIter` comes from the `recufy` library. For rustc in
        // comes from `rust-shadowing`.
        let inner_proof_iter: VmProofIter = VmProofIter::new();
        let mut proof_iter: Box<VmProofIter> = Box::<VmProofIter>::new(inner_proof_iter);
        let log_2_padded_height: Box<u32> = proof_iter.next_as_log2paddedheight();
        let padded_height: u32 = 1 << *log_2_padded_height;

        let fri: Box<FriVerify> = Box::<FriVerify>::new(parameters.derive_fri(padded_height));

        let base_merkle_tree_root: Box<Digest> = proof_iter.next_as_merkleroot();

        // The next function returns a pointer, but the value of this pointer is known statically. So
        // whenever the challenges are needed (which is for constraint evaluation), we can assume their
        // location and do not need to read this variable.
        let _challenges: Box<Challenges> =
            tasm::tasm_recufier_challenges_new_generic_dyn_claim_59_4(claim);

        let extension_tree_merkle_root: Box<Digest> = proof_iter.next_as_merkleroot();

        let quot_codeword_weights: [XFieldElement; 596] = <[XFieldElement; 596]>::try_from(
            Tip5WithState::sample_scalars(Recufier::num_quotients()),
        )
        .unwrap();
        let quotient_tree_merkle_root: Box<Digest> = proof_iter.next_as_merkleroot();

        let trace_domain_generator: BFieldElement =
            ArithmeticDomain::generator_for_length(padded_height as u64);

        let ___out_of_domain_point_curr_row: Vec<XFieldElement> = Tip5WithState::sample_scalars(1);
        let out_of_domain_point_curr_row: XFieldElement = ___out_of_domain_point_curr_row[0];
        let out_of_domain_point_next_row: XFieldElement =
            out_of_domain_point_curr_row * trace_domain_generator;
        let out_of_domain_point_curr_row_pow_num_segments: XFieldElement =
            tasm::tasm_arithmetic_xfe_to_the_fourth(out_of_domain_point_curr_row);

        let out_of_domain_curr_base_row: Box<Box<BaseRow<XFieldElement>>> =
            proof_iter.next_as_outofdomainbaserow();
        let out_of_domain_curr_ext_row: Box<Box<ExtensionRow>> =
            proof_iter.next_as_outofdomainextrow();
        let out_of_domain_next_base_row: Box<Box<BaseRow<XFieldElement>>> =
            proof_iter.next_as_outofdomainbaserow();
        let out_of_domain_next_ext_row: Box<Box<ExtensionRow>> =
            proof_iter.next_as_outofdomainextrow();
        let out_of_domain_curr_row_quot_segments: Box<[XFieldElement; 4]> =
            proof_iter.next_as_outofdomainquotientsegments();

        let quotient_summands: [XFieldElement; 596] = Recufier::quotient_summands(
            out_of_domain_point_curr_row,
            padded_height,
            trace_domain_generator,
        );

        let out_of_domain_quotient_value: XFieldElement =
            tasm::tasm_array_inner_product_of_596_xfes(quot_codeword_weights, quotient_summands);

        let sum_of_evaluated_out_of_domain_quotient_segments: XFieldElement =
            tasm::tasm_array_horner_evaluation_with_4_coefficients(
                *out_of_domain_curr_row_quot_segments,
                out_of_domain_point_curr_row,
            );

        assert!(sum_of_evaluated_out_of_domain_quotient_segments == out_of_domain_quotient_value);

        // Fiat-shamir 2
        let mut base_and_ext_codeword_weights: Vec<XFieldElement> = Tip5WithState::sample_scalars(
            Recufier::num_base_and_ext_and_quotient_segment_codeword_weights(),
        );

        // Split off the weights for the quotients
        let quotient_segment_codeword_weights: [XFieldElement; 4] = <[XFieldElement; 4]>::try_from(
            base_and_ext_codeword_weights.split_off(Recufier::num_columns()),
        )
        .unwrap();

        // sum out-of-domain values
        let out_of_domain_curr_row_base_and_ext_value: XFieldElement =
            Recufier::linearly_sum_xfe_base_and_ext_row(
                out_of_domain_curr_base_row,
                out_of_domain_curr_ext_row,
                &base_and_ext_codeword_weights,
            );
        let out_of_domain_next_row_base_and_ext_value: XFieldElement =
            Recufier::linearly_sum_xfe_base_and_ext_row(
                out_of_domain_next_base_row,
                out_of_domain_next_ext_row,
                &base_and_ext_codeword_weights,
            );
        let out_of_domain_curr_row_quotient_segment_value: XFieldElement =
            tasm::tasm_array_inner_product_of_4_xfes(
                quotient_segment_codeword_weights,
                *out_of_domain_curr_row_quot_segments,
            );

        // Fiat-Shamir 3
        let deep_codeword_weights: [XFieldElement; 3] =
            <[XFieldElement; 3]>::try_from(Tip5WithState::sample_scalars(3)).unwrap();

        // FRI
        let revealed_fri_indices_and_elements: Vec<(u32, XFieldElement)> =
            fri.verify(&mut proof_iter);

        // Check leafs
        // Dequeue base elements
        // Could be read from secret-in, but it's much more efficient to get them from memory
        let num_combination_codeword_checks: usize = 2 * fri.num_collinearity_checks as usize;
        let base_table_rows: Box<Vec<BaseRow<BFieldElement>>> =
            proof_iter.next_as_masterbasetablerows();

        // Read base authentication structure but ignore its value, as we divine-in the digests instead
        {
            let _dummy: Box<Vec<Digest>> = proof_iter.next_as_authenticationstructure();
        }

        // hash base rows to get leafs
        let mut leaf_digests_base: Vec<Digest> = Vec::<Digest>::default();
        {
            let mut i: usize = 0;
            while i < num_combination_codeword_checks {
                leaf_digests_base.push(tasm::tasm_hashing_algebraic_hasher_hash_varlen(
                    &base_table_rows[i],
                    356,
                ));
                i += 1;
            }
        }

        // Merkle verify (base tree)
        let merkle_tree_height: u32 = fri.domain_length.ilog2();
        {
            let mut i: usize = 0;
            while i < num_combination_codeword_checks {
                tasm::tasm_hashing_merkle_verify(
                    *base_merkle_tree_root,
                    revealed_fri_indices_and_elements[i].0,
                    leaf_digests_base[i],
                    merkle_tree_height,
                );
                i += 1;
            }
        }

        // dequeue extension elements
        let ext_table_rows: Box<Vec<ExtensionRow>> = proof_iter.next_as_masterexttablerows();

        // dequeue extension rows' authentication structure but ignore it (divination instead)
        {
            let _dummy: Box<Vec<Digest>> = proof_iter.next_as_authenticationstructure();
        }

        // hash extension rows to get leafs
        let mut leaf_digests_ext: Vec<Digest> = Vec::<Digest>::default();
        {
            let mut i: usize = 0;
            while i < num_combination_codeword_checks {
                leaf_digests_ext.push(tasm::tasm_hashing_algebraic_hasher_hash_varlen(
                    &ext_table_rows[i],
                    83 * 3,
                ));
                i += 1;
            }
        }

        // Merkle verify (extension tree)
        {
            let mut i: usize = 0;
            while i < num_combination_codeword_checks {
                tasm::tasm_hashing_merkle_verify(
                    *extension_tree_merkle_root,
                    revealed_fri_indices_and_elements[i].0,
                    leaf_digests_ext[i],
                    merkle_tree_height,
                );
                i += 1;
            }
        }

        // dequeue quotient segments
        let quotient_segment_elements: Box<Vec<QuotientSegments>> =
            proof_iter.next_as_quotientsegmentselements();

        // hash rows
        let mut leaf_digests_quot: Vec<Digest> = Vec::<Digest>::default();
        {
            let mut i: usize = 0;
            while i < num_combination_codeword_checks {
                leaf_digests_quot.push(tasm::tasm_hashing_algebraic_hasher_hash_varlen(
                    &quotient_segment_elements[i],
                    4 * 3,
                ));
                i += 1;
            }
        }

        // Merkle verify (quotient tree)
        {
            let mut i: usize = 0;
            while i < num_combination_codeword_checks {
                tasm::tasm_hashing_merkle_verify(
                    *quotient_tree_merkle_root,
                    revealed_fri_indices_and_elements[i].0,
                    leaf_digests_quot[i],
                    merkle_tree_height,
                );
                i += 1;
            }
        }

        // Linear combination
        // Some of these checks may be redundant, but this is what the verifier in TVM does
        assert!(num_combination_codeword_checks == revealed_fri_indices_and_elements.len());
        assert!(num_combination_codeword_checks == base_table_rows.len());
        assert!(num_combination_codeword_checks == ext_table_rows.len());
        assert!(num_combination_codeword_checks == quotient_segment_elements.len());

        // Main loop
        let trace_weights: [XFieldElement; 439] =
            <[XFieldElement; 439]>::try_from(base_and_ext_codeword_weights).unwrap();
        {
            let mut i: usize = 0;
            while i < num_combination_codeword_checks {
                // let randomizer_value: XFieldElement = ext_row[ext_row.len() - 1];
                let current_fri_domain_value: BFieldElement = fri.domain_offset
                    * fri
                        .domain_generator
                        .mod_pow_u32(revealed_fri_indices_and_elements[i].0);

                let base_and_ext_opened_row_element: XFieldElement =
                    tasm::tasm_array_inner_product_of_three_rows_with_weights(
                        trace_weights,
                        base_table_rows[i],
                        ext_table_rows[i],
                    );

                let quot_curr_row_deep_value: XFieldElement =
                    (out_of_domain_curr_row_quotient_segment_value
                        - tasm::tasm_array_inner_product_of_4_xfes(
                            quotient_segment_codeword_weights,
                            quotient_segment_elements[i],
                        ))
                        / (out_of_domain_point_curr_row_pow_num_segments
                            - current_fri_domain_value);

                let deep_value: XFieldElement = (out_of_domain_curr_row_base_and_ext_value
                    - base_and_ext_opened_row_element)
                    / (out_of_domain_point_curr_row - current_fri_domain_value)
                    * deep_codeword_weights[0]
                    + (out_of_domain_next_row_base_and_ext_value - base_and_ext_opened_row_element)
                        / (out_of_domain_point_next_row - current_fri_domain_value)
                        * deep_codeword_weights[1]
                    + quot_curr_row_deep_value * deep_codeword_weights[2];

                assert!(
                    revealed_fri_indices_and_elements[i].1 == deep_value + XFieldElement::zero()
                );

                i += 1;
            }
        }

        return;
    }

    const fn num_quotients() -> usize {
        return 596;
    }

    const fn constraint_evaluation_lengths() -> [usize; 4] {
        return [81usize, 94, 398, 23];
    }

    const fn constraint_evaluation_lengths_running_sum() -> [usize; 4] {
        let mut running_sum: usize = 0;
        let mut lengths: [usize; 4] = Recufier::constraint_evaluation_lengths();
        let mut i: usize = 0;
        while i < 4 {
            running_sum += lengths[i];
            lengths[i] = running_sum;
            i += 1;
        }

        return lengths;
    }

    /// Evaluate AIR constraintd and multiply with the correct inverse of zerofier.
    fn quotient_summands(
        out_of_domain_point_curr_row: XFieldElement,
        padded_height: u32,
        trace_domain_generator: BFieldElement,
    ) -> [XFieldElement; 596] {
        let initial_zerofier_inv: XFieldElement =
            (out_of_domain_point_curr_row - BFieldElement::one()).inverse();
        let consistency_zerofier_inv: XFieldElement =
            (out_of_domain_point_curr_row.mod_pow_u32(padded_height) - BFieldElement::one())
                .inverse();
        let except_last_row: XFieldElement =
            out_of_domain_point_curr_row - trace_domain_generator.inverse();
        let transition_zerofier_inv: XFieldElement = except_last_row * consistency_zerofier_inv;
        let terminal_zerofier_inv: XFieldElement = except_last_row.inverse();

        // Along with the challenges, the out-of-domain rows ({base,ext}*{curr,next}) are stored at
        // a statically-known location; those locations are assumed by the next function call.
        let mut evaluated_constraints: [XFieldElement; 596] =
            tasm::tasm_recufier_master_ext_table_air_constraint_evaluation();

        let categories_running_sum_lengths: [usize; 4] =
            Recufier::constraint_evaluation_lengths_running_sum();
        let mut i: usize = 0;
        while i < categories_running_sum_lengths[0] {
            evaluated_constraints[i] *= initial_zerofier_inv;
            i += 1;
        }
        while i < categories_running_sum_lengths[1] {
            evaluated_constraints[i] *= consistency_zerofier_inv;
            i += 1;
        }
        while i < categories_running_sum_lengths[2] {
            evaluated_constraints[i] *= transition_zerofier_inv;
            i += 1;
        }
        while i < categories_running_sum_lengths[3] {
            evaluated_constraints[i] *= terminal_zerofier_inv;
            i += 1;
        }

        return evaluated_constraints;
    }

    const fn num_base_and_ext_and_quotient_segment_codeword_weights() -> usize {
        return 443;
    }

    fn num_columns() -> usize {
        return 439;
    }

    #[allow(clippy::boxed_local)]
    #[allow(clippy::redundant_allocation)]
    #[allow(clippy::ptr_arg)]
    fn linearly_sum_xfe_base_and_ext_row(
        base_row: Box<Box<BaseRow<XFieldElement>>>,
        ext_row: Box<Box<ExtensionRow>>,
        base_and_ext_codeword_weights: &Vec<XFieldElement>,
    ) -> XFieldElement {
        let mut acc: XFieldElement = XFieldElement::zero();
        let mut i: usize = 0;
        while i < base_row.len() {
            acc += base_and_ext_codeword_weights[i] * base_row[i];
            i += 1;
        }

        i = 0;
        while i < ext_row.len() {
            acc += ext_row[i] * base_and_ext_codeword_weights[i + base_row.len()];
            i += 1;
        }

        return acc;
    }
}

/// Gives statements only intended for debugging its own scope.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct RecufyDebug;

impl RecufyDebug {
    pub fn _dump_u32(thing: u32) {
        tasm::tasm_io_write_to_stdout___u32(thing);

        return;
    }

    pub fn _dump_bfe(thing: BFieldElement) {
        tasm::tasm_io_write_to_stdout___bfe(thing);

        return;
    }

    pub fn _dump_xfe(thing: XFieldElement) {
        tasm::tasm_io_write_to_stdout___xfe(thing);

        return;
    }

    pub fn _dump_digest(digest: Digest) {
        tasm::tasm_io_write_to_stdout___digest(digest);

        return;
    }

    #[allow(clippy::ptr_arg)]
    pub fn _dump_bfes(bfes: &Vec<BFieldElement>) {
        let mut i: usize = 0;
        while i < bfes.len() {
            tasm::tasm_io_write_to_stdout___bfe(bfes[i]);
            i += 1;
        }

        return;
    }

    #[allow(clippy::ptr_arg)]
    pub fn _dump_xfes(xfes: &Vec<XFieldElement>) {
        let mut i: usize = 0;
        while i < xfes.len() {
            tasm::tasm_io_write_to_stdout___xfe(xfes[i]);
            i += 1;
        }

        return;
    }

    #[allow(clippy::ptr_arg)]
    pub fn _dump_digests(digests: &Vec<Digest>) {
        let mut i: usize = 0;
        while i < digests.len() {
            tasm::tasm_io_write_to_stdout___digest(digests[i]);
            i += 1;
        }

        return;
    }

    pub fn _sponge_state(rate: [BFieldElement; 10]) {
        let mut i: usize = 0;
        while i < 10 {
            tasm::tasm_io_write_to_stdout___bfe(rate[i]);
            i += 1;
        }

        return;
    }
}

#[cfg(test)]
mod test {
    use proptest::prelude::*;
    use tasm_lib::triton_vm::program::Program;
    use tasm_lib::triton_vm::stark::StarkProofStream;
    use tasm_lib::triton_vm::table::NUM_EXT_COLUMNS;
    use tasm_lib::triton_vm::table::NUM_QUOTIENT_SEGMENTS;
    use tasm_lib::triton_vm::triton_program;
    use test_strategy::proptest;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::programs::recufier::fri_verify::test::extract_fri_proof;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
    use crate::triton_vm;
    use crate::triton_vm::prelude::NonDeterminism;
    use crate::triton_vm::prelude::Proof;
    use crate::triton_vm::prelude::Stark;
    use crate::triton_vm::table::NUM_BASE_COLUMNS;

    use super::*;

    fn verify_stark_proof() {
        // Notice that this function is dual-compiled: By rustc and by this compiler.
        let program_digest: Digest = tasm::tasm_io_read_stdin___digest();

        let input_length: usize = tasm::tasm_io_read_stdin___u32() as usize;
        let mut input: Vec<BFieldElement> = Vec::<BFieldElement>::default();
        {
            let mut i: usize = 0;
            while i < input_length {
                input.push(tasm::tasm_io_read_stdin___bfe());
                i += 1;
            }
        }

        let output_length: usize = tasm::tasm_io_read_stdin___u32() as usize;
        let mut output: Vec<BFieldElement> = Vec::<BFieldElement>::default();
        {
            let mut i: usize = 0;
            while i < output_length {
                output.push(tasm::tasm_io_read_stdin___bfe());
                i += 1;
            }
        }

        #[allow(clippy::redundant_field_names)]
        let claim: Box<Claim> = Box::<Claim>::new(Claim {
            program_digest,
            input: input,
            output: output,
        });

        return Recufier::verify(&claim);
    }

    /// Return the claim encoded as a list of BFieldElements in the format that
    /// `verify_stark_proof` expects.
    pub(super) fn claim_to_stdin_for_stark_verifier(
        claim: &triton_vm::proof::Claim,
    ) -> Vec<BFieldElement> {
        let mut ret = claim.program_digest.reversed().values().to_vec();
        ret.extend(claim.input.encode());
        ret.extend(claim.output.encode());

        ret
    }

    /// Return `NonDeterminism` required for the `verify` function as well as the claim and the
    // padded-height for the associated proof, the proof that is to be verified.
    pub(super) fn non_determinism_for_verify_and_claim_and_padded_height(
        program: &Program,
        public_input: &[BFieldElement],
        non_determinism: NonDeterminism<BFieldElement>,
    ) -> (
        NonDeterminism<BFieldElement>,
        triton_vm::proof::Claim,
        usize,
    ) {
        // TODO: Delete this function once `u64` types are removed from TVM interface
        fn nd_bf_to_u64(nd: NonDeterminism<BFieldElement>) -> NonDeterminism<u64> {
            let individual_tokens = nd
                .individual_tokens
                .iter()
                .map(|&element| element.into())
                .collect();
            let ram = nd
                .ram
                .iter()
                .map(|(&key, &value)| (key.into(), value.into()))
                .collect();
            NonDeterminism {
                individual_tokens,
                digests: nd.digests.clone(),
                ram,
            }
        }

        println!("Generating proof for non-determinism");
        let (stark, claim, proof) = triton_vm::prove_program(
            program,
            &public_input.iter().map(|x| x.value()).collect_vec(),
            &nd_bf_to_u64(non_determinism),
        )
        .unwrap();
        println!("Done generating proof for non-determinism");

        assert!(
            triton_vm::verify(stark, &claim, &proof),
            "Proof from TVM must verify through TVM"
        );

        let fri = stark.derive_fri(proof.padded_height().unwrap()).unwrap();

        let proof_stream = StarkProofStream::try_from(&proof).unwrap();
        let proof_extraction = extract_fri_proof(&proof_stream, &claim, stark);
        let tasm_lib_fri: tasm_lib::recufier::fri_verify::FriVerify = fri.into();
        let fri_proof_digests =
            tasm_lib_fri.extract_digests_required_for_proving(&proof_extraction.fri_proof_stream);
        let padded_height = proof.padded_height().unwrap();
        let Proof(raw_proof) = proof;
        let ram = raw_proof
            .into_iter()
            .enumerate()
            .map(|(k, v)| (BFieldElement::new(k as u64), v))
            .collect();

        let nd_digests = [
            fri_proof_digests,
            proof_extraction
                .base_tree_authentication_paths
                .into_iter()
                .flatten()
                .collect_vec(),
            proof_extraction
                .ext_tree_authentication_paths
                .into_iter()
                .flatten()
                .collect_vec(),
            proof_extraction
                .quot_tree_authentication_paths
                .into_iter()
                .flatten()
                .collect_vec(),
        ]
        .concat();

        (
            NonDeterminism::default()
                .with_ram(ram)
                .with_digests(nd_digests),
            claim,
            padded_height,
        )
    }

    #[test]
    fn local_category_count_agrees_with_tvm() {
        assert_eq!(
            Recufier::constraint_evaluation_lengths(),
            [
                MasterExtTable::NUM_INITIAL_CONSTRAINTS,
                MasterExtTable::NUM_CONSISTENCY_CONSTRAINTS,
                MasterExtTable::NUM_TRANSITION_CONSTRAINTS,
                MasterExtTable::NUM_TERMINAL_CONSTRAINTS
            ]
        );
    }

    #[test]
    fn num_base_and_ext_and_quotient_segment_codeword_weights_agrees_with_tvm() {
        assert_eq!(
            NUM_BASE_COLUMNS + NUM_EXT_COLUMNS + NUM_QUOTIENT_SEGMENTS,
            Recufier::num_base_and_ext_and_quotient_segment_codeword_weights()
        )
    }

    #[test]
    fn num_columns_agrees_with_tvm() {
        assert_eq!(Recufier::num_columns(), NUM_BASE_COLUMNS + NUM_EXT_COLUMNS)
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
            fri.num_collinearity_checks as usize
        );
        prop_assert_eq!(vm_fri.domain.length as u64, fri.domain_length as u64);
        prop_assert_eq!(vm_fri.domain.offset, fri.domain_offset);
        prop_assert_eq!(vm_fri.domain.generator, fri.domain_generator);
    }

    #[test]
    fn verify_tvm_proof_factorial_program_no_io() {
        const FACTORIAL_ARGUMENT: u32 = 3;
        let factorial_program = factorial_program_no_io(FACTORIAL_ARGUMENT);
        let entrypoint_location =
            EntrypointLocation::disk("recufier", "verify", "test::verify_stark_proof");
        let test_case = TritonVMTestCase::new(entrypoint_location);
        let (non_determinism, claim_for_proof, inner_padded_height) =
            non_determinism_for_verify_and_claim_and_padded_height(
                &factorial_program,
                &[],
                NonDeterminism::default(),
            );
        let verifier_std_in = claim_to_stdin_for_stark_verifier(&claim_for_proof);

        let verifier_program = test_case.program();
        let native_output = rust_shadows::wrap_main_with_io_and_program_digest(&verify_stark_proof)(
            verifier_std_in.to_vec(),
            non_determinism.clone(),
            verifier_program.clone(),
        );

        let final_vm_state = test_case
            .with_non_determinism(non_determinism.clone())
            .with_std_in(verifier_std_in.to_vec())
            .execute()
            .unwrap();

        assert_eq!(native_output, final_vm_state.public_output);

        println!(
            "Clock cycle count of TASM-verifier of factorial({FACTORIAL_ARGUMENT}): {}.\nInner padded height was: {}",
            final_vm_state.cycle_count,
            inner_padded_height,
        );

        if std::env::var("DYING_TO_PROVE").is_ok() {
            let verifier_std_out = [];
            tasm_lib::prove_and_verify(
                &verifier_program,
                &verifier_std_in,
                &non_determinism,
                &verifier_std_out,
                None,
            );
        }
    }

    #[test]
    fn verify_tvm_proof_factorial_program_with_io_fact_40() {
        verify_tvm_proof_factorial_program_with_io_prop(40);
    }

    /// Verify the proof of the execution of a factorial program. Returns the padded height of the
    /// factorial program execution (the execution that the proof pertains to), and the clock cycle
    /// count of the verification program.
    fn verify_tvm_proof_factorial_program_with_io_prop(factorial_argument: u64) -> (usize, u32) {
        // Read $n$ from stdin and output $n!$!
        let factorial_program = factorial_program_with_io();

        verify_tvm_proof_prop(
            &factorial_program,
            &format!("factorial({factorial_argument})"),
            &[BFieldElement::new(factorial_argument)],
        )
    }

    fn verify_tvm_proof_prop(
        inner_program: &Program,
        inner_program_name: &str,
        input: &[BFieldElement],
    ) -> (usize, u32) {
        let entrypoint_location =
            EntrypointLocation::disk("recufier", "verify", "test::verify_stark_proof");
        let test_case = TritonVMTestCase::new(entrypoint_location);

        let (non_determinism, claim_relating_to_proof, inner_padded_height) =
            non_determinism_for_verify_and_claim_and_padded_height(
                inner_program,
                input,
                NonDeterminism::default(),
            );
        let verifier_std_in = claim_to_stdin_for_stark_verifier(&claim_relating_to_proof);

        let verifier_program = test_case.program();
        let native_output = rust_shadows::wrap_main_with_io_and_program_digest(&verify_stark_proof)(
            verifier_std_in.to_vec(),
            non_determinism.clone(),
            verifier_program.clone(),
        );

        let final_vm_state = test_case
            .with_non_determinism(non_determinism.clone())
            .with_std_in(verifier_std_in.to_vec())
            .execute()
            .unwrap();

        assert_eq!(native_output, final_vm_state.public_output);

        println!(
            "Clock cycle count of TASM-verifier of proof from {inner_program_name} execution : {}.\nInner padded height was: {}",
            final_vm_state.cycle_count,
            inner_padded_height
        );

        if std::env::var("DYING_TO_PROVE").is_ok() {
            let verifier_std_out = [];
            tasm_lib::prove_and_verify(
                &verifier_program,
                &verifier_std_in,
                &non_determinism,
                &verifier_std_out,
                None,
            );
        }

        (inner_padded_height, final_vm_state.cycle_count)
    }

    pub(super) fn factorial_program_with_io() -> Program {
        triton_program!(
            read_io 1
            push 1               // n accumulator
            call factorial       // 0 accumulator!
            write_io 1
            halt

            factorial:           // n acc
                // if n == 0: return
                dup 1            // n acc n
                push 0 eq        // n acc n==0
                skiz             // n acc
                return           // 0 acc
                // else: multiply accumulator with n and recurse
                dup 1            // n acc n
                mul              // n acc·n
                swap 1           // acc·n n
                push -1 add      // acc·n n-1
                swap 1           // n-1 acc·n

                recurse
        )
    }

    pub(super) fn factorial_program_no_io(factorial_arg: u32) -> Program {
        triton_program!(
            push {factorial_arg} // n
            push 1               // n accumulator
            call factorial       // 0 accumulator!
            halt

            factorial:           // n acc
                // if n == 0: return
                dup 1            // n acc n
                push 0 eq        // n acc n==0
                skiz             // n acc
                return           // 0 acc
                // else: multiply accumulator with n and recurse
                dup 1            // n acc n
                mul              // n acc·n
                swap 1           // acc·n n
                push -1 add      // acc·n n-1
                swap 1           // n-1 acc·n

                recurse
        )
    }
}

#[cfg(test)]
mod benches {
    use tasm_lib::snippet_bencher::write_benchmarks;
    use tasm_lib::snippet_bencher::NamedBenchmarkResult;
    use tasm_lib::triton_vm::program::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::programs::recufier::verify::test::claim_to_stdin_for_stark_verifier;
    use crate::tests_and_benchmarks::ozk::programs::recufier::verify::test::factorial_program_no_io;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    use super::test::non_determinism_for_verify_and_claim_and_padded_height;

    #[ignore = "Intended to generate data about verifier table heights as a function of inner padded
       height Make sure to run with `RUSTFLAGS=\"-C opt-level=3 -C debug-assertions=no`"]
    #[test]
    fn benchmark_verification_as_a_function_of_inner_padded_height() {
        for (fact_arg, expected_inner_padded_height) in [
            (10, 1 << 8),
            (40, 1 << 9),
            (80, 1 << 10),
            (100, 1 << 11),
            (200, 1 << 12),
            (400, 1 << 13),
            (800, 1 << 14),
            (1600, 1 << 15),
            (3200, 1 << 16),
            (6400, 1 << 17),
            (12800, 1 << 18),
            (25600, 1 << 19),
            (51200, 1 << 20),
            (102400, 1 << 21),
        ] {
            benchmark_verifier(fact_arg, expected_inner_padded_height);
        }
    }

    fn benchmark_verifier(factorial_argument: u32, expected_inner_padded_height: usize) {
        let factorial_program = factorial_program_no_io(factorial_argument);
        let entrypoint_location =
            EntrypointLocation::disk("recufier", "verify", "test::verify_stark_proof");
        let (non_determinism, claim_for_proof, inner_padded_height) =
            non_determinism_for_verify_and_claim_and_padded_height(
                &factorial_program,
                &[],
                NonDeterminism::default(),
            );
        let verifier_std_in = claim_to_stdin_for_stark_verifier(&claim_for_proof);
        assert_eq!(expected_inner_padded_height, inner_padded_height);

        let test_case = TritonVMTestCase::new(entrypoint_location)
            .with_non_determinism(non_determinism)
            .with_std_in(verifier_std_in);
        let benchmark_result = test_case.benchmark();
        let named_benchmark_result = NamedBenchmarkResult {
            name: format!("verification_with_inner_padded_height_{inner_padded_height}"),
            benchmark_result,
            case: tasm_lib::snippet_bencher::BenchmarkCase::CommonCase,
        };
        write_benchmarks(vec![named_benchmark_result]);
    }
}

#[cfg(test)]
mod profilers {
    use tasm_lib::triton_vm::program::NonDeterminism;
    use tasm_lib::triton_vm::program::Program;
    use tasm_lib::twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::programs::recufier::verify::test::claim_to_stdin_for_stark_verifier;
    use crate::tests_and_benchmarks::ozk::programs::recufier::verify::test::factorial_program_no_io;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    use super::test::factorial_program_with_io;
    use super::test::non_determinism_for_verify_and_claim_and_padded_height;

    #[test]
    fn profile_verify_factorial_program_inner_padded_height_256() {
        generate_profile_for_verifier_execution_for_factorial_execution_proof(3, 256);
    }

    #[test]
    fn profile_verify_factorial_program_inner_padded_height_1024() {
        generate_profile_for_verifier_execution_for_factorial_execution_proof(80, 1024);
    }

    #[ignore = "Rather slow, almost 20 seconds on my powerful laptop"]
    #[test]
    fn profile_verify_factorial_program_inner_padded_height_4096() {
        generate_profile_for_verifier_execution_for_factorial_execution_proof(200, 4096);
    }

    #[ignore = "Intended to generate data about verifier clock cycle count as a function of padded height"]
    #[test]
    fn generate_many_verifier_execution_profiles() {
        for (fact_arg, expected_inner_padded_height) in [
            (10, 1 << 8),
            (40, 1 << 9),
            (80, 1 << 10),
            (100, 1 << 11),
            (200, 1 << 12),
            (400, 1 << 13),
            (800, 1 << 14),
            (1600, 1 << 15),
            (3200, 1 << 16),
            (6400, 1 << 17),
            (12800, 1 << 18),
            (25600, 1 << 19),
            (51200, 1 << 20),
            (102400, 1 << 21),
        ] {
            generate_profile_for_verifier_execution_for_factorial_execution_proof(
                fact_arg,
                expected_inner_padded_height,
            );
        }
    }

    #[ignore = "Requires 400 GB of RAM. Make sure to run with `RUSTFLAGS=\"-C opt-level=3 -C debug-assertions=no`"]
    #[test]
    fn recursive_proof_verification() {
        // Three executions are used here:
        // - inner_inner_program: Calculates `fact(102400)` and generates proof of this execution
        // - inner_program:       Verifies proof for `fact(102400)` execution, generates proof
        // - outer_program:       Verifies proof for `inner_program`
        const EXPECTED_PADDED_HEIGHT: usize = 2usize.pow(21);
        let inner_inner_program = factorial_program_with_io();
        let inner_inner_stdin = [BFieldElement::new(102400)];
        println!("\nGenerating 1st proof");
        let (inner_nd, inner_inner_claim, inner_inner_padded_height) =
            non_determinism_for_verify_and_claim_and_padded_height(
                &inner_inner_program,
                &inner_inner_stdin,
                NonDeterminism::default(),
            );
        println!("Done generating 1st proof");
        assert_eq!(EXPECTED_PADDED_HEIGHT, inner_inner_padded_height);

        let inner_stdin = claim_to_stdin_for_stark_verifier(&inner_inner_claim);

        let main_function_name = "verify_stark_proof";
        let entrypoint_location =
            EntrypointLocation::disk("recufier", "verify", &format!("test::{main_function_name}"));
        let inner_program = TritonVMTestCase::new(entrypoint_location).program();

        generate_profile_of_verifier(&inner_program, &inner_stdin, inner_nd, None)
    }

    fn generate_profile_for_verifier_execution_for_factorial_execution_proof(
        factorial_arg: u32,
        expected_factorial_execution_padded_height: usize,
    ) {
        let factorial_program = factorial_program_no_io(factorial_arg);
        generate_profile_of_verifier(
            &factorial_program,
            &[],
            NonDeterminism::default(),
            Some(expected_factorial_execution_padded_height),
        )
    }

    fn generate_profile_of_verifier(
        inner_program: &Program,
        inner_input: &[BFieldElement],
        inner_nd: NonDeterminism<BFieldElement>,
        expected_inner_padded_height: Option<usize>,
    ) {
        let main_function_name = "verify_stark_proof";
        let entrypoint_location =
            EntrypointLocation::disk("recufier", "verify", &format!("test::{main_function_name}"));
        let test_case = TritonVMTestCase::new(entrypoint_location);
        let (non_determinism, claim_for_proof, inner_padded_height) =
            non_determinism_for_verify_and_claim_and_padded_height(
                inner_program,
                inner_input,
                inner_nd,
            );
        if let Some(expected_inner_padded_height) = expected_inner_padded_height {
            assert_eq!(expected_inner_padded_height, inner_padded_height);
        }
        let verifier_std_in = claim_to_stdin_for_stark_verifier(&claim_for_proof);

        let profile_file_name = format!(
            "{main_function_name}_inner_padded_height_{inner_padded_height}_{}_input_words",
            inner_input.len()
        );
        let verifier_program = test_case.program();
        let profile = tasm_lib::generate_full_profile(
            &profile_file_name,
            verifier_program.clone(),
            &verifier_std_in.to_vec().into(),
            &non_determinism,
            true,
        );
        println!("{profile}");

        use std::fs::create_dir_all;
        use std::fs::File;
        use std::io::Write;
        use std::path::Path;
        use std::path::PathBuf;
        let mut path = PathBuf::new();
        path.push("profiles");
        create_dir_all(&path).expect("profiles directory should exist or be created here");

        path.push(Path::new(&profile_file_name).with_extension("profile"));
        let mut file = File::create(&path).expect("open file for writing");
        write!(file, "{profile}").unwrap();

        if std::env::var("DYING_TO_PROVE").is_ok() {
            let verifier_std_out = [];
            tasm_lib::prove_and_verify(
                &verifier_program,
                &verifier_std_in,
                &non_determinism,
                &verifier_std_out,
                None,
            );
        }
    }
}
