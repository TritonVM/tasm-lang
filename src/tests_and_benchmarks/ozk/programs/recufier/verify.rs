use itertools::Itertools;
use num::One;
use num::Zero;
use serde_derive::Serialize;
use tasm_lib::triton_vm::table::extension_table::Quotientable;
use tasm_lib::triton_vm::table::master_table::MasterExtTable;
use tasm_lib::triton_vm::table::ExtensionRow;
use tasm_lib::triton_vm::table::QuotientSegments;

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
    #[allow(clippy::vec_init_then_push)]
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

    fn quotient_summands(
        out_of_domain_point_curr_row: XFieldElement,
        padded_height: u32,
        trace_domain_generator: BFieldElement,
    ) -> [XFieldElement; 596] {
        let initial_zerofier_inv: XFieldElement =
            (out_of_domain_point_curr_row - BFieldElement::one()).inverse();
        // RecufyDebug::dump_xfe(initial_zerofier_inv);
        // println!("initial_zerofier_inv: {initial_zerofier_inv:?}");
        let consistency_zerofier_inv: XFieldElement =
            (out_of_domain_point_curr_row.mod_pow_u32(padded_height) - BFieldElement::one())
                .inverse();
        // println!("consistency_zerofier_inv: {consistency_zerofier_inv:?}");
        // RecufyDebug::dump_xfe(consistency_zerofier_inv);
        let except_last_row: XFieldElement =
            out_of_domain_point_curr_row - trace_domain_generator.inverse();
        // println!("except_last_row: {except_last_row:?}");
        // RecufyDebug::dump_xfe(except_last_row);
        let transition_zerofier_inv: XFieldElement = except_last_row * consistency_zerofier_inv;
        // println!("transition_zerofier_inv: {transition_zerofier_inv:?}");
        // RecufyDebug::dump_xfe(transition_zerofier_inv);
        let terminal_zerofier_inv: XFieldElement = except_last_row.inverse();
        // println!("terminal_zerofier_inv: {terminal_zerofier_inv:?}");
        // i.e., only last row
        // RecufyDebug::dump_xfe(terminal_zerofier_inv);

        let mut evaluated_constraints: [XFieldElement; 596] =
            tasm::tasm_recufier_master_ext_table_air_constraint_evaluation();
        // println!("evaluated_constraints: {evaluated_constraints:?}");

        let categories_running_sum_lengths: [usize; 4] =
            Recufier::constraint_evaluation_lengths_running_sum();
        // println!("categories_running_sum_lengths: {categories_running_sum_lengths:?}");
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
    fn linearly_sum_base_and_ext_row(
        out_of_domain_curr_base_row: Box<Box<[XFieldElement; 356]>>,
        out_of_domain_curr_ext_row: Box<Box<[XFieldElement; 83]>>,
        base_and_ext_codeword_weights: &Vec<XFieldElement>,
    ) -> XFieldElement {
        let mut acc: XFieldElement = XFieldElement::zero();
        let mut i: usize = 0;
        while i < out_of_domain_curr_base_row.len() {
            acc += out_of_domain_curr_base_row[i] * base_and_ext_codeword_weights[i];
            i += 1;
        }

        i = 0;
        while i < out_of_domain_curr_ext_row.len() {
            acc += out_of_domain_curr_ext_row[i]
                * base_and_ext_codeword_weights[i + out_of_domain_curr_base_row.len()];
            i += 1;
        }

        return acc;
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

    pub fn dump_xfe(thing: XFieldElement) {
        tasm::tasm_io_write_to_stdout___xfe(thing);

        return;
    }

    pub fn dump_digest(digest: Digest) {
        tasm::tasm_io_write_to_stdout___digest(digest);

        return;
    }

    #[allow(clippy::ptr_arg)]
    pub fn dump_bfes(bfes: &Vec<BFieldElement>) {
        let mut i: usize = 0;
        while i < bfes.len() {
            tasm::tasm_io_write_to_stdout___bfe(bfes[i]);
            i += 1;
        }

        return;
    }

    #[allow(clippy::ptr_arg)]
    pub fn dump_xfes(xfes: &Vec<XFieldElement>) {
        let mut i: usize = 0;
        while i < xfes.len() {
            tasm::tasm_io_write_to_stdout___xfe(xfes[i]);
            i += 1;
        }

        return;
    }

    #[allow(clippy::ptr_arg)]
    pub fn dump_digests(digests: &Vec<Digest>) {
        let mut i: usize = 0;
        while i < digests.len() {
            tasm::tasm_io_write_to_stdout___digest(digests[i]);
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

impl FriVerify {
    pub fn verify(&self, proof_iter: &mut VmProofIter) -> Vec<(u32, XFieldElement)> {
        return tasm::tasm_recufier_fri_verify(proof_iter, self);
    }
}

fn recufy() {
    let program_digest: Box<Digest> = Box::<Digest>::new(Digest::new([
        BFieldElement::new(7881280935549951237),
        BFieldElement::new(18116781179058631336),
        BFieldElement::new(15683079992428274309),
        BFieldElement::new(2749753857496185052),
        BFieldElement::new(14083115970614877960),
    ]));
    // println!("program_digest:\n{program_digest}");
    let parameters: Box<StarkParameters> = Box::<StarkParameters>::new(StarkParameters::default());

    Tip5WithState::init();
    let encoded_claim: Vec<BFieldElement> = Recufier::encode_claim(*program_digest);
    // println!("encoded_claim: {}", encoded_claim.iter().join(","));
    RecufyDebug::dump_bfes(&encoded_claim);
    Tip5WithState::pad_and_absorb_all(&encoded_claim);

    let inner_proof_iter: VmProofIter = VmProofIter::new();
    let mut proof_iter: Box<VmProofIter> = Box::<VmProofIter>::new(inner_proof_iter);
    let log_2_padded_height: Box<u32> = proof_iter.next_as_log2paddedheight();
    // println!("log_2_padded_height: {log_2_padded_height}");
    let padded_height: u32 = 1 << *log_2_padded_height;
    // println!("padded_height: {padded_height}");
    RecufyDebug::dump_u32(padded_height);

    let fri: Box<FriVerify> = Box::<FriVerify>::new(parameters.derive_fri(padded_height));

    let base_merkle_tree_root: Box<Digest> = proof_iter.next_as_merkleroot();
    // println!("base_merkle_tree_root: {base_merkle_tree_root:?}");
    RecufyDebug::dump_digest(*base_merkle_tree_root);

    let challenges: Box<Challenges> =
        tasm::tasm_recufier_challenges_new_empty_input_and_output_59_4(*program_digest);
    // println!("challenges: {:?}", challenges.challenges);
    RecufyDebug::dump_xfes(&challenges.challenges.to_vec());

    let extension_tree_merkle_root: Box<Digest> = proof_iter.next_as_merkleroot();
    // println!("extension_tree_merkle_root: {extension_tree_merkle_root:?}");
    RecufyDebug::dump_digest(*extension_tree_merkle_root);

    let quot_codeword_weights: [XFieldElement; 596] =
        <[XFieldElement; 596]>::try_from(Tip5WithState::sample_scalars(Recufier::num_quotients()))
            .unwrap();
    // println!("quot_codeword_weights: {quot_codeword_weights:?}");
    RecufyDebug::dump_xfes(&quot_codeword_weights.to_vec());
    let quotient_tree_merkle_root: Box<Digest> = proof_iter.next_as_merkleroot();
    // println!("quotient_codeword_merkle_root: {quotient_codeword_merkle_root:?}");
    RecufyDebug::dump_digest(*quotient_tree_merkle_root);

    let trace_domain_generator: BFieldElement =
        ArithmeticDomain::generator_for_length(padded_height as u64);
    // println!("trace_domain_generator: {trace_domain_generator:?}");
    RecufyDebug::dump_bfe(trace_domain_generator);

    let ___out_of_domain_point_curr_row: Vec<XFieldElement> = Tip5WithState::sample_scalars(1);
    let out_of_domain_point_curr_row: XFieldElement = ___out_of_domain_point_curr_row[0];
    // println!("out_of_domain_point_curr_row: {out_of_domain_point_curr_row:?}");
    RecufyDebug::dump_xfe(out_of_domain_point_curr_row);
    let out_of_domain_point_next_row: XFieldElement =
        out_of_domain_point_curr_row * trace_domain_generator;
    // println!("out_of_domain_point_next_row: {out_of_domain_point_next_row:?}");
    RecufyDebug::dump_xfe(out_of_domain_point_next_row);
    let out_of_domain_point_curr_row_pow_num_segments: XFieldElement =
        tasm::tasm_arithmetic_xfe_to_the_fourth(out_of_domain_point_curr_row);
    // println!("out_of_domain_point_curr_row_pow_num_segments: {out_of_domain_point_curr_row_pow_num_segments:?}");
    RecufyDebug::dump_xfe(out_of_domain_point_curr_row_pow_num_segments);

    let out_of_domain_curr_base_row: Box<Box<BaseRow<XFieldElement>>> =
        proof_iter.next_as_outofdomainbaserow();
    // println!("out_of_domain_curr_base_row: {out_of_domain_curr_base_row:?}");
    RecufyDebug::dump_xfes(&out_of_domain_curr_base_row.to_vec());
    let out_of_domain_curr_ext_row: Box<Box<ExtensionRow>> = proof_iter.next_as_outofdomainextrow();
    // println!("out_of_domain_curr_ext_row: {out_of_domain_curr_ext_row:?}");
    RecufyDebug::dump_xfes(&out_of_domain_curr_ext_row.to_vec());
    let out_of_domain_next_base_row: Box<Box<BaseRow<XFieldElement>>> =
        proof_iter.next_as_outofdomainbaserow();
    // println!("out_of_domain_next_base_row: {out_of_domain_next_base_row:?}");
    RecufyDebug::dump_xfes(&out_of_domain_next_base_row.to_vec());
    let out_of_domain_next_ext_row: Box<Box<ExtensionRow>> = proof_iter.next_as_outofdomainextrow();
    // println!("out_of_domain_next_ext_row: {out_of_domain_next_ext_row:?}");
    RecufyDebug::dump_xfes(&out_of_domain_next_ext_row.to_vec());
    let out_of_domain_curr_row_quot_segments: Box<[XFieldElement; 4]> =
        proof_iter.next_as_outofdomainquotientsegments();
    // println!("out_of_domain_curr_row_quot_segments: {out_of_domain_curr_row_quot_segments:?}");
    RecufyDebug::dump_xfes(&out_of_domain_curr_row_quot_segments.to_vec());

    let quotient_summands: [XFieldElement; 596] = Recufier::quotient_summands(
        out_of_domain_point_curr_row,
        padded_height,
        trace_domain_generator,
    );
    // println!("quotient_summands: {quotient_summands:?}");
    RecufyDebug::dump_xfes(&quotient_summands.to_vec());

    let out_of_domain_quotient_value: XFieldElement =
        tasm::tasm_array_inner_product_of_596_xfes(quot_codeword_weights, quotient_summands);
    // println!("out_of_domain_quotient_value: {out_of_domain_quotient_value:?}");
    RecufyDebug::dump_xfe(out_of_domain_quotient_value);

    let sum_of_evaluated_out_of_domain_quotient_segments: XFieldElement =
        tasm::tasm_array_horner_evaluation_with_4_coefficients(
            *out_of_domain_curr_row_quot_segments,
            out_of_domain_point_curr_row,
        );

    RecufyDebug::dump_xfe(sum_of_evaluated_out_of_domain_quotient_segments);
    // println!("sum_of_evaluated_out_of_domain_quotient_segments: {sum_of_evaluated_out_of_domain_quotient_segments:?}");
    // println!("out_of_domain_quotient_value: {out_of_domain_quotient_value:?}");
    assert!(sum_of_evaluated_out_of_domain_quotient_segments == out_of_domain_quotient_value);

    // Fiat-shamir 2
    let mut base_and_ext_codeword_weights: Vec<XFieldElement> = Tip5WithState::sample_scalars(
        Recufier::num_base_and_ext_and_quotient_segment_codeword_weights(),
    );
    RecufyDebug::dump_xfes(&base_and_ext_codeword_weights);

    let quotient_segment_codeword_weights: [XFieldElement; 4] = <[XFieldElement; 4]>::try_from(
        base_and_ext_codeword_weights.split_off(Recufier::num_columns()),
    )
    .unwrap();
    RecufyDebug::dump_xfes(&quotient_segment_codeword_weights.to_vec());
    RecufyDebug::dump_xfes(&base_and_ext_codeword_weights);

    // sum out-of-domain values
    let out_of_domain_curr_row_base_and_ext_value: XFieldElement =
        Recufier::linearly_sum_base_and_ext_row(
            out_of_domain_curr_base_row,
            out_of_domain_curr_ext_row,
            &base_and_ext_codeword_weights,
        );
    let out_of_domain_next_row_base_and_ext_value: XFieldElement =
        Recufier::linearly_sum_base_and_ext_row(
            out_of_domain_next_base_row,
            out_of_domain_next_ext_row,
            &base_and_ext_codeword_weights,
        );
    let out_of_domain_curr_row_quotient_segment_value: XFieldElement =
        tasm::tasm_array_inner_product_of_4_xfes(
            quotient_segment_codeword_weights,
            *out_of_domain_curr_row_quot_segments,
        );
    RecufyDebug::dump_xfe(out_of_domain_curr_row_base_and_ext_value);
    RecufyDebug::dump_xfe(out_of_domain_next_row_base_and_ext_value);
    RecufyDebug::dump_xfe(out_of_domain_curr_row_quotient_segment_value);

    // Fiat-Shamir 3
    let deep_coreword_weights: [XFieldElement; 3] =
        <[XFieldElement; 3]>::try_from(Tip5WithState::sample_scalars(3)).unwrap();
    RecufyDebug::dump_xfes(&deep_coreword_weights.to_vec());

    // FRI
    let revealed_fri_indices_and_elements: Vec<(u32, XFieldElement)> = fri.verify(&mut proof_iter);

    // Check leafs
    // Dequeue base elements
    // Could be read from secret-in, but it's much more efficient to get them from memory
    let num_combination_codeword_checks: usize = 2 * fri.num_colinearity_checks as usize;
    let base_table_rows: Box<Vec<BaseRow<BFieldElement>>> =
        proof_iter.next_as_masterbasetablerows();
    {
        let mut i: usize = 0;
        while i < num_combination_codeword_checks {
            // RecufyDebug::dump_bfes(&base_table_rows[i].to_vec());
            i += 1;
        }
    }

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
    RecufyDebug::dump_digests(&leaf_digests_base);

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
    RecufyDebug::dump_digests(&leaf_digests_ext);

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
    {
        let mut i: usize = 0;
    }

    // Ensure that sponge-states are in sync
    RecufyDebug::sponge_state(Tip5WithState::squeeze());
    return;
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use proptest_arbitrary_interop::arb;
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
    use crate::triton_vm::prelude::Claim;
    use crate::triton_vm::prelude::NonDeterminism;
    use crate::triton_vm::prelude::Proof;
    use crate::triton_vm::prelude::Stark;
    use crate::triton_vm::table::NUM_BASE_COLUMNS;

    use super::*;

    fn recufier_non_determinism() -> NonDeterminism<BFieldElement> {
        let factorial_program = triton_program!(
            push 3           // n
            push 1              // n accumulator
            call factorial      // 0 accumulator!
            halt

            factorial:          // n acc
                // if n == 0: return
                dup 1           // n acc n
                push 0 eq       // n acc n==0
                skiz            // n acc
                return      // 0 acc
                // else: multiply accumulator with n and recurse
                dup 1           // n acc n
                mul             // n acc·n
                swap 1          // acc·n n
                push -1 add     // acc·n n-1
                swap 1          // n-1 acc·n
                recurse
        );
        let public_input = [];
        let non_determinism = NonDeterminism::default();
        let (stark, claim, proof) =
            triton_vm::prove_program(&factorial_program, &public_input, &non_determinism).unwrap();
        assert!(
            triton_vm::verify(stark, &claim, &proof),
            "Proof must verify"
        );

        let fri = stark.derive_fri(proof.padded_height().unwrap()).unwrap();

        let proof_stream = StarkProofStream::try_from(&proof).unwrap();
        let proof_extraction = extract_fri_proof(&proof_stream, &claim, stark);
        let tasm_lib_fri: tasm_lib::recufier::fri_verify::FriVerify = fri.into();
        let fri_proof_digests =
            tasm_lib_fri.extract_digests_required_for_proving(&proof_extraction.fri_proof_stream);
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
        NonDeterminism::default()
            .with_ram(ram)
            .with_digests(nd_digests)
    }

    #[test]
    fn local_challenges_count_agrees_with_tvm() {
        assert_eq!(
            triton_vm::table::challenges::Challenges::COUNT,
            Challenges::count()
        );
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
        let non_determinism = recufier_non_determinism();
        let program = test_case.program();

        let native_output = rust_shadows::wrap_main_with_io_and_program_digest(&recufy)(
            Vec::default(),
            non_determinism.clone(),
            program,
        );

        let vm_output = test_case
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
