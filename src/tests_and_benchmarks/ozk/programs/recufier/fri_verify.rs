#![allow(clippy::explicit_auto_deref)]
// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use num::One;
use tasm_lib::{
    recufier::proof_stream::vm_proof_stream::VmProofStream, structure::tasm_object::TasmObject,
};
use triton_vm::{BFieldElement, Digest};
use twenty_first::{
    shared_math::{
        bfield_codec::BFieldCodec,
        traits::{ModPowU32, PrimitiveRootOfUnity},
        x_field_element::XFieldElement,
    },
    util_types::algebraic_hasher::AlgebraicHasher,
};
type H = twenty_first::shared_math::tip5::Tip5;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ArithmeticDomain {
    pub offset: BFieldElement,
    pub generator: BFieldElement,
    pub length: usize,
}

impl ArithmeticDomain {
    /// Create a new domain with the given length.
    /// No offset is applied, but can added through [`with_offset()`](Self::with_offset).
    pub fn of_length(length: usize) -> ArithmeticDomain {
        return ArithmeticDomain {
            offset: BFieldElement::one(),
            generator: ArithmeticDomain::generator_for_length(length as u64),
            length,
        };
    }

    /// Set the offset of the domain.
    pub fn with_offset(mut self, offset: BFieldElement) -> ArithmeticDomain {
        self.offset = offset;
        return self;
    }

    /// Derive a generator for a domain of the given length.
    /// The domain length must be a power of 2.
    pub fn generator_for_length(domain_length: u64) -> BFieldElement {
        fn is_power_of_two(val: u64) -> bool {
            return val != 0u64 && (val & (val - 1)) == 0u64;
        }

        assert!(0u64 == domain_length || is_power_of_two(domain_length));

        return BFieldElement::primitive_root_of_unity(domain_length).unwrap();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, BFieldCodec, TasmObject)]
pub struct FriVerify {
    // expansion factor = 1 / rate
    pub expansion_factor: u32,
    pub num_colinearity_checks: u32,
    pub domain_length: u32,
    pub domain_offset: BFieldElement,
    domain_generator: BFieldElement,
}

impl FriVerify {
    pub fn new(
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

    pub fn first_round_max_degree(&self) -> u32 {
        assert!(self.domain_length >= self.expansion_factor);
        return (self.domain_length / self.expansion_factor) - 1;
    }

    /// Computes the number of rounds
    pub fn num_rounds(&self) -> u32 {
        let first_round_code_dimension: u32 = self.first_round_max_degree() + 1;
        let max_num_rounds: u32 =
            tasm::tasm_arithmetic_u64_log_2_floor(first_round_code_dimension as u64);

        // Skip rounds for which Merkle tree verification cost exceeds arithmetic cost,
        // because more than half the codeword's locations are queried.
        let num_rounds_checking_all_locations: u32 =
            tasm::tasm_arithmetic_u64_log_2_floor(self.num_colinearity_checks as u64);
        let num_rounds_checking_most_locations: u32 = num_rounds_checking_all_locations + 1;

        return if max_num_rounds > num_rounds_checking_most_locations {
            max_num_rounds - num_rounds_checking_most_locations
        } else {
            0
        };
    }

    /// Computes the max degree of the codeword interpolant after the last round
    pub fn last_round_max_degree(&self) -> u32 {
        return self.first_round_max_degree() >> self.num_rounds();
    }

    fn get_colinearity_check_x(&self, idx: u32, round: usize) -> XFieldElement {
        let domain_value: BFieldElement =
            self.domain_offset * self.domain_generator.mod_pow_u32(idx);
        // let round_exponent: u32 = 2u32.pow(round as u32);
        let round_exponent: u32 = 1u32 << (round as u32);
        let evaluation_argument: BFieldElement = domain_value.mod_pow_u32(round_exponent);

        return evaluation_argument.lift();
    }

    // fn inner_verify(
    //     &self,
    //     proof_stream: &mut VmProofStream,
    //     nondeterministic_digests: &mut Vec<Digest>,
    // ) -> anyhow::Result<Vec<(u32, XFieldElement)>> {
    fn inner_verify(
        &self,
        // proof_stream: &mut VmProofStream,
        nondeterministic_digests: &mut Vec<Digest>,
    ) {
        let mut num_nondeterministic_digests_read: u32 = 0;
        let num_rounds: u32 = self.num_rounds();
        let last_round_max_degree: u32 = self.last_round_max_degree();

        // Extract all roots and calculate alpha based on Fiat-Shamir challenge
        let mut roots: Vec<Digest> = Vec::<Digest>::with_capacity(num_rounds as usize);
        let mut alphas: Vec<XFieldElement> =
            Vec::<XFieldElement>::with_capacity(num_rounds as usize);

        // let first_root = proof_stream.dequeue().unwrap().as_merkle_root().unwrap();
        // roots.push(first_root);

        return;
    }
}

fn main() -> u32 {
    return 14u32;
}

mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::ast_types;
    use crate::tests_and_benchmarks::test_helpers::shared_test::{
        execute_compiled_with_stack_memory_and_ins_for_test, init_memory_from,
    };
    use crate::tests_and_benchmarks::{
        ozk::{ozk_parsing, rust_shadows},
        test_helpers::shared_test::*,
    };
    use triton_vm::NonDeterminism;
    use twenty_first::shared_math::other::random_elements;
    use twenty_first::util_types::merkle_tree::{CpuParallel, MerkleTree};
    use twenty_first::util_types::merkle_tree_maker::MerkleTreeMaker;

    #[test]
    fn fri_verify_test() {
        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test(
            "recufier",
            "fri_verify",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &mut HashMap::default(),
            Default::default(),
            Default::default(),
            0,
        )
        .unwrap();
    }
}
