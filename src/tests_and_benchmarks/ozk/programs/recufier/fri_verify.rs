#![allow(clippy::explicit_auto_deref)]
// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use num::One;
use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::{BFieldElement, Digest};
use twenty_first::{
    shared_math::{bfield_codec::BFieldCodec, traits::PrimitiveRootOfUnity},
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
