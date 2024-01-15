use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::twenty_first::shared_math::bfield_codec::BFieldCodec;
use triton_vm::BFieldElement;

use super::arithmetic_domain::*;

#[derive(Debug, Clone, PartialEq, Eq, BFieldCodec, TasmObject)]
struct FriVerify {
    pub expansion_factor: u32,
    pub num_colinearity_checks: u32,
    pub domain_length: u32,
    pub domain_offset: BFieldElement,
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

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use rand::random;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_memory_and_ins_for_test;

    use super::*;

    #[test]
    fn fri_verify_test() {
        // Rust program on host machine
        let stdin = vec![random(), random(), random(), random()];
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test(
            "recufier",
            "fri_verify",
            "main",
            crate::ast_types::ListType::Unsafe,
        );

        let expected_stack_diff = 0;
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &HashMap::default(),
            Default::default(),
            Default::default(),
            expected_stack_diff,
        )
        .unwrap();

        assert_eq!(native_output, vm_output.output);
    }
}
