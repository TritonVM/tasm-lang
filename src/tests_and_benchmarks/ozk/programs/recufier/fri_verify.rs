use tasm_lib::prelude::TasmObject;
use tasm_lib::triton_vm::prelude::*;

use super::arithmetic_domain::*;

#[derive(Debug, Clone, PartialEq, Eq, BFieldCodec, TasmObject)]
struct FriVerify {
    pub(crate) expansion_factor: u32,
    pub(crate) num_collinearity_checks: u32,
    pub(crate) domain_length: u32,
    pub(crate) domain_offset: BFieldElement,
    domain_generator: BFieldElement,
}

impl FriVerify {
    fn new(
        offset: BFieldElement,
        domain_length: u32,
        expansion_factor: u32,
        num_collinearity_checks: u32,
    ) -> FriVerify {
        let domain: ArithmeticDomain =
            ArithmeticDomain::of_length(domain_length as usize).with_offset(offset);

        return FriVerify {
            expansion_factor,
            num_collinearity_checks,
            domain_length,
            domain_offset: domain.offset,
            domain_generator: domain.generator,
        };
    }
}

#[cfg(test)]
pub(crate) mod test {
    use rand::random;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;

    fn main() {
        let _a: FriVerify = FriVerify::new(BFieldElement::new(7), 32, 4, 3);

        return;
    }

    #[test]
    fn fri_verify_test() {
        // Rust program on host machine
        let stdin = vec![random(), random(), random(), random()];
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        // Run test on Triton-VM
        let entrypoint_location = EntrypointLocation::disk("recufier", "fri_verify", "test::main");
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

        assert_eq!(native_output, vm_output.public_output);
    }
}
