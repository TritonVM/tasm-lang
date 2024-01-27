use crate::triton_vm::prelude::*;
use crate::triton_vm::twenty_first::shared_math::traits::ModPowU32;
use num::One;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let base_number: BFieldElement = BFieldElement::new(144449u64);
    let mut pow_expected: BFieldElement = BFieldElement::one();
    let threshold: u32 = 50;
    let mut exponent: u32 = 0;
    while exponent <= threshold {
        assert!(pow_expected == base_number.mod_pow_u32(exponent));
        pow_expected *= base_number;
        exponent += 1;
    }

    tasm::tasm_io_write_to_stdout___bfe(base_number.mod_pow_u32(exponent));

    return;
}

#[cfg(test)]
mod test {

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn mod_pow_u32_test() {
        // Test function on host machine
        let stdin = vec![];
        let non_determinism = NonDeterminism::default();
        let native_output = rust_shadows::wrap_main_with_io(&main)(stdin, non_determinism);

        let expected_output = vec![BFieldElement::new(144449u64).mod_pow_u32(51)];
        assert_eq!(native_output, expected_output);

        let entrypoint = EntrypointLocation::disk("arithmetic", "mod_pow_u32", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_safe_lists()
            .expect_stack_difference(0)
            .execute()
            .unwrap();

        assert_eq!(expected_output, vm_output.output);
    }
}
