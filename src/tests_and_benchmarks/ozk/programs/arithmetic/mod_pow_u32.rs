// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use num::One;
use triton_vm::BFieldElement;
use twenty_first::shared_math::traits::ModPowU32;

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

mod tests {
    use super::*;
    use crate::tests_and_benchmarks::{
        ozk::{ozk_parsing, rust_shadows},
        test_helpers::shared_test::*,
    };
    use triton_vm::{BFieldElement, NonDeterminism};

    #[test]
    fn mod_pow_u32_test() {
        // Test function on host machine
        let stdin = vec![];
        let non_determinism = NonDeterminism::new(vec![]);
        let expected_output = vec![BFieldElement::new(144449u64).mod_pow_u32(51)];
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let (parsed, _, _) =
            ozk_parsing::parse_function_and_structs("arithmetic", "mod_pow_u32", "main");
        let expected_stack_diff = 0;
        let stack_start = vec![];
        let vm_output =
            execute_with_stack_safe_lists(&parsed, stack_start, expected_stack_diff).unwrap();
        assert_eq!(expected_output, vm_output.output);
    }
}
