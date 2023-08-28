// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use triton_vm::BFieldElement;

fn main() {
    let a: BFieldElement = BFieldElement::new(14);
    let b: BFieldElement = BFieldElement::new(15);
    let c: BFieldElement = a + b;
    tasm::tasm_io_write_to_stdout_bfe(c);
    return;
}

mod tests {
    use crate::tests_and_benchmarks::{
        ozk::{self, ozk_parsing, rust_shadows},
        test_helpers::shared_test::*,
    };
    use triton_vm::{BFieldElement, NonDeterminism};

    #[test]
    fn add_bfe_ozk_test_same_file() {
        // Test function on host machine
        let input = vec![];
        let non_determinism = NonDeterminism::new(vec![]);
        let expected_output = vec![BFieldElement::new(29)];
        let native_output = rust_shadows::wrap_main_with_io(&ozk::programs::bfe_add::main)(
            input.clone(),
            non_determinism.clone(),
        );
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let (parsed, _, _) = ozk_parsing::parse_main_and_structs("bfe_add");
        let expected_stack_diff = 0;
        let stack_start = vec![];
        let vm_output = execute_with_stack(&parsed, stack_start, expected_stack_diff).unwrap();
        assert_eq!(expected_output, vm_output.output);
    }
}
