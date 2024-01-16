use triton_vm::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let a: BFieldElement = BFieldElement::new(17u64);
    let a_u64: u64 = a.value();
    let b: BFieldElement = BFieldElement::new(a_u64);
    tasm::tasm_io_write_to_stdout___bfe(b);

    let c: BFieldElement = BFieldElement::new((1 << 32) + 17u64);
    let c_u64: u64 = c.value();
    let d: BFieldElement = BFieldElement::new(c_u64);
    tasm::tasm_io_write_to_stdout___bfe(d);
    return;
}

#[cfg(test)]
mod test {
    use triton_vm::BFieldElement;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn value_test() {
        // Test function on host machine
        let stdin: Vec<BFieldElement> = vec![];
        let non_determinism: NonDeterminism<BFieldElement> = NonDeterminism::new(vec![]);
        let expected_output = vec![
            BFieldElement::new(17u64),
            BFieldElement::new((1 << 32) + 17u64),
        ];
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        assert_eq!(native_output, expected_output);

        // Test function in Triton VM

        let entrypoint_location = EntrypointLocation::disk("other", "value", "main");
        let (parsed, _) = ozk_parsing::parse_functions_and_types(&entrypoint_location);
        let expected_stack_diff = 0;
        let stack_start = vec![];
        let vm_output =
            execute_with_stack_safe_lists(&parsed, stack_start, expected_stack_diff).unwrap();
        assert_eq!(expected_output, *vm_output.output);
    }
}
