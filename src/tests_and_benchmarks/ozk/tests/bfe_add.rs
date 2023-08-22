use crate::tests_and_benchmarks::{
    ozk::{self, ozk_parsing, rust_shadows},
    test_helpers::shared_test::*,
};
use triton_vm::{BFieldElement, NonDeterminism};

#[test]
fn add_bfe_ozk_test() {
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
    let (parsed, _) = ozk_parsing::parse_main("bfe_add");
    let expected_stack_diff = 0;
    let stack_start = vec![];
    let vm_output = execute_with_stack(&parsed, stack_start, expected_stack_diff).unwrap();
    assert_eq!(expected_output, vm_output.output);
}
