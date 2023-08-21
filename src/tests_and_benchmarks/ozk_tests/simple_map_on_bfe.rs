use std::collections::HashMap;

use crate::tests_and_benchmarks::{
    ozk_programs,
    test_helpers::{shared_test::*, *},
};
use itertools::Itertools;
use triton_vm::{BFieldElement, NonDeterminism};

#[test]
fn simple_map_on_bfe_test() {
    // Test function on host machine
    let input = vec![
        BFieldElement::new(1000),
        BFieldElement::new(3000),
        BFieldElement::new(BFieldElement::MAX),
    ];
    let non_determinism = NonDeterminism::new(vec![]);
    let expected_output = input.iter().map(|x| *x + *x).collect_vec();
    let native_output = io_native::wrap_main_with_io(&ozk_programs::simple_map_on_bfe::main)(
        input.clone(),
        non_determinism.clone(),
    );
    assert_eq!(native_output, expected_output);

    // Test function in Triton VM
    let parsed = ozk_parsing::parse_main("simple_map_on_bfe");
    let expected_stack_diff = 0;
    let vm_output = execute_with_stack_memory_and_ins(
        &parsed,
        vec![],
        &mut HashMap::default(),
        input,
        non_determinism,
        expected_stack_diff,
    )
    .unwrap();
    assert_eq!(expected_output, vm_output.output);
}
