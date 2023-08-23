use std::collections::HashMap;

use crate::tests_and_benchmarks::{
    ozk,
    test_helpers::{shared_test::*, *},
};
use itertools::Itertools;
use triton_vm::{BFieldElement, NonDeterminism};

#[test]
fn simple_map_on_bfe_test() {
    // Test function on host machine
    let input = vec![
        BFieldElement::new(3),
        BFieldElement::new(1000),
        BFieldElement::new(3000),
        BFieldElement::new(BFieldElement::MAX),
    ];
    let non_determinism = NonDeterminism::new(vec![]);
    let expected_output = input[1..].iter().map(|x| *x + *x).collect_vec();
    let native_output = io_native::wrap_main_with_io(&ozk::programs::simple_map_on_bfe::main)(
        input.clone(),
        non_determinism.clone(),
    );
    assert_eq!(native_output, expected_output);

    // Test function in Triton VM
    let (parsed, _) = ozk_parsing::parse_main("simple_map_on_bfe");
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

mod bench {
    use super::*;
    use std::collections::HashMap;

    use triton_vm::NonDeterminism;
    use twenty_first::shared_math::other::random_elements;

    use crate::tests_and_benchmarks::benchmarks::{execute_and_write_benchmark, BenchmarkInput};

    #[test]
    fn simple_map_bench() {
        let common_case = BenchmarkInput {
            input_args: vec![],
            memory: HashMap::default(),
            std_in: vec![vec![BFieldElement::new(5)], random_elements(5)].concat(),
            non_determinism: NonDeterminism::new(vec![]),
        };

        let worst_case = BenchmarkInput {
            input_args: vec![],
            memory: HashMap::default(),
            std_in: vec![vec![BFieldElement::new(100)], random_elements(100)].concat(),
            non_determinism: NonDeterminism::new(vec![]),
        };

        let (parsed_code, module_name) = ozk_parsing::parse_main("simple_map_on_bfe");
        let (code, _fn_name) = compile_for_run_test(&parsed_code);
        execute_and_write_benchmark(module_name, code, common_case, worst_case, 0)
    }
}
