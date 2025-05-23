use itertools::Itertools;
use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    fn local_function(input: BFieldElement) -> BFieldElement {
        return input * BFieldElement::new(2);
    }
    let mut input_values: Vec<BFieldElement> = Vec::<BFieldElement>::default();

    let length_indication: usize = tasm::tasmlib_io_read_stdin___bfe().value() as usize;
    let mut i: usize = 0;
    while i < length_indication {
        input_values.push(tasm::tasmlib_io_read_stdin___bfe());
        i += 1;
    }
    let output_values: Vec<BFieldElement> =
        input_values.into_iter().map(local_function).collect_vec();

    let mut j: usize = 0;
    while j < length_indication {
        tasm::tasmlib_io_write_to_stdout___bfe(output_values[j]);
        j += 1;
    }

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use tasm_lib::triton_vm::prelude::*;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn simple_map_on_bfe_test() {
        // Test function on host machine
        let stdin = vec![
            BFieldElement::new(3),
            BFieldElement::new(1000),
            BFieldElement::new(3000),
            BFieldElement::new(BFieldElement::MAX),
        ];
        let non_determinism = NonDeterminism::new(vec![]);
        let expected_output = stdin[1..].iter().map(|x| *x + *x).collect_vec();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let entrypoint_location = EntrypointLocation::disk("other", "simple_map_on_bfe", "main");
        let parsed = entrypoint_location.extract_entrypoint();
        let expected_stack_diff = 0;
        let vm_output = execute_with_stack_and_ins_safe_lists(
            &parsed,
            vec![],
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.public_output);
    }
}

mod benches {
    use std::collections::HashMap;

    use tasm_lib::triton_vm::prelude::*;
    use tasm_lib::twenty_first::math::other::random_elements;

    use crate::tests_and_benchmarks::benchmarks::execute_and_write_benchmark;
    use crate::tests_and_benchmarks::benchmarks::BenchmarkInput;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn simple_map_bench() {
        let common_case = BenchmarkInput {
            input_args: vec![],
            memory: HashMap::default(),
            std_in: [vec![BFieldElement::new(5)], random_elements(5)].concat(),
            non_determinism: NonDeterminism::new(vec![]),
        };

        let worst_case = BenchmarkInput {
            input_args: vec![],
            memory: HashMap::default(),
            std_in: [vec![BFieldElement::new(100)], random_elements(100)].concat(),
            non_determinism: NonDeterminism::new(vec![]),
        };

        let module_name = "simple_map_on_bfe".to_string();
        let entrypoint_location = EntrypointLocation::disk("other", &module_name, "main");
        let parsed_code = entrypoint_location.extract_entrypoint();
        let (code, _fn_name) = compile_for_run_test(&parsed_code);
        execute_and_write_benchmark(module_name, code, common_case, worst_case)
    }
}
