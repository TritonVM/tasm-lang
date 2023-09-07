use itertools::Itertools;
use triton_vm::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    fn local_function(input: BFieldElement) -> BFieldElement {
        return input * BFieldElement::new(2);
    }
    let mut input_values: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(200);

    let length_indication: usize = tasm::tasm_io_read_stdin_bfe().value() as usize;
    let mut i: usize = 0;
    while i < length_indication {
        input_values.push(tasm::tasm_io_read_stdin_bfe());
        i += 1;
    }
    let output_values: Vec<BFieldElement> =
        input_values.into_iter().map(local_function).collect_vec();

    let mut j: usize = 0;
    while j < length_indication {
        tasm::tasm_io_write_to_stdout_bfe(output_values[j]);
        j += 1;
    }

    return;
}

mod tests {
    use super::*;
    use crate::tests_and_benchmarks::{
        ozk::{ozk_parsing, rust_shadows},
        test_helpers::shared_test::*,
    };
    use itertools::Itertools;
    use std::collections::HashMap;
    use triton_vm::{BFieldElement, NonDeterminism};

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
        let (parsed, _, _) = ozk_parsing::parse_main_and_structs("other", "simple_map_on_bfe");
        let expected_stack_diff = 0;
        let vm_output = execute_with_stack_memory_and_ins_safe_lists(
            &parsed,
            vec![],
            &mut HashMap::default(),
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.output);
    }
}

mod benches {
    use crate::tests_and_benchmarks::benchmarks::{execute_and_write_benchmark, BenchmarkInput};
    use crate::tests_and_benchmarks::{ozk::ozk_parsing, test_helpers::shared_test::*};
    use std::collections::HashMap;
    use triton_vm::{BFieldElement, NonDeterminism};
    use twenty_first::shared_math::other::random_elements;

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

        let (parsed_code, _, module_name) =
            ozk_parsing::parse_main_and_structs("other", "simple_map_on_bfe");
        let (code, _fn_name) =
            compile_for_run_test(&parsed_code, crate::ast_types::ListType::Unsafe);
        execute_and_write_benchmark(module_name, code, common_case, worst_case, 0)
    }
}
