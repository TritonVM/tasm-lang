use triton_vm::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    // Store two BFieldElements in memory. Then read them out again.
    let a: BFieldElement = tasm::tasm_io_read_stdin___bfe();
    let b: BFieldElement = tasm::tasm_io_read_stdin___bfe();
    let boxed_ab: Box<(BFieldElement, BFieldElement)> =
        Box::<(BFieldElement, BFieldElement)>::new((a, b));
    let e: (BFieldElement, BFieldElement) = *boxed_ab;
    assert!(a == e.0);
    assert!(b == e.1);

    tasm::tasm_io_write_to_stdout___bfe(e.1);
    tasm::tasm_io_write_to_stdout___bfe(e.0);
    tasm::tasm_io_write_to_stdout___bfe(e.0);

    return;
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use itertools::Itertools;
    use rand::random;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn boxed_bfe_pair_test() {
        // Test function on host machine
        let a: BFieldElement = random();
        let b: BFieldElement = random();
        let stdin = vec![a, b];
        let non_determinism = NonDeterminism::new(vec![]);
        let expected_output = vec![b, a, a];
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let test_program = ozk_parsing::compile_for_test(
            "boxed",
            "bfe_pair",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        let expected_stack_diff = 0;
        println!("test_program:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &HashMap::default(),
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap();
        if expected_output != vm_output.output {
            panic!(
                "expected:\n{}\n\ngot:\n{}",
                expected_output.iter().join(","),
                vm_output.output.iter().join(",")
            );
        }
    }
}
