use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use triton_vm::BFieldElement;

fn main() {
    // let a: BFieldElement = BFieldElement::new(100);
    let bfe_array_boxed: Box<[BFieldElement; 4]> = {
        let bfe_array: [BFieldElement; 4] = [
            BFieldElement::new(1000),
            BFieldElement::new(1001),
            BFieldElement::new(1002),
            BFieldElement::new(1003),
        ];

        Box::<[BFieldElement; 4]>::new(bfe_array)
    };

    // let b: BFieldElement = BFieldElement::new(200);
    // bfe_array_boxed[0] = tasm::tasm_io_read_stdin___bfe();
    // bfe_array_boxed[1] = tasm::tasm_io_read_stdin___bfe();

    // Don't set element 2 to verify that indexing into
    // agrees with ordering of a array declaration as made above
    // let _l: BFieldElement = tasm::tasm_io_read_stdin___bfe();
    // bfe_array_boxed[3] = tasm::tasm_io_read_stdin___bfe();
    // let c: BFieldElement = BFieldElement::new(400);

    tasm::tasm_io_write_to_stdout___bfe(bfe_array_boxed[2]);
    // tasm::tasm_io_write_to_stdout___bfe(bfe_array_boxed[0]);
    // tasm::tasm_io_write_to_stdout___bfe(bfe_array_boxed[0]);
    // tasm::tasm_io_write_to_stdout___bfe(bfe_array_boxed[2]);
    // tasm::tasm_io_write_to_stdout___bfe(bfe_array_boxed[3]);
    // let d: BFieldElement = BFieldElement::new(1u64 << 50);
    // tasm::tasm_io_write_to_stdout___bfe(bfe_array_boxed[3]);
    // tasm::tasm_io_write_to_stdout___bfe(bfe_array_boxed[0]);
    // tasm::tasm_io_write_to_stdout___bfe(a);
    // tasm::tasm_io_write_to_stdout___bfe(b);
    // tasm::tasm_io_write_to_stdout___bfe(c);
    // tasm::tasm_io_write_to_stdout___bfe(d);

    return;
}

mod tests {
    use super::*;
    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_memory_and_ins_for_test;
    use itertools::Itertools;
    use rand::random;
    use std::collections::HashMap;
    use triton_vm::{BFieldElement, NonDeterminism};

    #[ignore = "Doesn't work yet, as we can't move pointer types to memory"]
    #[test]
    fn bfe_array_boxed_test() {
        let non_determinism = NonDeterminism::default();

        let stdin: [BFieldElement; 10] = random();
        let stdin = stdin.to_vec();

        let expected_output = vec![
            BFieldElement::new(1002),
            // stdin[0],
            // stdin[0],
            // BFieldElement::new(1002),
            // stdin[3],
            // stdin[3],
            // stdin[0],
            // BFieldElement::new(100),
            // BFieldElement::new(200),
            // BFieldElement::new(400),
            // BFieldElement::new(1 << 50),
        ];

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.to_vec(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test(
            "arrays",
            "bfe_array_boxed",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &HashMap::default(),
            stdin,
            non_determinism,
            0,
        )
        .unwrap();
        if expected_output != vm_output.output {
            panic!(
                "expected_output:\n{}, got:\n{}. Code was:\n{}",
                expected_output.iter().join(", "),
                vm_output.output.iter().join(", "),
                test_program.iter().join("\n")
            );
        }
    }
}
