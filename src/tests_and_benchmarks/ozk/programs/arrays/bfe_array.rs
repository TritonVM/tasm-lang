use num::Zero;
use triton_vm::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let _k: BFieldElement = BFieldElement::zero();
    let a: Box<BFieldElement> = Box::<BFieldElement>::new(BFieldElement::new(100));
    let mut bfe_array: [BFieldElement; 10] = [
        BFieldElement::new(1000),
        BFieldElement::new(1001),
        BFieldElement::new(1002),
        BFieldElement::new(1003),
        BFieldElement::new(1004),
        BFieldElement::new(1005),
        BFieldElement::new(1006),
        BFieldElement::new(1007),
        BFieldElement::new(1008),
        BFieldElement::new(1009),
    ];

    let b: Box<BFieldElement> = Box::<BFieldElement>::new(BFieldElement::new(200));
    bfe_array[0] = tasm::tasm_io_read_stdin___bfe();
    bfe_array[1] = tasm::tasm_io_read_stdin___bfe();

    // Don't set element 2 to verify that indexing into
    // agrees with ordering of a array declaration as made above
    let _l: BFieldElement = tasm::tasm_io_read_stdin___bfe();
    bfe_array[3] = tasm::tasm_io_read_stdin___bfe();
    bfe_array[4] = tasm::tasm_io_read_stdin___bfe();
    let c: BFieldElement = BFieldElement::new(400);
    bfe_array[5] = tasm::tasm_io_read_stdin___bfe();
    bfe_array[6] = tasm::tasm_io_read_stdin___bfe();
    bfe_array[7] = tasm::tasm_io_read_stdin___bfe();
    bfe_array[8] = tasm::tasm_io_read_stdin___bfe();
    bfe_array[9] = tasm::tasm_io_read_stdin___bfe();

    tasm::tasm_io_write_to_stdout___bfe(bfe_array[7]);
    tasm::tasm_io_write_to_stdout___bfe(bfe_array[0]);
    tasm::tasm_io_write_to_stdout___bfe(bfe_array[2]);
    tasm::tasm_io_write_to_stdout___bfe(bfe_array[2]);
    tasm::tasm_io_write_to_stdout___bfe(bfe_array[9]);
    let d: BFieldElement = BFieldElement::new(1u64 << 50);
    tasm::tasm_io_write_to_stdout___bfe(bfe_array[9]);
    tasm::tasm_io_write_to_stdout___bfe(bfe_array[0]);
    tasm::tasm_io_write_to_stdout___bfe(*a);
    tasm::tasm_io_write_to_stdout___bfe(*b);
    tasm::tasm_io_write_to_stdout___bfe(c);
    tasm::tasm_io_write_to_stdout___bfe(d);

    return;
}

#[cfg(test)]
mod test {

    use itertools::Itertools;
    use rand::random;
    use triton_vm::BFieldElement;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;

    use super::*;

    #[test]
    fn bfe_array_test() {
        let non_determinism = NonDeterminism::default();

        let stdin: [BFieldElement; 10] = random();
        let stdin = stdin.to_vec();
        assert_ne!(stdin[0], stdin[1]);
        assert_ne!(stdin[0], stdin[2]);

        let expected_output = vec![
            stdin[7],
            stdin[0],
            BFieldElement::new(1002),
            BFieldElement::new(1002),
            stdin[9],
            stdin[9],
            stdin[0],
            BFieldElement::new(100),
            BFieldElement::new(200),
            BFieldElement::new(400),
            BFieldElement::new(1 << 50),
        ];

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.to_vec(), non_determinism.clone());
        println!("native_output: {native_output:#?}");
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test(
            "arrays",
            "bfe_array",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            0,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.output);
        println!("vm_output.output: {:#?}", vm_output.output);

        println!("Final stack is: {}", vm_output.final_stack.iter().join(","));
    }
}
