use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use triton_vm::BFieldElement;

fn main() {
    // Store two BFieldElements in memory. Then read them out again.
    let a: BFieldElement = tasm::tasm_io_read_stdin_bfe();
    let b: BFieldElement = BFieldElement::new((1u64 << 40) + 132);
    let boxed_a: Box<BFieldElement> = Box::<BFieldElement>::new(a);
    let boxed_b: Box<BFieldElement> = Box::<BFieldElement>::new(b);

    assert!(a == *boxed_a);
    assert!(b == *boxed_b);

    tasm::tasm_io_write_to_stdout_bfe(*boxed_b);
    tasm::tasm_io_write_to_stdout_bfe(*boxed_a);

    return;
}

mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::tests_and_benchmarks::{
        ozk::{ozk_parsing, rust_shadows},
        test_helpers::shared_test::*,
    };
    use itertools::Itertools;
    use rand::random;
    use triton_vm::NonDeterminism;

    #[test]
    fn boxed_bfe_test() {
        // Test function on host machine
        let rand: BFieldElement = random();
        let stdin = vec![rand];
        let non_determinism = NonDeterminism::new(vec![]);
        let expected_output = vec![BFieldElement::new((1u64 << 40) + 132), rand];
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let test_program =
            ozk_parsing::compile_for_test("boxed", "bfe", crate::ast_types::ListType::Unsafe);
        let expected_stack_diff = 0;
        println!("test_program:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &mut HashMap::default(),
            stdin,
            NonDeterminism::new(vec![]),
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
