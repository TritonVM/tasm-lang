use num::One;
use triton_vm::BFieldElement;
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let bfe: BFieldElement = tasm::tasm_io_read_stdin___bfe();
    let result_bfe: Result<BFieldElement, ()> = Ok(bfe);
    assert!(result_bfe.is_ok());
    return;
}

mod test {
    use std::collections::HashMap;
    use std::default::Default;

    use itertools::Itertools;
    use rand::random;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn result_bfe_test() {
        let rand: BFieldElement = random();
        let stdin = vec![rand];
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let test_program = ozk_parsing::compile_for_test(
            "result_types",
            "bfe",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        let expected_stack_diff = 0;
        println!("test_program:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &mut HashMap::default(),
            stdin,
            NonDeterminism::default(),
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
