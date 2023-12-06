use triton_vm::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[allow(clippy::assertions_on_constants)]
#[allow(clippy::vec_init_then_push)]
fn main() {
    let bfe_0: BFieldElement = tasm::tasm_io_read_stdin___bfe();
    let bfe_1: BFieldElement = tasm::tasm_io_read_stdin___bfe();
    let mut bfes: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(2);
    bfes.push(bfe_0);
    bfes.push(bfe_1);
    let result_bfes: Result<Vec<BFieldElement>, ()> = Ok(bfes);
    assert!(result_bfes.is_ok());

    match result_bfes {
        Result::Ok(bfes) => {
            tasm::tasm_io_write_to_stdout___bfe(bfes[0]);
            tasm::tasm_io_write_to_stdout___bfe(bfes[1]);
            tasm::tasm_io_write_to_stdout___u32(bfes.len() as u32);
        }
        Result::Err(_) => {
            assert!(false);
        }
    };

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
    fn non_copy_types_test() {
        let stdin = vec![random(), random()];
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let test_program = ozk_parsing::compile_for_test(
            "result_types",
            "non_copy_types",
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