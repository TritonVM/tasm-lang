#![allow(clippy::needless_question_mark)]

use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    fn create_result_type(succeed: bool) -> Result<BFieldElement, ()> {
        let bfe: BFieldElement = tasm::tasm_io_read_stdin___bfe();
        let result_bfe: Result<BFieldElement, ()> = if succeed { Ok(bfe) } else { Err(()) };
        return result_bfe;
    }

    fn call_create(succeed: bool) -> Result<BFieldElement, ()> {
        return Ok(create_result_type(succeed)?);
    }

    fn question_mark_try_is_just_unwrap_bfe(
        val0: Result<BFieldElement, ()>,
        val1: Result<BFieldElement, ()>,
    ) -> Result<BFieldElement, ()> {
        assert!(val0? == val0.unwrap());
        assert!(val1? == val1.unwrap());
        let a: BFieldElement = val0.unwrap();
        let b: BFieldElement = val1?;

        return Ok(a + b);
    }

    fn question_mark_try_is_just_unwrap_xfe(
        val0: Result<XFieldElement, ()>,
        val1: Result<XFieldElement, ()>,
    ) -> Result<XFieldElement, ()> {
        assert!(val0? == val0.unwrap());
        assert!(val1? == val1.unwrap());
        let a: XFieldElement = val0.unwrap();
        let b: XFieldElement = val1?;

        return Ok(a + b + val0? + val1?);
    }

    // Result<Bfe>
    let bfe0: Result<BFieldElement, ()> = create_result_type(true);
    let bfe1: Result<BFieldElement, ()> = call_create(true);
    let bfe_sum_res: Result<BFieldElement, ()> = question_mark_try_is_just_unwrap_bfe(bfe0, bfe1);
    let bfe_sum_unwrapped: BFieldElement = bfe_sum_res.unwrap();
    tasm::tasm_io_write_to_stdout___bfe(bfe_sum_unwrapped);

    // Result<Xfe>
    let xfe0: Result<XFieldElement, ()> = Ok(tasm::tasm_io_read_stdin___xfe());
    let xfe1: Result<XFieldElement, ()> = Ok(tasm::tasm_io_read_stdin___xfe());
    let xfe_sum_res: Result<XFieldElement, ()> = question_mark_try_is_just_unwrap_xfe(xfe0, xfe1);
    tasm::tasm_io_write_to_stdout___xfe(xfe_sum_res.unwrap());

    return;
}

mod test {

    use std::default::Default;

    use tasm_lib::twenty_first::shared_math::other::random_elements;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn question_mark_operator_test() {
        let stdin = random_elements(8);
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let entrypoint_location =
            EntrypointLocation::disk("result_types", "question_mark_operator", "main");
        let test_program =
            ozk_parsing::compile_for_test(&entrypoint_location, crate::ast_types::ListType::Unsafe);
        let expected_stack_diff = 0;
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            NonDeterminism::default(),
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
