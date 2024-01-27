use tasm_lib::triton_vm::prelude::*;

#[allow(clippy::unnecessary_literal_unwrap)]
fn _crash_on_unwrap() {
    let result_bfe: Result<BFieldElement, ()> = Err(());
    let _a: BFieldElement = result_bfe.unwrap();

    return;
}

#[allow(clippy::needless_question_mark)]
fn _crash_on_try() {
    fn inner(input: Result<BFieldElement, ()>) -> Result<BFieldElement, ()> {
        return Ok(input?);
    }

    let _result_bfe: Result<BFieldElement, ()> = inner(Err(()));

    return;
}

mod test {
    use crate::tests_and_benchmarks::ozk::ozk_parsing::compile_for_test;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use std::default::Default;
    use tasm_lib::triton_vm::prelude::*;

    #[test]
    fn unwrap_crash_test() {
        let entrypoint_location =
            EntrypointLocation::disk("result_types", "unwrap_crash", "_crash_on_unwrap");
        let execution_result = execute_compiled_with_stack_and_ins_for_test(
            &compile_for_test(&entrypoint_location, crate::ast_types::ListType::Unsafe),
            vec![],
            vec![],
            NonDeterminism::default(),
            0,
        );
        assert!(execution_result.is_err());

        let entrypoint_location =
            EntrypointLocation::disk("result_types", "unwrap_crash", "_crash_on_try");
        let execution_result = execute_compiled_with_stack_and_ins_for_test(
            &compile_for_test(&entrypoint_location, crate::ast_types::ListType::Unsafe),
            vec![],
            vec![],
            NonDeterminism::default(),
            0,
        );
        assert!(execution_result.is_err());
    }
}
