use triton_vm::BFieldElement;

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
    use std::collections::HashMap;
    use std::default::Default;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn unwrap_crash_test() {
        assert!(execute_compiled_with_stack_memory_and_ins_for_test(
            &ozk_parsing::compile_for_test(
                "result_types",
                "unwrap_crash",
                "_crash_on_unwrap",
                crate::ast_types::ListType::Unsafe,
            ),
            vec![],
            &mut HashMap::default(),
            vec![],
            NonDeterminism::default(),
            0,
        )
        .is_err());

        assert!(execute_compiled_with_stack_memory_and_ins_for_test(
            &ozk_parsing::compile_for_test(
                "result_types",
                "unwrap_crash",
                "_crash_on_try",
                crate::ast_types::ListType::Unsafe,
            ),
            vec![],
            &mut HashMap::default(),
            vec![],
            NonDeterminism::default(),
            0,
        )
        .is_err());
    }
}
