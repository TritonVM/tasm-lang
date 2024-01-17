#![allow(clippy::needless_borrow)]
use triton_vm::BFieldElement;

#[allow(dead_code)]
fn main() {
    let mut a: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(32);
    a.push(BFieldElement::new(102));
    let mut b: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(32);
    b.clone_from(&a);
    assert!(1 == b.len());

    return;
}

#[cfg(test)]
mod test {
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use triton_vm::NonDeterminism;

    #[test]
    fn clone_from_very_simple() {
        let code = ozk_parsing::compile_for_test(
            "vectors",
            "clone_from_very_simple",
            "main",
            crate::ast_types::ListType::Unsafe,
        );

        let stdin = vec![];
        let non_determinism = NonDeterminism::default();
        let expected_stack_diff = 0;
        execute_compiled_with_stack_and_ins_for_test(
            &code,
            vec![],
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap();
    }
}
