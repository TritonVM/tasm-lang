#![allow(clippy::needless_borrow)]
use triton_vm::BFieldElement;

fn main() {
    let mut a: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(32);
    a.push(BFieldElement::new(102));
    let mut b: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(32);
    b.clone_from(&a);
    assert!(1 == b.len());
    assert!(b[0] == a[0]);

    // Mutating `a` does not mutate b
    a[0] = BFieldElement::new(204);
    assert!(b[0] != a[0]);
    assert!(204 == a[0].value());
    assert!(102 == b[0].value());

    // Mutating `b` does not mutate a
    b.push(BFieldElement::new(103));
    assert!(102 == b[0].value());
    assert!(103 == b[1].value());
    assert!(2 == b.len());

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use itertools::Itertools;
    use triton_vm::NonDeterminism;

    #[test]
    fn clone_from_vector_simple() {
        let code = ozk_parsing::compile_for_test(
            "vectors",
            "clone_from_simple",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        println!("code:\n{}", code.iter().join("\n"));

        let stdin = vec![];
        let non_determinism = NonDeterminism::default();
        let expected_stack_diff = 0;
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &code,
            vec![],
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap()
        .output;
        assert_eq!(native_output, vm_output);
    }
}
