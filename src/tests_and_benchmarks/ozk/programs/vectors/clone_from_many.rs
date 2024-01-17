use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use triton_vm::BFieldElement;

fn main() {
    let mut a: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(32);
    a.push(BFieldElement::new(100));
    a.push(BFieldElement::new(101));

    let mut b: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(32);
    b.clone_from(&a);
    b.push(BFieldElement::new(102));

    let mut c: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(32);
    c.clone_from(&b);
    c.push(BFieldElement::new(103));

    assert!(2 == a.len());
    assert!(3 == b.len());
    assert!(4 == c.len());

    tasm::tasm_io_write_to_stdout___bfe(a[0]);
    tasm::tasm_io_write_to_stdout___bfe(a[1]);

    tasm::tasm_io_write_to_stdout___bfe(b[0]);
    tasm::tasm_io_write_to_stdout___bfe(b[1]);
    tasm::tasm_io_write_to_stdout___bfe(b[2]);

    tasm::tasm_io_write_to_stdout___bfe(c[0]);
    tasm::tasm_io_write_to_stdout___bfe(c[1]);
    tasm::tasm_io_write_to_stdout___bfe(c[2]);
    tasm::tasm_io_write_to_stdout___bfe(c[3]);

    a.push(BFieldElement::new(104));
    a.push(BFieldElement::new(105));
    a.push(BFieldElement::new(106));

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use triton_vm::NonDeterminism;

    #[test]
    fn clone_from_vector_many() {
        let code = ozk_parsing::compile_for_test(
            "vectors",
            "clone_from_many",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
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
