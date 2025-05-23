#[cfg(test)]
mod test {
    use tasm_lib::triton_vm::prelude::*;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[allow(clippy::vec_init_then_push)]
    fn main() {
        let mut a: Vec<BFieldElement> = Vec::<BFieldElement>::default();
        a.push(BFieldElement::new(102));
        let mut b: Vec<BFieldElement> = Vec::<BFieldElement>::default();
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

    #[test]
    fn clone_from_vector_simple() {
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(vec![], NonDeterminism::default());
        let entrypoint = EntrypointLocation::disk("vectors", "clone_from_simple", "test::main");
        let vm_output = TritonVMTestCase::new(entrypoint).execute().unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
