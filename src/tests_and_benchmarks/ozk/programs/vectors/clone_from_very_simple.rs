#[cfg(test)]
mod test {
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use crate::triton_vm::prelude::*;

    fn main() {
        let mut a: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(32);
        a.push(BFieldElement::new(102));
        let mut b: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(32);
        b.clone_from(&a);
        assert!(1 == b.len());

        return;
    }

    #[test]
    fn clone_from_very_simple() {
        rust_shadows::wrap_main_with_io(&main)(vec![], NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("vectors", "clone_from_very_simple", "test::main");
        TritonVMTestCase::new(entrypoint)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
    }
}
