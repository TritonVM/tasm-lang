#[cfg(test)]
mod test {
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use tasm_lib::triton_vm::prelude::*;

    #[allow(clippy::vec_init_then_push)]
    fn main() {
        let mut a: Vec<BFieldElement> = Vec::<BFieldElement>::default();
        a.push(BFieldElement::new(102));
        let mut b: Vec<BFieldElement> = Vec::<BFieldElement>::default();
        b.clone_from(&a);
        assert!(1 == b.len());

        return;
    }

    #[test]
    fn clone_from_very_simple() {
        rust_shadows::wrap_main_with_io(&main)(vec![], NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("vectors", "clone_from_very_simple", "test::main");
        TritonVMTestCase::new(entrypoint).execute().unwrap();
    }
}
