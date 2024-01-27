#[cfg(test)]
mod test {
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use crate::triton_vm::prelude::*;

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

    #[test]
    fn clone_from_vector_many() {
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(vec![], NonDeterminism::default());
        let entrypoint = EntrypointLocation::disk("vectors", "clone_from_many", "test::main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
