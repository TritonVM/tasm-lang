#[cfg(test)]
mod test {
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use crate::triton_vm::prelude::*;

    fn main() {
        let mut list: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(32);
        list.push(BFieldElement::new(102));
        let boxed_list: Box<Vec<BFieldElement>> = Box::<Vec<BFieldElement>>::new(list);
        tasm::tasm_io_write_to_stdout___bfe(boxed_list[0]);

        return;
    }

    #[test]
    fn box_a_list_simple() {
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(vec![], NonDeterminism::default());
        let entrypoint = EntrypointLocation::disk("boxed", "box_a_list_simple", "test::main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
