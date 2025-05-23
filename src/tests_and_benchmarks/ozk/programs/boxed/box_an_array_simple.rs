#[cfg(test)]
mod test {
    use tasm_lib::triton_vm::prelude::*;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    fn main() {
        let array: [BFieldElement; 2] = [BFieldElement::new(101), BFieldElement::new(102)];
        let boxed_array: Box<[BFieldElement; 2]> = Box::<[BFieldElement; 2]>::new(array);
        tasm::tasmlib_io_write_to_stdout___bfe(boxed_array[0]);

        return;
    }

    #[test]
    fn box_an_array_simple() {
        let entrypoint = EntrypointLocation::disk("boxed", "box_an_array_simple", "test::main");
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(vec![], NonDeterminism::default());
        let vm_output = TritonVMTestCase::new(entrypoint).execute().unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
