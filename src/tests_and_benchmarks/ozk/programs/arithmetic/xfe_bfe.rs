use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let bfe: BFieldElement = tasm::tasmlib_io_read_stdin___bfe();
    let xfe: XFieldElement = tasm::tasmlib_io_read_stdin___xfe();
    tasm::tasmlib_io_write_to_stdout___xfe(xfe + bfe);
    tasm::tasmlib_io_write_to_stdout___xfe(xfe - bfe);

    return;
}

#[cfg(test)]
mod test {
    use tasm_lib::twenty_first::math::other::random_elements;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn xfe_bfe_arithmetic() {
        let std_in = random_elements(4);
        let native_output_add =
            rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
        let entrypoint_add = EntrypointLocation::disk("arithmetic", "xfe_bfe", "main");
        let vm_output = TritonVMTestCase::new(entrypoint_add)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output_add, vm_output.public_output);
    }
}
