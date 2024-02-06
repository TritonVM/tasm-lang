use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let bfe: BFieldElement = tasm::tasm_io_read_stdin___bfe();
    let xfe: XFieldElement = tasm::tasm_io_read_stdin___xfe();
    tasm::tasm_io_write_to_stdout___xfe(xfe + bfe);

    return;
}

#[cfg(test)]
mod test {
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use tasm_lib::twenty_first::shared_math::other::random_elements;

    use super::*;

    #[test]
    fn add_bfe_to_xfe() {
        let std_in = random_elements(4);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
        let entrypoint = EntrypointLocation::disk("arithmetic", "xfe_bfe_add", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
