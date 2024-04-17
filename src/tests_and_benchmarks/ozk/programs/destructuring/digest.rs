use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::twenty_first::prelude::*;

fn main() {
    let digest: Digest = tasm::tasmlib_io_read_stdin___digest();
    let Digest([d0, d1, d2, d3, d4]) = digest;

    tasm::tasmlib_io_write_to_stdout___bfe(d2);
    tasm::tasmlib_io_write_to_stdout___bfe(d1);
    tasm::tasmlib_io_write_to_stdout___bfe(d4);
    tasm::tasmlib_io_write_to_stdout___bfe(d3);
    tasm::tasmlib_io_write_to_stdout___bfe(d0);

    return;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
    use crate::triton_vm::prelude::*;
    use tasm_lib::twenty_first::math::other::random_elements;
    use tasm_lib::DIGEST_LENGTH;

    #[test]
    fn destructure_digest() {
        let entrypoint_location = EntrypointLocation::disk("destructuring", "digest", "main");
        let test_case = TritonVMTestCase::new(entrypoint_location);
        let std_in = random_elements(DIGEST_LENGTH);

        let native_output =
            rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
        let vm_output = test_case.with_std_in(std_in).execute().unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
