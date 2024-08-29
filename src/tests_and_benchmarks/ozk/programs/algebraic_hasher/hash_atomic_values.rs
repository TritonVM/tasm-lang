use tasm_lib::triton_vm::prelude::*;
use twenty_first::prelude::AlgebraicHasher;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[cfg(test)]
mod test {
    use tasm::wrap_main_with_io;
    use tasm_lib::triton_vm::prelude::*;
    use twenty_first::math::other::random_elements;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    use super::*;

    fn hash_atomic_values() {
        let digest: Digest = tasm::tasmlib_io_read_stdin___digest();
        let digest_digest: Digest = Tip5::hash(&digest);
        tasm::tasmlib_io_write_to_stdout___digest(digest_digest);

        let bfe: BFieldElement = tasm::tasmlib_io_read_stdin___bfe();
        let bfe_digest: Digest = Tip5::hash(&bfe);
        tasm::tasmlib_io_write_to_stdout___digest(bfe_digest);

        let xfe: XFieldElement = tasm::tasmlib_io_read_stdin___xfe();
        let xfe_digest: Digest = Tip5::hash(&xfe);
        tasm::tasmlib_io_write_to_stdout___digest(xfe_digest);

        return;
    }

    #[test]
    fn hash_values_on_stack_test() {
        let std_in = random_elements(Digest::LEN * 3);
        let native_output =
            wrap_main_with_io(&hash_atomic_values)(std_in.clone(), NonDeterminism::default());

        let entrypoint = EntrypointLocation::disk(
            "algebraic_hasher",
            "hash_atomic_values",
            "test::hash_atomic_values",
        );
        let vm_output = TritonVMTestCase::new(entrypoint.clone())
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
