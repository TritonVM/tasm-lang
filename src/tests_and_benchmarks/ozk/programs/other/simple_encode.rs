use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let a: Digest = tasm::tasmlib_io_read_stdin___digest();
    let b: Digest = tasm::tasmlib_io_read_stdin___digest();

    let a_list: Vec<BFieldElement> = a.encode();
    let b_list: Vec<BFieldElement> = b.encode();

    tasm::tasmlib_io_write_to_stdout___bfe(a_list[3]);
    tasm::tasmlib_io_write_to_stdout___bfe(b_list[4]);
    tasm::tasmlib_io_write_to_stdout___bfe(a_list[4]);
    tasm::tasmlib_io_write_to_stdout___bfe(a_list[1]);
    tasm::tasmlib_io_write_to_stdout___bfe(b_list[0]);

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
    fn simple_encode_test() {
        // Test function on host machine
        let digests: Vec<Digest> = random_elements(2);
        let stdin = [
            digests[0].reversed().encode(),
            digests[1].reversed().encode(),
        ]
        .concat();
        let non_determinism = NonDeterminism::new(vec![]);
        let expected_output = vec![
            digests[0].values()[3],
            digests[1].values()[4],
            digests[0].values()[4],
            digests[0].values()[1],
            digests[1].values()[0],
        ];
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        let entrypoint_location = EntrypointLocation::disk("other", "simple_encode", "main");
        let rust_ast = entrypoint_location.extract_entrypoint();
        let expected_stack_diff = 0;
        let vm_output = execute_with_stack_and_ins_safe_lists(
            &rust_ast,
            vec![],
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.public_output);
    }
}
