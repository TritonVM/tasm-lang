// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use triton_vm::BFieldElement;
use triton_vm::Digest;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

fn main() {
    let a: Digest = tasm::tasm_io_read_stdin___digest();
    let b: Digest = tasm::tasm_io_read_stdin___digest();

    let a_list: Vec<BFieldElement> = a.encode();
    let b_list: Vec<BFieldElement> = b.encode();

    tasm::tasm_io_write_to_stdout___bfe(a_list[3]);
    tasm::tasm_io_write_to_stdout___bfe(b_list[4]);
    tasm::tasm_io_write_to_stdout___bfe(a_list[4]);
    tasm::tasm_io_write_to_stdout___bfe(a_list[1]);
    tasm::tasm_io_write_to_stdout___bfe(b_list[0]);

    return;
}

mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::tests_and_benchmarks::{
        ozk::{ozk_parsing, rust_shadows},
        test_helpers::shared_test::*,
    };
    use triton_vm::NonDeterminism;
    use twenty_first::shared_math::other::random_elements;

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

        let (rust_ast, _, _) =
            ozk_parsing::parse_function_and_structs("other", "simple_encode", "main");
        let expected_stack_diff = 0;
        let vm_output = execute_with_stack_memory_and_ins_safe_lists(
            &rust_ast,
            vec![],
            &mut HashMap::default(),
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.output);
    }
}