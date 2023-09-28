#![allow(clippy::needless_borrow)]
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

// Since struct is copy, you can call `&self` methods
// without explicitly creating a `Box<NonCopyStruct>` value.
#[derive(Clone, Copy)]
struct CopyStruct(u64);

impl CopyStruct {
    fn new(value: u64) -> CopyStruct {
        return CopyStruct(value + 0xabcde123u64);
    }

    fn valued(&self) -> u64 {
        return self.0;
    }
}

#[allow(dead_code)]
fn main() {
    let a: CopyStruct = CopyStruct::new(tasm::tasm_io_read_stdin___u64());
    tasm::tasm_io_write_to_stdout___u64((&a).valued());
    return;
}

mod tests {
    use super::*;
    use std::collections::HashMap;

    use itertools::Itertools;
    use rand::random;
    use triton_vm::NonDeterminism;
    use twenty_first::shared_math::bfield_codec::BFieldCodec;

    use crate::tests_and_benchmarks::{
        ozk::{ozk_parsing, rust_shadows},
        test_helpers::shared_test::execute_compiled_with_stack_memory_and_ins_for_test,
    };

    #[test]
    fn ref_struct_typecheck_succeed_test() {
        // Verify compilation works
        let test_program = ozk_parsing::compile_for_test(
            "boxed",
            "ref_struct_typecheck_succeed_bc_copy",
            crate::ast_types::ListType::Unsafe,
        );

        // Test function in Triton VM
        let rand: u64 = random::<u64>() / 2;
        let mut encoded = rand.encode();
        encoded.reverse();
        let stdin = encoded;
        let non_determinism = NonDeterminism::new(vec![]);
        let expected_output = (rand + 0xabcde123u64).encode();
        let expected_stack_diff = 0;
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &mut HashMap::default(),
            stdin.clone(),
            non_determinism.clone(),
            expected_stack_diff,
        )
        .unwrap();
        if expected_output != vm_output.output {
            panic!(
                "expected:\n{}\n\ngot:\n{}",
                expected_output.iter().join(","),
                vm_output.output.iter().join(",")
            );
        }

        // Test function on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);
    }
}
