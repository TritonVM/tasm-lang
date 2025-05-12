use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

// Since struct is not copy, you cannot just call `&self` methods
// without explicitly creating a `Box<NonCopyStruct>` value.
struct NonCopyStruct(u64);

impl NonCopyStruct {
    fn new(value: u64) -> NonCopyStruct {
        return NonCopyStruct(value + 0xabcde123u64);
    }

    fn valued(&self) -> u64 {
        return self.0;
    }
}

#[allow(dead_code)]
fn main() {
    let a: NonCopyStruct = NonCopyStruct::new(tasm::tasmlib_io_read_stdin___u64());
    let boxed_a: Box<NonCopyStruct> = Box::<NonCopyStruct>::new(a);
    tasm::tasmlib_io_write_to_stdout___u64(boxed_a.valued());
    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use rand::random;
    use tasm_lib::triton_vm::prelude::*;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    #[test]
    fn ref_struct_typecheck_succeed_bc_boxed_test() {
        // Verify that compilation works
        let entrypoint =
            EntrypointLocation::disk("boxed", "ref_struct_typecheck_succeed_bc_boxed", "main");
        let _test_program = ozk_parsing::compile_for_test(&entrypoint);

        // Test function in Triton VM
        let rand: u64 = random::<u64>() / 2;
        let mut encoded = rand.encode();
        encoded.reverse();
        let stdin = encoded;
        let expected_output = (rand + 0xabcde123u64).encode();
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(stdin.clone())
            .execute()
            .unwrap();
        if expected_output != vm_output.public_output {
            panic!(
                "expected:\n{}\n\ngot:\n{}",
                expected_output.iter().join(","),
                vm_output.public_output.iter().join(",")
            );
        }

        // Test function on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), NonDeterminism::default());
        assert_eq!(native_output, expected_output);
    }
}
