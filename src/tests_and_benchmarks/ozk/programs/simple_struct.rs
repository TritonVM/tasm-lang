// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

const SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS: u64 = 84;

#[derive(TasmObject, BFieldCodec)]
struct TestStruct {
    pub a: BFieldElement,
    pub b: BFieldElement,
}

fn main() {
    let test_struct: Box<TestStruct> =
        TestStruct::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    let a: &BFieldElement = &test_struct.a; // Use 1 `&`, ignore the 2nd `&`.
    tasm::tasm_io_write_to_stdout_bfe(*a); // Implement both `*` and method `to_owned` to mean put this onto the stack. We might need exceptions for list though.

    let b: &BFieldElement = &test_struct.b;
    tasm::tasm_io_write_to_stdout_bfe(*b);
    return;
}

mod tests {
    use super::*;
    use std::collections::HashMap;
    use triton_vm::BFieldElement;

    use crate::tests_and_benchmarks::ozk::programs::simple_struct::SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS;
    use crate::tests_and_benchmarks::ozk::{self, ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::{
        execute_compiled_with_stack_memory_and_ins_for_test, init_memory_from,
    };

    #[test]
    fn simple_struct_ozk_test() {
        // Test function on host machine
        let bfield_code_start_address = SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS;
        let non_determinism = init_memory_from(
            TestStruct {
                a: BFieldElement::new(14),
                b: BFieldElement::new(15),
            },
            BFieldElement::new(bfield_code_start_address),
        );

        let expected_output = vec![BFieldElement::new(14), BFieldElement::new(15)];
        let input = vec![];

        // Run test on host machine
        let native_output = rust_shadows::wrap_main_with_io(&ozk::programs::simple_struct::main)(
            input.clone(),
            non_determinism.clone(),
        );
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test("simple_struct");
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &mut HashMap::default(),
            input,
            non_determinism,
            0,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.output);
    }
}
