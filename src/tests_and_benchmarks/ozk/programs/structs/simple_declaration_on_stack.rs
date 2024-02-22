use num::One;
use tasm_lib::prelude::TasmObject;
use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(TasmObject, BFieldCodec)]
struct TestStruct {
    a: BFieldElement,
    b: BFieldElement,
    c: XFieldElement,
    d: Digest,
    e: bool,
    f: u32,
    g: u64,
}

fn main() {
    let mut test_struct: TestStruct = TestStruct {
        a: BFieldElement::new(14u64 << 40),
        b: BFieldElement::new(48u64 << 41),
        c: XFieldElement::one(),
        d: Digest::new([
            BFieldElement::new(2u64),
            BFieldElement::new(4u64),
            BFieldElement::new(8u64),
            BFieldElement::new(16u64),
            BFieldElement::new(32u64),
        ]),
        e: false,
        f: 1 << 22,
        g: 1 << 44,
    };
    let a: BFieldElement = test_struct.a;
    tasm::tasm_io_write_to_stdout___bfe(test_struct.a);
    tasm::tasm_io_write_to_stdout___bfe(a);
    tasm::tasm_io_write_to_stdout___bfe(test_struct.b);
    tasm::tasm_io_write_to_stdout___digest(test_struct.d);

    // Change a field value in the struct
    test_struct.a = BFieldElement::one();
    tasm::tasm_io_write_to_stdout___bfe(test_struct.a);

    return;
}

#[cfg(test)]
mod test {
    use tasm_lib::triton_vm::prelude::*;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;

    use super::*;

    #[test]
    fn simple_struct_declaration_on_stack_test() {
        // Test function on host machine
        let expected_output = [
            vec![BFieldElement::new(14u64 << 40)],
            vec![BFieldElement::new(14u64 << 40)],
            vec![BFieldElement::new(48u64 << 41)],
            vec![
                BFieldElement::new(2u64),
                BFieldElement::new(4u64),
                BFieldElement::new(8u64),
                BFieldElement::new(16u64),
                BFieldElement::new(32u64),
            ],
            vec![BFieldElement::one()],
        ]
        .concat();
        let stdin = vec![];
        let non_determinism = NonDeterminism::default();

        // Run on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run on Triton-VM
        let entrypoint_location =
            EntrypointLocation::disk("structs", "simple_declaration_on_stack", "main");
        let test_program = ozk_parsing::compile_for_test(&entrypoint_location);
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            0,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.output);
    }
}
