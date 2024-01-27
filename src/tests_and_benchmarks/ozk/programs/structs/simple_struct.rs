use crate::triton_vm::prelude::*;
use crate::triton_vm::twenty_first::shared_math::bfield_codec::BFieldCodec;
use crate::triton_vm::twenty_first::shared_math::x_field_element::XFieldElement;

use tasm_lib::structure::tasm_object::TasmObject;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

const SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS: u64 = 84;

#[derive(TasmObject, BFieldCodec)]
struct TestStruct {
    a: BFieldElement,
    b: BFieldElement,
    c: XFieldElement,
    d: Digest,
    e: bool,
    f: u32,
    g: u64,
    h: u128,
}

fn main() {
    let test_struct: Box<TestStruct> =
        TestStruct::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    let a: BFieldElement = test_struct.a; // Use 1 `&`, ignore the 2nd `&`.
    tasm::tasm_io_write_to_stdout___u128(test_struct.h);
    tasm::tasm_io_write_to_stdout___bfe(a); // Implement both `*` and method `to_owned` to mean put this onto the stack. We might need exceptions for list though.

    tasm::tasm_io_write_to_stdout___bool(test_struct.e);
    let b: BFieldElement = test_struct.b;
    tasm::tasm_io_write_to_stdout___u64(test_struct.g);
    tasm::tasm_io_write_to_stdout___bfe(b);
    tasm::tasm_io_write_to_stdout___digest(test_struct.d);
    tasm::tasm_io_write_to_stdout___xfe(test_struct.c);
    tasm::tasm_io_write_to_stdout___u32(test_struct.f);
    return;
}

#[cfg(test)]
mod test {

    use crate::triton_vm::prelude::*;
    use itertools::Itertools;
    use rand::random;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;

    use super::*;

    #[test]
    fn simple_struct_ozk_test() {
        // Test function on host machine
        let bfield_code_start_address = SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS;
        let test_struct = TestStruct {
            a: random(),
            b: random(),
            c: random(),
            d: random(),
            e: random(),
            f: random(),
            g: random(),
            h: random(),
        };
        let non_determinism =
            init_memory_from(&test_struct, BFieldElement::new(bfield_code_start_address));

        let expected_output = [
            test_struct.h.encode(),
            vec![test_struct.a],
            test_struct.e.encode(),
            test_struct.g.encode(),
            vec![test_struct.b],
            test_struct.d.values().to_vec(),
            test_struct.c.coefficients.to_vec(),
            test_struct.f.encode(),
        ]
        .concat();
        let stdin = vec![];

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let entrypoint_location = EntrypointLocation::disk("structs", "simple_struct", "main");
        let test_program =
            ozk_parsing::compile_for_test(&entrypoint_location, crate::ast_types::ListType::Unsafe);
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            0,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.output);

        println!("Final stack is: {}", vm_output.final_stack.iter().join(","));
    }
}
