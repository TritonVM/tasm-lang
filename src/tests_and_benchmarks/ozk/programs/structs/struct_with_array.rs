use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use arbitrary::Arbitrary;
use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::{BFieldElement, Digest};
use twenty_first::shared_math::bfield_codec::BFieldCodec;

#[derive(TasmObject, BFieldCodec, Arbitrary)]
struct TestStructWithArrays {
    pub a: BFieldElement,
    pub b: [BFieldElement; 4],
    pub c: [Digest; 2],
    pub d: u64,
}

fn main() {
    let test_struct: Box<TestStructWithArrays> =
        TestStructWithArrays::decode(&tasm::load_from_memory(BFieldElement::new(300))).unwrap();

    // Print `a`
    let a: BFieldElement = test_struct.a;
    tasm::tasm_io_write_to_stdout___bfe(a);

    // Print `b`s
    tasm::tasm_io_write_to_stdout___bfe(test_struct.b[0]);
    tasm::tasm_io_write_to_stdout___bfe(test_struct.b[1]);
    tasm::tasm_io_write_to_stdout___bfe(test_struct.b[2]);
    tasm::tasm_io_write_to_stdout___bfe(test_struct.b[3]);

    // Print `c`s
    tasm::tasm_io_write_to_stdout___digest(test_struct.c[0]);
    tasm::tasm_io_write_to_stdout___digest(test_struct.c[1]);

    // Print `d`
    tasm::tasm_io_write_to_stdout___u64(test_struct.d);

    return;
}

mod tests {
    use super::*;
    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use itertools::Itertools;
    use rand::random;
    use std::collections::HashMap;
    use triton_vm::BFieldElement;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::{
        execute_compiled_with_stack_memory_and_ins_for_test, init_memory_from,
    };

    #[test]
    fn struct_with_arrays_test() {
        let raw_data: [u8; 100] = random();
        let mut unstructured = Unstructured::new(&raw_data);
        let test_struct = TestStructWithArrays::arbitrary(&mut unstructured).unwrap();
        let non_determinism = init_memory_from(&test_struct, BFieldElement::new(300));
        let stdin = vec![];
        let expected_output = [
            vec![test_struct.a],
            vec![
                test_struct.b[0],
                test_struct.b[1],
                test_struct.b[2],
                test_struct.b[3],
            ],
            test_struct.c[0].encode(),
            test_struct.c[1].encode(),
            test_struct.d.encode(),
        ]
        .concat();

        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test(
            "structs",
            "struct_with_array",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        println!("executing:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &HashMap::default(),
            stdin,
            non_determinism,
            0,
        )
        .unwrap();
        if expected_output != vm_output.output {
            panic!(
                "expected_output:\n{}\ngot:\n{}\n. Code was:\n{}",
                expected_output.iter().join(", "),
                vm_output.output.iter().join(", "),
                test_program.iter().join("\n")
            );
        }
    }
}
