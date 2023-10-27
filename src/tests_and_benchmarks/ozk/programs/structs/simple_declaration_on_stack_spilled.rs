use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::BFieldElement;
use triton_vm::Digest;
use twenty_first::shared_math::bfield_codec::BFieldCodec;
use twenty_first::shared_math::x_field_element::XFieldElement;

#[derive(TasmObject, BFieldCodec)]
struct TestStruct {
    a: BFieldElement,
    b: Vec<XFieldElement>,
    d: Digest,
    h: u128,
    i: u128,
}

fn main() {
    let val_0: u128 = 14;
    let mut test_struct: TestStruct = TestStruct {
        b: Vec::<XFieldElement>::with_capacity(14),
        d: Digest::new([
            BFieldElement::new(2u64),
            BFieldElement::new(4u64),
            BFieldElement::new(8u64),
            BFieldElement::new(16u64),
            BFieldElement::new(32u64),
        ]),
        a: BFieldElement::new(14u64 << 40),
        h: u128::MAX,
        i: u128::MAX - 4,
    };
    test_struct.b.push(XFieldElement::new([
        BFieldElement::new(0x0010000000000001u64),
        BFieldElement::new(0x0020000000000004u64),
        BFieldElement::new(0x0040000000000002u64),
    ]));
    let val_1: u128 = 14 << 101;
    let val_2: u128 = 15 << 101;
    let val_3: u128 = 16 << 101;
    let val_4: u128 = 17 << 101;
    tasm::tasm_io_write_to_stdout___u128(test_struct.i);
    tasm::tasm_io_write_to_stdout___u128(test_struct.i);
    let a: BFieldElement = test_struct.a;
    tasm::tasm_io_write_to_stdout___bfe(test_struct.a);
    tasm::tasm_io_write_to_stdout___bfe(a);
    tasm::tasm_io_write_to_stdout___digest(test_struct.d);
    tasm::tasm_io_write_to_stdout___u128(test_struct.h);
    tasm::tasm_io_write_to_stdout___u128(test_struct.h);
    tasm::tasm_io_write_to_stdout___u128(test_struct.i);
    tasm::tasm_io_write_to_stdout___u128(val_0);
    tasm::tasm_io_write_to_stdout___u128(val_1);
    tasm::tasm_io_write_to_stdout___u128(val_2);
    tasm::tasm_io_write_to_stdout___u128(val_3);
    tasm::tasm_io_write_to_stdout___u128(val_4);

    // Change a field value in the struct on stack
    test_struct.d = Digest::new([
        BFieldElement::new(4u64),
        BFieldElement::new(8u64),
        BFieldElement::new(16u64),
        BFieldElement::new(32u64),
        BFieldElement::new(64u64),
    ]);
    tasm::tasm_io_write_to_stdout___digest(test_struct.d);

    tasm::tasm_io_write_to_stdout___xfe(test_struct.b[0]);

    return;
}

mod tests {
    use super::*;
    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_memory_and_ins_for_test;
    use std::collections::HashMap;
    use triton_vm::{BFieldElement, NonDeterminism};

    #[test]
    fn simple_declaration_on_stack_spilled_test() {
        // Test function on host machine
        let expected_output = [
            (u128::MAX - 4).encode(),
            (u128::MAX - 4).encode(),
            vec![BFieldElement::new(14u64 << 40)],
            vec![BFieldElement::new(14u64 << 40)],
            vec![
                BFieldElement::new(2u64),
                BFieldElement::new(4u64),
                BFieldElement::new(8u64),
                BFieldElement::new(16u64),
                BFieldElement::new(32u64),
            ],
            u128::MAX.encode(),
            u128::MAX.encode(),
            (u128::MAX - 4).encode(),
            14u128.encode(),
            (14u128 << 101).encode(),
            (15u128 << 101).encode(),
            (16u128 << 101).encode(),
            (17u128 << 101).encode(),
            vec![
                BFieldElement::new(4u64),
                BFieldElement::new(8u64),
                BFieldElement::new(16u64),
                BFieldElement::new(32u64),
                BFieldElement::new(64u64),
            ],
            XFieldElement::new([
                BFieldElement::new(0x0010000000000001u64),
                BFieldElement::new(0x0020000000000004u64),
                BFieldElement::new(0x0040000000000002u64),
            ])
            .encode(),
        ]
        .concat();
        let stdin = vec![];
        let non_determinism = NonDeterminism::default();

        // Run on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run on Triton-VM
        let test_program = ozk_parsing::compile_for_test(
            "structs",
            "simple_declaration_on_stack_spilled",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &mut HashMap::default(),
            stdin,
            non_determinism,
            0,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.output);
    }
}
