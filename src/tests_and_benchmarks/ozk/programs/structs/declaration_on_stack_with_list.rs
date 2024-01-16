use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::twenty_first::shared_math::bfield_codec::BFieldCodec;
use triton_vm::twenty_first::shared_math::x_field_element::XFieldElement;
use triton_vm::BFieldElement;
use triton_vm::Digest;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(TasmObject, BFieldCodec)]
struct TestStruct {
    a: BFieldElement,
    b: Vec<Digest>,
    c: Vec<BFieldElement>,
    d: XFieldElement,
}

fn main() {
    let mut test_struct: TestStruct = TestStruct {
        a: BFieldElement::new(0x0000000100000000u64),
        b: Vec::<Digest>::with_capacity(100),
        c: Vec::<BFieldElement>::with_capacity(200),
        d: XFieldElement::new([
            BFieldElement::new(0x0000000200000000u64),
            BFieldElement::new(0x0000000400000000u64),
            BFieldElement::new(0x0000000800000000u64),
        ]),
    };
    let digest_0: Digest = Digest::new([
        BFieldElement::new(1u64),
        BFieldElement::new(2u64),
        BFieldElement::new(3u64),
        BFieldElement::new(4u64),
        BFieldElement::new(5u64),
    ]);
    let digest_1: Digest = Digest::new([
        BFieldElement::new(6u64),
        BFieldElement::new(7u64),
        BFieldElement::new(8u64),
        BFieldElement::new(9u64),
        BFieldElement::new(10u64),
    ]);
    test_struct.b.push(digest_0);
    test_struct.b.push(digest_1);
    test_struct
        .c
        .push(BFieldElement::new(0x0000001000000000u64));
    test_struct
        .c
        .push(BFieldElement::new(0x0000002000000000u64));
    tasm::tasm_io_write_to_stdout___bfe(test_struct.a);
    tasm::tasm_io_write_to_stdout___digest(test_struct.b[0]);
    tasm::tasm_io_write_to_stdout___digest(test_struct.b[1]);
    tasm::tasm_io_write_to_stdout___bfe(test_struct.c[1]);
    tasm::tasm_io_write_to_stdout___bfe(test_struct.c[0]);

    return;
}

#[cfg(test)]
mod test {

    use triton_vm::BFieldElement;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;

    use super::*;

    #[test]
    fn declaration_on_stack_with_list_test() {
        // Test function on host machine
        let expected_output = [
            vec![BFieldElement::new(1u64 << 32)],
            vec![
                BFieldElement::new(1u64),
                BFieldElement::new(2u64),
                BFieldElement::new(3u64),
                BFieldElement::new(4u64),
                BFieldElement::new(5u64),
                BFieldElement::new(6u64),
                BFieldElement::new(7u64),
                BFieldElement::new(8u64),
                BFieldElement::new(9u64),
                BFieldElement::new(10u64),
            ],
            vec![BFieldElement::new(0x0000002000000000u64)],
            vec![BFieldElement::new(0x0000001000000000u64)],
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
            EntrypointLocation::disk("structs", "declaration_on_stack_with_list", "main");
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
    }
}
