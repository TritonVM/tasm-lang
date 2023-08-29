// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::{BFieldElement, Digest};
use twenty_first::shared_math::{bfield_codec::BFieldCodec, x_field_element::XFieldElement};

#[derive(TasmObject, BFieldCodec)]
struct TestStructWithVecs {
    pub a: BFieldElement,
    pub b: BFieldElement,
    pub c: Vec<Digest>,
    pub d: XFieldElement,
    pub e: Vec<Digest>,
    pub f: BFieldElement,
    pub g: Vec<XFieldElement>,
}

fn main() {
    let test_struct: Box<TestStructWithVecs> =
        TestStructWithVecs::decode(&tasm::load_from_memory(BFieldElement::new(300))).unwrap();
    let a: &BFieldElement = &test_struct.a; // Use 1 `&`, ignore the 2nd `&`.
    tasm::tasm_io_write_to_stdout_bfe(*a); // Implement both `*` and method `to_owned` to mean put this onto the stack. We might need exceptions for list though.

    let b: &BFieldElement = &test_struct.b;
    tasm::tasm_io_write_to_stdout_bfe(*b);

    let c_vector_length_u64: u64 = test_struct.c.len() as u64;
    let c_vector_length: u32 = test_struct.c.len() as u32;
    tasm::tasm_io_write_to_stdout_bfe(BFieldElement::new(c_vector_length as u64));
    tasm::tasm_io_write_to_stdout_bfe(BFieldElement::new(c_vector_length_u64));

    // TODO: Add reading/expressions with more struct fields.

    return;
}

mod tests {
    use super::*;
    use rand::random;
    use std::collections::HashMap;
    use triton_vm::BFieldElement;
    use twenty_first::shared_math::other::random_elements;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::{
        execute_compiled_with_stack_memory_and_ins_for_test, init_memory_from,
    };

    #[test]
    fn struct_with_vecs_test() {
        let test_struct = TestStructWithVecs {
            a: random(),
            b: random(),
            c: random_elements(12),
            d: random(),
            e: random_elements(25),
            f: random(),
            g: random_elements(9),
        };
        let non_determinism = init_memory_from(&test_struct, BFieldElement::new(300));
        let expected_output = vec![
            test_struct.a,
            test_struct.b,
            BFieldElement::new(12),
            BFieldElement::new(12),
        ];
        let input = vec![];

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(input.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test("struct_with_vecs");
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
