use crate::triton_vm::prelude::*;
use crate::triton_vm::twenty_first::shared_math::bfield_codec::BFieldCodec;
use crate::triton_vm::twenty_first::shared_math::x_field_element::XFieldElement;

use tasm_lib::structure::tasm_object::TasmObject;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(TasmObject, BFieldCodec)]
struct TestStructWithVecs {
    pub(crate) a: BFieldElement,
    pub(crate) b: BFieldElement,
    pub(crate) c: Vec<Digest>,
    pub(crate) d: XFieldElement,
    pub(crate) e: Vec<Digest>,
    pub(crate) f: BFieldElement,
    pub(crate) g: Vec<XFieldElement>,
    pub(crate) h: Vec<Vec<XFieldElement>>,
    pub(crate) i: Vec<Vec<Vec<XFieldElement>>>,
}

fn main() {
    let test_struct: Box<TestStructWithVecs> =
        TestStructWithVecs::decode(&tasm::load_from_memory(BFieldElement::new(300))).unwrap();

    // Print `a`
    let a: BFieldElement = test_struct.a;
    tasm::tasm_io_write_to_stdout___bfe(a);

    // Print `b`
    let b: BFieldElement = test_struct.b;
    tasm::tasm_io_write_to_stdout___bfe(b);

    // Print length of `c` three times
    let c_vector_length_u64: u64 = test_struct.c.len() as u64;
    let c_vector_length: u32 = test_struct.c.len() as u32;
    tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(c_vector_length as u64));
    tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(c_vector_length_u64));
    tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(test_struct.c.len() as u64));

    // Print element 1 of `c`
    tasm::tasm_io_write_to_stdout___digest(test_struct.c[1]);

    // Print element 7 of `g`
    tasm::tasm_io_write_to_stdout___xfe(test_struct.g[7]);

    // Print length of `e`
    tasm::tasm_io_write_to_stdout___u32(test_struct.e.len() as u32);

    // Print `d`
    tasm::tasm_io_write_to_stdout___xfe(test_struct.d);

    // Print elements from `h`
    tasm::tasm_io_write_to_stdout___xfe(test_struct.h[2][5]);
    tasm::tasm_io_write_to_stdout___xfe(test_struct.h[2][5]);
    tasm::tasm_io_write_to_stdout___xfe(test_struct.h[0][0]);
    tasm::tasm_io_write_to_stdout___xfe(test_struct.h[3][10]);
    tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(test_struct.h[3].len() as u64));
    tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(test_struct.h[0].len() as u64));
    tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(test_struct.h.len() as u64));

    // Print elements from `i`
    tasm::tasm_io_write_to_stdout___xfe(test_struct.i[2][3][4]);
    tasm::tasm_io_write_to_stdout___xfe(test_struct.i[4][3][2]);
    tasm::tasm_io_write_to_stdout___xfe(test_struct.i[0][0][0]);
    tasm::tasm_io_write_to_stdout___xfe(test_struct.i[5][0][0]);
    tasm::tasm_io_write_to_stdout___xfe(test_struct.i[0][5][0]);
    tasm::tasm_io_write_to_stdout___xfe(test_struct.i[0][0][5]);
    tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(test_struct.i[0][0].len() as u64));
    tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(test_struct.i[4][3].len() as u64));

    return;
}

#[cfg(test)]
mod test {

    use crate::triton_vm::prelude::*;
    use crate::triton_vm::twenty_first::shared_math::other::random_elements;
    use itertools::Itertools;
    use rand::random;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;

    use super::*;

    #[test]
    fn struct_with_vecs_test() {
        let c_list_length = 14;
        let test_struct = TestStructWithVecs {
            a: random(),
            b: random(),
            c: random_elements(c_list_length),
            d: random(),
            e: random_elements(25),
            f: random(),
            g: random_elements(9),
            h: vec![
                random_elements(10),
                random_elements(0),
                random_elements(7),
                random_elements(11),
            ],
            i: vec![
                // [0]
                vec![
                    random_elements(6), // [0][0]
                    random_elements(1),
                    random_elements(2),
                    random_elements(3),
                    random_elements(1), // [0][4]
                    random_elements(3),
                ],
                // [1]
                vec![],
                // [2]
                vec![
                    random_elements(6),
                    random_elements(1),
                    random_elements(1),
                    random_elements(5),
                ],
                // [3]
                vec![],
                // [4]
                vec![
                    random_elements(1),
                    random_elements(1),
                    random_elements(1),
                    random_elements(10), // [4][3]
                    random_elements(1),
                ],
                // [5]
                vec![random_elements(12)],
            ],
        };
        println!(
            "test_struct.encode()\n{}",
            test_struct.encode().iter().join("\n")
        );
        let non_determinism = init_memory_from(&test_struct, BFieldElement::new(300));
        let expected_output = vec![
            test_struct.a.encode(),
            test_struct.b.encode(),
            (c_list_length as u32).encode(),
            (c_list_length as u32).encode(),
            (c_list_length as u32).encode(),
            test_struct.c[1].encode(),
            test_struct.g[7].encode(),
            (test_struct.e.len() as u32).encode(),
            test_struct.d.encode(),
            test_struct.h[2][5].encode(),
            test_struct.h[2][5].encode(),
            test_struct.h[0][0].encode(),
            test_struct.h[3][10].encode(),
            (test_struct.h[3].len() as u32).encode(),
            (test_struct.h[0].len() as u32).encode(),
            (test_struct.h.len() as u32).encode(),
            test_struct.i[2][3][4].encode(),
            test_struct.i[4][3][2].encode(),
            test_struct.i[0][0][0].encode(),
            test_struct.i[5][0][0].encode(),
            test_struct.i[0][5][0].encode(),
            test_struct.i[0][0][5].encode(),
            (test_struct.i[0][0].len() as u32).encode(),
            (test_struct.i[4][3].len() as u32).encode(),
        ]
        .concat();
        let stdin = vec![];

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let entrypoint_location = EntrypointLocation::disk("structs", "struct_with_vecs", "main");
        let test_program =
            ozk_parsing::compile_for_test(&entrypoint_location, crate::ast_types::ListType::Unsafe);
        println!("executing:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            0,
        )
        .unwrap();
        // assert_eq!(expected_output, vm_output.output);
        if expected_output != vm_output.output {
            panic!(
                "expected_output:\n {}, got:\n{}. Code was:\n{}",
                expected_output.iter().join(", "),
                vm_output.output.iter().join(", "),
                test_program.iter().join("\n")
            );
        }
    }
}
