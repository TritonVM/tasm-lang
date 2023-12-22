use arbitrary::Arbitrary;
use triton_vm::Digest;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Arbitrary, BFieldCodec, Clone, Debug)]
struct NotCopyStruct {
    digests: Vec<Digest>,
    xfes: Vec<XFieldElement>,
}

#[allow(clippy::assertions_on_constants)]
#[allow(clippy::len_zero)]
fn main() {
    let a_stack_value: BFieldElement = BFieldElement::new(404);
    let boxed_enum_type: Box<NotCopyStruct> =
        NotCopyStruct::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();

    let as_ok: Result<Box<NotCopyStruct>, ()> = Ok(boxed_enum_type);
    assert!(as_ok.is_ok());

    match as_ok {
        Result::Ok(inner) => {
            tasm::tasm_io_write_to_stdout___u32(inner.digests.len() as u32);
            tasm::tasm_io_write_to_stdout___u32(inner.xfes.len() as u32);
            if inner.digests.len() > 0 {
                tasm::tasm_io_write_to_stdout___digest(inner.digests[0]);
            }
            if inner.digests.len() > 1 {
                tasm::tasm_io_write_to_stdout___digest(inner.digests[1]);
            }
            if inner.xfes.len() > 0 {
                tasm::tasm_io_write_to_stdout___xfe(inner.xfes[0]);
            }
            if inner.xfes.len() > 1 {
                tasm::tasm_io_write_to_stdout___xfe(inner.xfes[1]);
            }
        }
        Result::Err(_) => {
            assert!(false);
        }
    };

    let as_err: Result<Box<NotCopyStruct>, ()> = Err(());
    match as_err {
        Result::Ok(_) => {
            assert!(false);
        }
        Result::Err(_) => {
            tasm::tasm_io_write_to_stdout___bfe(a_stack_value);
        }
    };

    // assert!(as_ok.is_ok());
    assert!(as_err.is_err());

    return;
}

mod test {
    use arbitrary::Unstructured;
    use itertools::Itertools;
    use rand::random;
    use std::collections::HashMap;
    use std::default::Default;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn boxed_result_test() {
        for _ in 0..5 {
            let rand: [u8; 1000] = random();
            let sv = NotCopyStruct::arbitrary(&mut Unstructured::new(&rand)).unwrap();
            let non_determinism = init_memory_from(&sv, BFieldElement::new(84));
            let stdin = vec![];

            // Run program on host machine
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
            let test_program = ozk_parsing::compile_for_test(
                "result_types",
                "boxed_struct",
                "main",
                crate::ast_types::ListType::Unsafe,
            );
            println!("test_program:\n{}", test_program.iter().join("\n"));
            let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
                &test_program,
                vec![],
                &HashMap::default(),
                stdin,
                non_determinism.clone(),
                0,
            )
            .unwrap();
            assert_eq!(native_output, vm_output.output);
            println!("native_output: {native_output:#?}");
        }
    }
}
