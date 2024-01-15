use arbitrary::Arbitrary;
use triton_vm::twenty_first::shared_math::bfield_codec::BFieldCodec;
use triton_vm::twenty_first::shared_math::x_field_element::XFieldElement;
use triton_vm::BFieldElement;
use triton_vm::Digest;

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
    let boxed_struct: Box<NotCopyStruct> =
        NotCopyStruct::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();

    let as_ok: Result<Box<NotCopyStruct>, ()> = Ok(boxed_struct);
    let boxed_as_ok: Box<Result<Box<NotCopyStruct>, ()>> =
        Box::<Result<Box<NotCopyStruct>, ()>>::new(as_ok);
    assert!(boxed_as_ok.is_ok());
    assert!(!boxed_as_ok.is_err());

    let boxed_struct_again: Box<NotCopyStruct> =
        NotCopyStruct::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    let as_ok_again: Result<Box<NotCopyStruct>, ()> = Ok(boxed_struct_again);
    match as_ok_again {
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

    let boxed_as_err: Box<Result<Box<NotCopyStruct>, ()>> =
        Box::<Result<Box<NotCopyStruct>, ()>>::new(as_err);
    assert!(!boxed_as_err.is_ok());
    assert!(boxed_as_err.is_err());

    return;
}

mod test {
    use std::collections::HashMap;
    use std::default::Default;

    use arbitrary::Unstructured;
    use itertools::Itertools;
    use rand::random;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
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
