use triton_vm::{BFieldElement, Digest};
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[allow(clippy::unnecessary_literal_unwrap)]
#[allow(clippy::assertions_on_constants)]
fn main() {
    let bfe: BFieldElement = tasm::tasm_io_read_stdin___bfe();
    let ok_bfe: Result<BFieldElement, ()> = Ok(bfe);
    match ok_bfe {
        Ok(inner) => {
            tasm::tasm_io_write_to_stdout___bfe(inner);
        }
        Err(_) => {
            assert!(false);
        }
    };

    let boxed_ok_bfe: Box<Result<BFieldElement, ()>> =
        Box::<Result<BFieldElement, ()>>::new(ok_bfe);
    assert!(boxed_ok_bfe.is_ok());

    let xfe: XFieldElement = XFieldElement::new([
        BFieldElement::new(14),
        BFieldElement::new(15),
        BFieldElement::new(16),
    ]);
    let ok_xfe: Result<XFieldElement, ()> = Ok(xfe);
    let boxed_ok_xfe: Box<Result<XFieldElement, ()>> =
        Box::<Result<XFieldElement, ()>>::new(ok_xfe);
    assert!(boxed_ok_xfe.is_ok());

    let digest: Digest = tasm::tasm_io_read_stdin___digest();
    let ok_digest: Result<Digest, ()> = Ok(digest);
    tasm::tasm_io_write_to_stdout___digest(ok_digest.unwrap());
    let boxed_ok_digest: Box<Result<Digest, ()>> = Box::<Result<Digest, ()>>::new(ok_digest);
    assert!(boxed_ok_digest.is_ok());
    assert!(!boxed_ok_digest.is_err());

    tasm::tasm_io_write_to_stdout___bfe(bfe);

    match ok_bfe {
        Result::Ok(bfe_again) => {
            tasm::tasm_io_write_to_stdout___bfe(bfe_again);
            assert!(bfe == bfe_again);
        }
        Result::Err(_) => {
            assert!(false);
        }
    };

    match ok_xfe {
        Result::Ok(xfe_again) => {
            tasm::tasm_io_write_to_stdout___bfe(bfe);
            tasm::tasm_io_write_to_stdout___xfe(xfe_again);
            tasm::tasm_io_write_to_stdout___bfe(bfe);
            assert!(xfe == xfe_again);
        }
        Result::Err(_) => {
            assert!(false);
        }
    };

    match ok_digest {
        Result::Ok(digest_again) => {
            tasm::tasm_io_write_to_stdout___digest(digest_again);
            assert!(digest == digest_again);
        }
        Result::Err(_) => {
            assert!(false);
        }
    };

    let bfe_err: Result<BFieldElement, ()> = Err(());
    let xfe_err: Result<XFieldElement, ()> = Err(());
    let digest_err: Result<Digest, ()> = Err(());
    match bfe_err {
        Result::Ok(_) => {
            assert!(false);
        }
        Result::Err(_) => {
            tasm::tasm_io_write_to_stdout___bfe(bfe);
        }
    };
    match xfe_err {
        Result::Ok(_) => {
            assert!(false);
        }
        Result::Err(_) => {
            tasm::tasm_io_write_to_stdout___bfe(bfe);
        }
    };
    match digest_err {
        Result::Ok(_) => {
            assert!(false);
        }
        Result::Err(_) => {
            tasm::tasm_io_write_to_stdout___bfe(bfe);
        }
    };

    tasm::tasm_io_write_to_stdout___xfe(xfe);
    tasm::tasm_io_write_to_stdout___bfe(bfe);

    return;
}

mod test {
    use std::collections::HashMap;
    use std::default::Default;

    use itertools::Itertools;
    use rand::random;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn copy_types_test() {
        let stdin = vec![random(), random(), random(), random(), random(), random()];
        println!("stdin: {stdin:#?}");
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let test_program = ozk_parsing::compile_for_test(
            "result_types",
            "copy_types",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        let expected_stack_diff = 0;
        println!("test_program:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &HashMap::default(),
            stdin,
            NonDeterminism::default(),
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
