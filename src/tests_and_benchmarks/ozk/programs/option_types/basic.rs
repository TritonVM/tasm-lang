use triton_vm::{BFieldElement, Digest};
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[allow(clippy::unnecessary_literal_unwrap)]
#[allow(clippy::assertions_on_constants)]
fn main() {
    let bfe: BFieldElement = tasm::tasm_io_read_stdin___bfe();
    let some_bfe: Option<BFieldElement> = Some(bfe);
    assert!(some_bfe.is_some());
    tasm::tasm_io_write_to_stdout___u64(some_bfe.unwrap().value());

    let xfe: XFieldElement = tasm::tasm_io_read_stdin___xfe();
    let some_xfe: Option<XFieldElement> = Some(xfe);
    assert!(some_xfe.is_some());
    tasm::tasm_io_write_to_stdout___xfe(some_xfe.unwrap());

    let none_xfe: Option<XFieldElement> = None;
    assert!(none_xfe.is_none());
    let none_digest: Option<Digest> = None;
    assert!(none_digest.is_none());
    let none_bfe: Option<BFieldElement> = None;
    assert!(none_bfe.is_none());

    match some_xfe {
        Some(inner) => {
            tasm::tasm_io_write_to_stdout___xfe(inner);
        }
        None => {
            assert!(false);
        }
    };

    match some_bfe {
        Some(inner) => {
            tasm::tasm_io_write_to_stdout___bfe(inner);
        }
        None => {
            assert!(false);
        }
    };
    match some_bfe {
        Some(inner) => {
            tasm::tasm_io_write_to_stdout___bfe(inner);
        }
        _ => {
            assert!(false);
        }
    };

    match none_xfe {
        Some(_) => {
            assert!(false);
        }
        None => {
            tasm::tasm_io_write_to_stdout___u32(100);
        }
    };
    match none_xfe {
        Some(_) => {
            assert!(false);
        }
        _ => {
            tasm::tasm_io_write_to_stdout___u32(100);
        }
    };

    match none_digest {
        Some(_) => {
            assert!(false);
        }
        None => {
            tasm::tasm_io_write_to_stdout___u32(101);
        }
    };

    return;
}

mod test {
    use std::collections::HashMap;
    use std::default::Default;

    use rand::random;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn simple_is_some_test() {
        let stdin = vec![random(), random(), random(), random()];
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let test_program = ozk_parsing::compile_for_test(
            "option_types",
            "basic",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        let expected_stack_diff = 0;
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
