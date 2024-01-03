use triton_vm::Digest;
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[allow(clippy::unnecessary_literal_unwrap)]
#[allow(clippy::assertions_on_constants)]
#[allow(unused_assignments)]
fn main() {
    let mut none_value: Option<u64> = None;
    assert!(none_value.is_none());
    none_value = None;
    assert!(none_value.is_none());

    let mut none_to_some_value: Option<u64> = None;
    assert!(none_to_some_value.is_none());
    none_to_some_value = Some((1u64 << 40) + 100);
    assert!(none_to_some_value.is_some());

    let mut some_to_none_value: Option<XFieldElement> = Some(tasm::tasm_io_read_stdin___xfe());
    tasm::tasm_io_write_to_stdout___xfe(some_to_none_value.unwrap());
    assert!(some_to_none_value.is_some());
    some_to_none_value = None;
    assert!(some_to_none_value.is_none());

    let mut some_value: Option<Digest> = Some(tasm::tasm_io_read_stdin___digest());
    tasm::tasm_io_write_to_stdout___digest(some_value.unwrap());
    assert!(some_value.is_some());
    some_value = Some(tasm::tasm_io_read_stdin___digest());
    tasm::tasm_io_write_to_stdout___digest(some_value.unwrap());
    assert!(some_value.is_some());

    // Match statement to ensure stack size is tracked correctly
    match none_to_some_value {
        None => {
            assert!(false);
        }
        Some(payload) => {
            tasm::tasm_io_write_to_stdout___u64(payload);
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
    fn mutable_values_test() {
        let stdin = vec![random(); 13];
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let test_program = ozk_parsing::compile_for_test(
            "option_types",
            "mutable_values",
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
