use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use tasm_lib::triton_vm::prelude::*;

#[allow(clippy::unnecessary_literal_unwrap)]
#[allow(clippy::single_match)]
fn main() {
    // None -> None
    let mut none_value: Option<u64> = None;
    match none_value {
        Some(_) => {
            panic!();
        }
        None => {}
    };
    none_value = None;
    match none_value {
        Some(_) => {
            panic!();
        }
        None => {}
    };

    // None -> Some
    let mut none_to_some_value: Option<u64> = None;
    match none_to_some_value {
        Some(_) => {
            panic!();
        }
        None => {}
    };
    none_to_some_value = Some((1u64 << 40) + 100);
    match none_to_some_value {
        Some(x) => {
            tasm::tasm_io_write_to_stdout___u64(x);
        }
        None => {
            panic!();
        }
    };

    // Some -> None
    let mut some_to_none_value: Option<XFieldElement> = Some(tasm::tasm_io_read_stdin___xfe());
    tasm::tasm_io_write_to_stdout___xfe(some_to_none_value.unwrap());
    match some_to_none_value {
        Some(x) => {
            tasm::tasm_io_write_to_stdout___xfe(x);
        }
        None => {
            panic!();
        }
    };
    some_to_none_value = None;
    match some_to_none_value {
        Some(_) => {
            panic!();
        }
        None => {}
    };

    // Some -> Some
    let mut some_value: Option<Digest> = Some(tasm::tasm_io_read_stdin___digest());
    tasm::tasm_io_write_to_stdout___digest(some_value.unwrap());
    match some_value {
        Some(x) => {
            tasm::tasm_io_write_to_stdout___digest(x);
        }
        None => {
            panic!();
        }
    };
    some_value = Some(tasm::tasm_io_read_stdin___digest());
    tasm::tasm_io_write_to_stdout___digest(some_value.unwrap());
    match some_value {
        Some(x) => {
            tasm::tasm_io_write_to_stdout___digest(x);
        }
        None => {
            panic!();
        }
    };

    return;
}

mod test {
    use std::default::Default;

    use rand::random;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn mutable_values_test() {
        let stdin = vec![random(); 13];
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let entrypoint_location =
            EntrypointLocation::disk("option_types", "mutable_values", "main");
        let test_program = ozk_parsing::compile_for_test(&entrypoint_location);
        let expected_stack_diff = 0;
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            NonDeterminism::default(),
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
