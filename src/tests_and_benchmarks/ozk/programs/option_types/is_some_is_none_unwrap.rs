use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[allow(clippy::unnecessary_literal_unwrap)]
fn main() {
    let bfe: BFieldElement = tasm::tasmlib_io_read_stdin___bfe();
    let some_bfe_boxed: Box<Option<BFieldElement>> = Box::<Option<BFieldElement>>::new(Some(bfe));
    assert!(some_bfe_boxed.is_some());
    let some_bfe_stack: Option<BFieldElement> = Some(bfe);
    tasm::tasmlib_io_write_to_stdout___u64(some_bfe_stack.unwrap().value());

    let xfe: XFieldElement = tasm::tasmlib_io_read_stdin___xfe();
    let some_xfe_boxed: Box<Option<XFieldElement>> = Box::<Option<XFieldElement>>::new(Some(xfe));
    assert!(some_xfe_boxed.is_some());
    let some_xfe_stack: Option<XFieldElement> = Some(xfe);
    tasm::tasmlib_io_write_to_stdout___xfe(some_xfe_stack.unwrap());
    let none_xfe_stack: Option<XFieldElement> = None;
    let none_xfe_boxed: Box<Option<XFieldElement>> =
        Box::<Option<XFieldElement>>::new(none_xfe_stack);
    assert!(none_xfe_boxed.is_none());

    let none_digest_stack: Option<Digest> = None;
    let none_digest_boxed: Box<Option<Digest>> = Box::<Option<Digest>>::new(none_digest_stack);
    assert!(none_digest_boxed.is_none());

    match some_xfe_boxed.as_ref() {
        Some(inner) => {
            tasm::tasmlib_io_write_to_stdout___xfe(*inner);
        }
        None => {
            panic!();
        }
    };
    match some_xfe_stack {
        Some(inner) => {
            tasm::tasmlib_io_write_to_stdout___xfe(inner);
        }
        None => {
            panic!();
        }
    };

    match some_bfe_boxed.as_ref() {
        Some(inner) => {
            tasm::tasmlib_io_write_to_stdout___bfe(*inner);
        }
        None => {
            panic!();
        }
    };
    match some_bfe_boxed.as_ref() {
        Some(inner) => {
            tasm::tasmlib_io_write_to_stdout___bfe(*inner);
        }
        _ => {
            panic!();
        }
    };

    match none_xfe_boxed.as_ref() {
        Some(_) => {
            panic!();
        }
        None => {
            tasm::tasmlib_io_write_to_stdout___u32(100);
        }
    };
    match none_xfe_boxed.as_ref() {
        Some(_) => {
            panic!();
        }
        _ => {
            tasm::tasmlib_io_write_to_stdout___u32(100);
        }
    };

    match none_digest_boxed.as_ref() {
        Some(_) => {
            panic!();
        }
        None => {
            tasm::tasmlib_io_write_to_stdout___u32(101);
        }
    };

    return;
}

mod test {
    use std::default::Default;

    use rand::random;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn simple_is_some_test() {
        let stdin = vec![random(), random(), random(), random()];
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let entrypoint_location =
            EntrypointLocation::disk("option_types", "is_some_is_none_unwrap", "main");
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
