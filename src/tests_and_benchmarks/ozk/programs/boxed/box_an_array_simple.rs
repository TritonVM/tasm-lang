#![allow(clippy::needless_borrow)]
use triton_vm::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let array: [BFieldElement; 2] = [BFieldElement::new(101), BFieldElement::new(102)];
    let boxed_array: Box<[BFieldElement; 2]> = Box::<[BFieldElement; 2]>::new(array);
    tasm::tasm_io_write_to_stdout___bfe(boxed_array[0]);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use triton_vm::NonDeterminism;

    #[test]
    fn box_an_array_simple() {
        let entrypoint_location = EntrypointLocation::disk("boxed", "box_an_array_simple", "main");
        let test_program =
            ozk_parsing::compile_for_test(&entrypoint_location, crate::ast_types::ListType::Unsafe);

        let stdin = vec![];
        let non_determinism = NonDeterminism::default();
        let expected_stack_diff = 0;
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap()
        .output;
        assert_eq!(native_output, vm_output);
    }
}
