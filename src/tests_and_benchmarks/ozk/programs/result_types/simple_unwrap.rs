use tasm_lib::Digest;
use triton_vm::BFieldElement;
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[allow(clippy::unnecessary_literal_unwrap)]
fn main() {
    let bfe: BFieldElement = BFieldElement::new(500u64);
    let result_bfe: Result<BFieldElement, ()> = Ok(bfe);
    let bfe_again: BFieldElement = result_bfe.unwrap();
    assert!(bfe == bfe_again);

    let xfe: XFieldElement = tasm::tasm_io_read_stdin___xfe();
    let result_xfe: Result<XFieldElement, ()> = Ok(xfe);
    let xfe_again: XFieldElement = result_xfe.unwrap();
    assert!(xfe == xfe_again);

    let digest: Digest = tasm::tasm_io_read_stdin___digest();
    let result_digest: Result<Digest, ()> = Ok(digest);
    let digest_again: Digest = result_digest.unwrap();
    assert!(digest == digest_again);

    return;
}

mod test {
    use itertools::Itertools;
    use std::collections::HashMap;
    use std::default::Default;
    use triton_vm::NonDeterminism;
    use twenty_first::shared_math::other::random_elements;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn simple_unwrap_test() {
        let stdin = random_elements(8);
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let test_program = ozk_parsing::compile_for_test(
            "result_types",
            "simple_unwrap",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        let expected_stack_diff = 0;
        println!("test_program:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &mut HashMap::default(),
            stdin,
            NonDeterminism::default(),
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}