use tasm_lib::triton_vm::prelude::*;

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
    use std::default::Default;

    use tasm_lib::twenty_first::shared_math::other::random_elements;

    use itertools::Itertools;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn simple_unwrap_test() {
        let stdin = random_elements(8);
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let entrypoint_location = EntrypointLocation::disk("result_types", "simple_unwrap", "main");
        let test_program = ozk_parsing::compile_for_test(&entrypoint_location);
        let expected_stack_diff = 0;
        println!("test_program:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            NonDeterminism::default(),
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
