use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

pub fn recufy() {
    tasm::tasm_recufier_assert_stdin_starts_with_own_program_digest();
    return;
}

#[cfg(test)]
mod tests {
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
    use crate::triton_vm::prelude::*;

    use super::*;

    #[test]
    fn recursive_verification() {
        let entrypoint_location = EntrypointLocation::disk("recufier", "verify", "recufy");
        let test_case = TritonVMTestCase::new(entrypoint_location);
        let code = test_case.compile();
        let program = Program::new(&code);
        let program_digest = program.hash::<Tip5>();
        let std_in = program_digest.reversed().values().to_vec();

        let native_output =
            rust_shadows::wrap_main_with_io(&recufy)(std_in.clone(), NonDeterminism::default());

        let vm_output = test_case.with_std_in(std_in).execute().unwrap();

        assert_eq!(native_output, vm_output.output);
    }
}
