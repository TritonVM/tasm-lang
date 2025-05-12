use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn output_ilog2() {
    let value: u32 = tasm::tasmlib_io_read_stdin___u32();
    tasm::tasmlib_io_write_to_stdout___u32(value.ilog2());

    return;
}

#[cfg(test)]
mod test {
    use num::Zero;
    use test_strategy::proptest;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[proptest(cases = 10)]
    fn ilog2_proptest(value: u32) {
        let entrypoint = EntrypointLocation::disk("arithmetic", "ilog2_u32", "output_ilog2");
        let std_in = vec![BFieldElement::new(value as u64)];
        let native_output = rust_shadows::wrap_main_with_io(&output_ilog2)(
            std_in.clone(),
            NonDeterminism::default(),
        );
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in.clone())
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn ilog2_unit_test() {
        let entrypoint = EntrypointLocation::disk("arithmetic", "ilog2_u32", "output_ilog2");
        let test_case = TritonVMTestCase::new(entrypoint);
        let compiled = test_case.compile();

        let test_cases = (1..100).chain([u32::MAX - 1, u32::MAX]);
        for i in test_cases {
            let std_in = vec![BFieldElement::new(i as u64)];
            let native_output = rust_shadows::wrap_main_with_io(&output_ilog2)(
                std_in.clone(),
                NonDeterminism::default(),
            );

            let vm_output = execute_compiled_with_stack_and_ins_for_test(
                &compiled,
                vec![],
                std_in,
                NonDeterminism::default(),
                0,
            )
            .unwrap();
            assert_eq!(native_output, vm_output.public_output);
        }
    }

    #[test]
    fn ilog2_of_zero_verify_crash_negative_test() {
        let std_in = vec![BFieldElement::zero()];
        let entrypoint = EntrypointLocation::disk("arithmetic", "ilog2_u32", "output_ilog2");
        let err = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in.clone())
            .execute()
            .unwrap_err();
        let err = err.downcast::<InstructionError>().unwrap();
        assert_eq!(InstructionError::LogarithmOfZero, err);

        // Verify that host-machine execution environment also crashes
        let host_machine_execution_result = std::panic::catch_unwind(|| {
            rust_shadows::wrap_main_with_io(&output_ilog2)(
                std_in.clone(),
                NonDeterminism::default(),
            )
        });
        assert!(host_machine_execution_result.is_err());
    }
}
