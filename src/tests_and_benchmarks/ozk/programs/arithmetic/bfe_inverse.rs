use num::One;
use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let bfe: BFieldElement = tasm::tasmlib_io_read_stdin___bfe();
    tasm::tasmlib_io_write_to_stdout___bfe(bfe.inverse());

    assert!(BFieldElement::one() == bfe.inverse() * bfe);

    return;
}

#[cfg(test)]
mod test {
    use std::panic::catch_unwind;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows::wrap_main_with_io;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use num::Zero;
    use tasm_lib::twenty_first::math::other::random_elements;

    use super::*;

    #[test]
    fn bfe_inverse_test() {
        let std_in = random_elements(1);
        let native_output = wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
        let entrypoint = EntrypointLocation::disk("arithmetic", "bfe_inverse", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn bfe_crash_on_zero_input_arg() {
        let std_in = vec![BFieldElement::zero()];
        catch_unwind(|| {
            let rust_program = wrap_main_with_io(&main);
            rust_program(std_in.clone(), NonDeterminism::default());
        })
        .unwrap_err();

        let entrypoint = EntrypointLocation::disk("arithmetic", "bfe_inverse", "main");
        let err = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap_err();
        let err = err.downcast::<InstructionError>().unwrap();
        assert_eq!(InstructionError::InverseOfZero, err);
    }
}
