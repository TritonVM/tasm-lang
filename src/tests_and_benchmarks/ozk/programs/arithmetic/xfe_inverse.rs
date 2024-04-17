use num::One;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::math::traits::Inverse;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let xfe: XFieldElement = tasm::tasmlib_io_read_stdin___xfe();
    tasm::tasmlib_io_write_to_stdout___xfe(xfe.inverse());

    assert!(XFieldElement::one() == xfe.inverse() * xfe);

    return;
}

fn no_name_clash_on_bfe_and_xfe_inverse() {
    let xfe: XFieldElement = tasm::tasmlib_io_read_stdin___xfe();
    let bfe: BFieldElement = tasm::tasmlib_io_read_stdin___bfe();

    assert!(XFieldElement::one() == xfe.inverse() * xfe);
    assert!(BFieldElement::one() == bfe.inverse() * bfe);

    tasm::tasmlib_io_write_to_stdout___xfe(xfe.inverse());
    tasm::tasmlib_io_write_to_stdout___bfe(bfe.inverse());

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows::wrap_main_with_io;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use num::Zero;
    use std::panic::catch_unwind;
    use tasm_lib::twenty_first::math::other::random_elements;
    use tasm_lib::twenty_first::math::x_field_element::EXTENSION_DEGREE;

    #[test]
    fn xfe_inverse_test() {
        let std_in = random_elements(EXTENSION_DEGREE);
        let native_output = wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
        let entrypoint = EntrypointLocation::disk("arithmetic", "xfe_inverse", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn no_name_clash_bfe_xfe_inverse_test() {
        let std_in = random_elements(EXTENSION_DEGREE + 1);
        let native_output = wrap_main_with_io(&no_name_clash_on_bfe_and_xfe_inverse)(
            std_in.clone(),
            NonDeterminism::default(),
        );
        let entrypoint = EntrypointLocation::disk(
            "arithmetic",
            "xfe_inverse",
            "no_name_clash_on_bfe_and_xfe_inverse",
        );
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn xfe_crash_on_zero_input_arg() {
        let std_in = vec![BFieldElement::zero(); EXTENSION_DEGREE];
        catch_unwind(|| {
            let rust_program = wrap_main_with_io(&main);
            rust_program(std_in.clone(), NonDeterminism::default());
        })
        .unwrap_err();

        let entrypoint = EntrypointLocation::disk("arithmetic", "xfe_inverse", "main");
        let err = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap_err();
        let err = err.downcast::<InstructionError>().unwrap();
        assert_eq!(InstructionError::InverseOfZero, err);
    }
}
