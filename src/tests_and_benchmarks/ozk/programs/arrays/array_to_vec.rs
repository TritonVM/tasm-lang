use num::Zero;
use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let mut array: [XFieldElement; 3] = [XFieldElement::zero(); 3];
    array[0] = tasm::tasm_io_read_stdin___xfe();
    array[1] = tasm::tasm_io_read_stdin___xfe();
    array[2] = tasm::tasm_io_read_stdin___xfe();
    let as_vec: Vec<XFieldElement> = array.to_vec();
    tasm::tasm_io_write_to_stdout___xfe(as_vec[0]);
    tasm::tasm_io_write_to_stdout___xfe(as_vec[1]);
    tasm::tasm_io_write_to_stdout___xfe(as_vec[2]);

    return;
}

#[cfg(test)]
mod test {
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use tasm_lib::twenty_first::shared_math::other::random_elements;
    use tasm_lib::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

    use super::*;

    #[test]
    fn array_to_vec() {
        let std_in = random_elements(3 * EXTENSION_DEGREE);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
        let entrypoint_add = EntrypointLocation::disk("arrays", "array_to_vec", "main");
        let vm_output = TritonVMTestCase::new(entrypoint_add)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
