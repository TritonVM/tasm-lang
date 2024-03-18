use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[allow(clippy::vec_init_then_push)]
fn succeed_conversion() {
    let mut as_vec: Vec<XFieldElement> = Vec::<XFieldElement>::default();
    as_vec.push(tasm::tasm_io_read_stdin___xfe());
    as_vec.push(tasm::tasm_io_read_stdin___xfe());
    as_vec.push(tasm::tasm_io_read_stdin___xfe());
    let as_array: [XFieldElement; 3] = <[XFieldElement; 3]>::try_from(as_vec).unwrap();
    tasm::tasm_io_write_to_stdout___xfe(as_array[0]);
    tasm::tasm_io_write_to_stdout___xfe(as_array[1]);
    tasm::tasm_io_write_to_stdout___xfe(as_array[2]);

    return;
}

#[allow(clippy::vec_init_then_push)]
fn _fail_conversion() {
    let mut as_vec: Vec<XFieldElement> = Vec::<XFieldElement>::default();
    as_vec.push(tasm::tasm_io_read_stdin___xfe());
    as_vec.push(tasm::tasm_io_read_stdin___xfe());
    as_vec.push(tasm::tasm_io_read_stdin___xfe());
    let conv_res: Result<[XFieldElement; 4], _> = <[XFieldElement; 4]>::try_from(as_vec);
    let boxed_conv_res: Box<Result<[XFieldElement; 4], _>> =
        Box::<Result<[XFieldElement; 4], _>>::new(conv_res);
    assert!(boxed_conv_res.is_err());
    assert!(!boxed_conv_res.is_ok());

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
    fn vec_to_array_positive_test() {
        let std_in = random_elements(3 * EXTENSION_DEGREE);
        let native_output = rust_shadows::wrap_main_with_io(&succeed_conversion)(
            std_in.clone(),
            NonDeterminism::default(),
        );
        let vm_output = TritonVMTestCase::new(EntrypointLocation::disk(
            "arrays",
            "vec_to_array",
            "succeed_conversion",
        ))
        .with_std_in(std_in.clone())
        .execute()
        .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn vec_to_array_negative_test() {
        let std_in = random_elements(3 * EXTENSION_DEGREE);
        assert!(TritonVMTestCase::new(EntrypointLocation::disk(
            "arrays",
            "vec_to_array",
            "_fail_conversion",
        ))
        .with_std_in(std_in)
        .execute()
        .is_ok());
    }
}
