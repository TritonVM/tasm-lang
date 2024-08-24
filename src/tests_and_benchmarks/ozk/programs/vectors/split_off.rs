use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn xfe_vec() {
    let orignal_vec_length: usize = tasm::tasmlib_io_read_stdin___u32() as usize;
    let mut original_vec: Vec<XFieldElement> = Vec::<XFieldElement>::default();

    {
        let mut i: usize = 0;
        while i < orignal_vec_length {
            original_vec.push(tasm::tasmlib_io_read_stdin___xfe());
            i += 1;
        }
    }

    let mid: usize = tasm::tasmlib_io_read_stdin___u32() as usize;

    let new_vec: Vec<XFieldElement> = original_vec.split_off(mid);

    // Check original vec
    assert!(mid == original_vec.len());
    {
        let mut i: usize = 0;
        while i < mid {
            tasm::tasmlib_io_write_to_stdout___xfe(original_vec[i]);
            i += 1;
        }
    }

    // Check new vec
    assert!(orignal_vec_length - mid == new_vec.len());
    {
        let mut i: usize = 0;
        while i < new_vec.len() {
            tasm::tasmlib_io_write_to_stdout___xfe(new_vec[i]);
            i += 1;
        }
    }

    return;
}

fn bfe_vec() {
    let orignal_vec_length: usize = tasm::tasmlib_io_read_stdin___u32() as usize;
    let mut original_vec: Vec<BFieldElement> = Vec::<BFieldElement>::default();

    {
        let mut i: usize = 0;
        while i < orignal_vec_length {
            original_vec.push(tasm::tasmlib_io_read_stdin___bfe());
            i += 1;
        }
    }

    let mid: usize = tasm::tasmlib_io_read_stdin___u32() as usize;

    let new_vec: Vec<BFieldElement> = original_vec.split_off(mid);

    // Check original vec
    assert!(mid == original_vec.len());
    {
        let mut i: usize = 0;
        while i < mid {
            tasm::tasmlib_io_write_to_stdout___bfe(original_vec[i]);
            i += 1;
        }
    }

    // Check new vec
    assert!(orignal_vec_length - mid == new_vec.len());
    {
        let mut i: usize = 0;
        while i < new_vec.len() {
            tasm::tasmlib_io_write_to_stdout___bfe(new_vec[i]);
            i += 1;
        }
    }

    return;
}

fn digest_vec() {
    let orignal_vec_length: usize = tasm::tasmlib_io_read_stdin___u32() as usize;
    let mut original_vec: Vec<Digest> = Vec::<Digest>::default();

    {
        let mut i: usize = 0;
        while i < orignal_vec_length {
            original_vec.push(tasm::tasmlib_io_read_stdin___digest());
            i += 1;
        }
    }

    let mid: usize = tasm::tasmlib_io_read_stdin___u32() as usize;

    let new_vec: Vec<Digest> = original_vec.split_off(mid);

    // Check original vec
    assert!(mid == original_vec.len());
    {
        let mut i: usize = 0;
        while i < mid {
            tasm::tasmlib_io_write_to_stdout___digest(original_vec[i]);
            i += 1;
        }
    }

    // Check new vec
    assert!(orignal_vec_length - mid == new_vec.len());
    {
        let mut i: usize = 0;
        while i < new_vec.len() {
            tasm::tasmlib_io_write_to_stdout___digest(new_vec[i]);
            i += 1;
        }
    }

    return;
}

#[cfg(test)]
mod test {
    use tasm_lib::twenty_first::math::other::random_elements;
    use tasm_lib::twenty_first::math::x_field_element::EXTENSION_DEGREE;
    use test_strategy::proptest;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows::wrap_main_with_io;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn bfe_vec_split_off_test() {
        let std_in: Vec<BFieldElement> = [
            vec![BFieldElement::new(12)],
            random_elements(12),
            vec![BFieldElement::new(8)],
        ]
        .concat();
        let native_output = wrap_main_with_io(&bfe_vec)(std_in.clone(), NonDeterminism::default());

        let entrypoint = EntrypointLocation::disk("vectors", "split_off", "bfe_vec");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn xfe_vec_split_off_test() {
        let std_in: Vec<BFieldElement> = [
            vec![BFieldElement::new(12)],
            random_elements(12 * EXTENSION_DEGREE),
            vec![BFieldElement::new(8)],
        ]
        .concat();
        let native_output = wrap_main_with_io(&xfe_vec)(std_in.clone(), NonDeterminism::default());

        let entrypoint = EntrypointLocation::disk("vectors", "split_off", "xfe_vec");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn digest_vec_split_off_test() {
        let std_in: Vec<BFieldElement> = [
            vec![BFieldElement::new(12)],
            random_elements(12 * Digest::LEN),
            vec![BFieldElement::new(8)],
        ]
        .concat();
        let native_output =
            wrap_main_with_io(&digest_vec)(std_in.clone(), NonDeterminism::default());

        let entrypoint = EntrypointLocation::disk("vectors", "split_off", "digest_vec");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[proptest(cases = 10)]
    fn xfe_vec_split_off_proptest(
        #[strategy(0usize..=400)] original_length: usize,
        #[strategy(0usize..#original_length)] mid: usize,
    ) {
        let std_in: Vec<BFieldElement> = [
            vec![BFieldElement::new(original_length as u64)],
            random_elements(original_length * EXTENSION_DEGREE),
            vec![BFieldElement::new(mid as u64)],
        ]
        .concat();

        let native_output = wrap_main_with_io(&xfe_vec)(std_in.clone(), NonDeterminism::default());

        let entrypoint = EntrypointLocation::disk("vectors", "split_off", "xfe_vec");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
