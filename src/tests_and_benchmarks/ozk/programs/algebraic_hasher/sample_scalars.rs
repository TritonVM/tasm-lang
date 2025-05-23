use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::tests_and_benchmarks::ozk::rust_shadows::Tip5WithState;
use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
use crate::triton_vm::prelude::*;

fn sample_scalars() {
    Tip5WithState::init();
    let input_count: usize = tasm::tasmlib_io_read_stdin___u32() as usize;
    let mut preimage: Vec<BFieldElement> = Vec::<BFieldElement>::default();
    {
        let mut i: usize = 0;
        while i < input_count {
            preimage.push(tasm::tasmlib_io_read_stdin___bfe());
            i += 1;
        }
    }
    assert!(input_count == preimage.len());
    Tip5WithState::pad_and_absorb_all(&preimage);

    let scalar_count: usize = tasm::tasmlib_io_read_stdin___u32() as usize;
    let scalars: Vec<XFieldElement> = Tip5WithState::sample_scalars(scalar_count);

    {
        let mut i: usize = 0;
        while i < scalar_count {
            tasm::tasmlib_io_write_to_stdout___xfe(scalars[i]);
            i += 1;
        }
    }

    return;
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use rand::Rng;
    use tasm_lib::triton_vm::prelude::NonDeterminism;
    use tasm_lib::twenty_first::math::other::random_elements;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows::wrap_main_with_io;

    #[test]
    fn preimage_length_one_test_loop_over_sample_count_test() {
        let entrypoint =
            EntrypointLocation::disk("algebraic_hasher", "sample_scalars", "sample_scalars");
        let preimage_length: u32 = 1;
        for sample_count in 0..30 {
            let std_in = vec![
                BFieldElement::new(preimage_length as u64),
                BFieldElement::new(424242),
                BFieldElement::new(sample_count as u64),
            ];
            let native_output =
                wrap_main_with_io(&sample_scalars)(std_in.clone(), NonDeterminism::default());

            let vm_output = TritonVMTestCase::new(entrypoint.clone())
                .with_std_in(std_in)
                .execute()
                .unwrap();
            if native_output != vm_output.public_output {
                panic!(
                    "native_output:\n{}\n\n VM output:\n{}",
                    native_output.iter().join("\n"),
                    vm_output.public_output.iter().join("\n")
                );
            }
            assert_eq!(native_output, vm_output.public_output);
        }
    }

    #[test]
    fn preimage_length_one_test() {
        let preimage_length: u32 = 1;
        let sample_count: u32 = 1;
        let std_in = vec![
            BFieldElement::new(preimage_length as u64),
            BFieldElement::new(424242),
            BFieldElement::new(sample_count as u64),
        ];
        let native_output =
            wrap_main_with_io(&sample_scalars)(std_in.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("algebraic_hasher", "sample_scalars", "sample_scalars");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn empty_preimage_test() {
        let preimage_length: u32 = 0;
        let sample_count: u32 = 1;
        let std_in = vec![
            BFieldElement::new(preimage_length as u64),
            BFieldElement::new(sample_count as u64),
        ];
        let native_output =
            wrap_main_with_io(&sample_scalars)(std_in.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("algebraic_hasher", "sample_scalars", "sample_scalars");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn sample_scalars_test() {
        let preimage_length: u32 = rand::rng().random_range(0..200);
        let sample_count: u32 = rand::rng().random_range(0..200);
        let std_in = [
            vec![BFieldElement::new(preimage_length as u64)],
            random_elements(preimage_length as usize),
            vec![BFieldElement::new(sample_count as u64)],
        ]
        .concat();
        let native_output =
            wrap_main_with_io(&sample_scalars)(std_in.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("algebraic_hasher", "sample_scalars", "sample_scalars");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
