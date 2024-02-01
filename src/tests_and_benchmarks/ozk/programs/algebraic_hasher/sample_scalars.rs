use tasm_lib::twenty_first::shared_math::tip5::Tip5State;
use tasm_lib::twenty_first::util_types::algebraic_hasher::{AlgebraicHasher, SpongeHasher};

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
use crate::triton_vm::prelude::*;

fn sample_scalars() {
    let mut sponge: Tip5State = Tip5::init();
    let input_count: usize = tasm::tasm_io_read_stdin___u32() as usize;
    let mut preimage: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(input_count + 20);
    {
        let mut i: usize = 0;
        while i < input_count {
            preimage.push(tasm::tasm_io_read_stdin___bfe());
            i += 1;
        }
    }
    assert!(input_count == preimage.len());
    Tip5::pad_and_absorb_all(&mut sponge, &preimage);

    let scalar_count: usize = tasm::tasm_io_read_stdin___u32() as usize;
    let scalars: Vec<XFieldElement> = Tip5::sample_scalars(&mut sponge, scalar_count);

    {
        let mut i: usize = 0;
        while i < scalar_count {
            tasm::tasm_io_write_to_stdout___xfe(scalars[i]);
            i += 1;
        }
    }

    return;
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use rand::thread_rng;
    use rand::Rng;
    use tasm_lib::triton_vm::program::NonDeterminism;
    use tasm_lib::twenty_first::shared_math::other::random_elements;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows::wrap_main_with_io;

    use super::*;

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
            if native_output != vm_output.output {
                panic!(
                    "native_output:\n{}\n\n VM output:\n{}",
                    native_output.iter().join("\n"),
                    vm_output.output.iter().join("\n")
                );
            }
            assert_eq!(native_output, vm_output.output);
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
        assert_eq!(native_output, vm_output.output);
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
        assert_eq!(native_output, vm_output.output);
    }

    #[test]
    fn sample_scalars_test() {
        let preimage_length: u32 = thread_rng().gen_range(0..200);
        let sample_count: u32 = thread_rng().gen_range(0..200);
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
        assert_eq!(native_output, vm_output.output);
    }
}
