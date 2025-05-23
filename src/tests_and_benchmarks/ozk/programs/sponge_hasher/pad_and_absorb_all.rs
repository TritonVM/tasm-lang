use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::tests_and_benchmarks::ozk::rust_shadows::Tip5WithState;
use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
use crate::triton_vm::prelude::*;

fn pad_and_absorb_all() {
    Tip5WithState::init();
    let input_length: u32 = tasm::tasmlib_io_read_stdin___u32();
    let mut preimage: Vec<BFieldElement> = Vec::<BFieldElement>::default();
    {
        let mut i: usize = 0;
        while i < input_length as usize {
            preimage.push(tasm::tasmlib_io_read_stdin___bfe());
            i += 1;
        }
    }

    Tip5WithState::pad_and_absorb_all(&preimage);
    let produce: [BFieldElement; 10] = Tip5WithState::squeeze();
    {
        let mut i: usize = 0;
        while i < 10 {
            tasm::tasmlib_io_write_to_stdout___bfe(produce[i]);
            i += 1;
        }
    }

    return;
}

#[cfg(test)]
mod tests {
    use rand::Rng;
    use tasm_lib::triton_vm::prelude::NonDeterminism;
    use tasm_lib::twenty_first::math::other::random_elements;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows::wrap_main_with_io;

    #[test]
    fn pad_and_absorb_all_test() {
        let random_length: u32 = rand::rng().random_range(0..200);
        let std_in = [
            vec![BFieldElement::new(random_length as u64)],
            random_elements(random_length as usize),
        ]
        .concat();
        let native_output =
            wrap_main_with_io(&pad_and_absorb_all)(std_in.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("sponge_hasher", "pad_and_absorb_all", "pad_and_absorb_all");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
