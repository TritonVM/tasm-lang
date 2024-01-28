use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
use crate::triton_vm::prelude::*;
use tasm_lib::twenty_first::shared_math::tip5::Tip5State;
use tasm_lib::twenty_first::util_types::algebraic_hasher::SpongeHasher;

fn pad_and_absorb_all() {
    let mut sponge: Tip5State = Tip5::init();
    let input_length: u32 = tasm::tasm_io_read_stdin___u32();
    let mut preimage: Vec<BFieldElement> =
        Vec::<BFieldElement>::with_capacity(input_length as usize);
    {
        let mut i: usize = 0;
        while i < input_length as usize {
            preimage.push(tasm::tasm_io_read_stdin___bfe());
            i += 1;
        }
    }

    Tip5::pad_and_absorb_all(&mut sponge, &preimage);
    let produce: [BFieldElement; 10] = Tip5::squeeze_once(&mut sponge);
    {
        let mut i: usize = 0;
        while i < 10 {
            tasm::tasm_io_write_to_stdout___bfe(produce[i]);
            i += 1;
        }
    }

    return;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows::wrap_main_with_io;
    use rand::{thread_rng, Rng};
    use tasm_lib::triton_vm::program::NonDeterminism;
    use tasm_lib::twenty_first::shared_math::other::random_elements;

    #[test]
    fn pad_and_absorb_all_test() {
        let random_length: u32 = thread_rng().gen_range(0..200);
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
            .expect_stack_difference(0)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
