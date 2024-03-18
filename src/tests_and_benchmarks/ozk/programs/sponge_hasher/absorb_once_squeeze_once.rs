use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::tests_and_benchmarks::ozk::rust_shadows::Tip5WithState;
use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
use crate::triton_vm::prelude::*;
use num::Zero;

fn absorb_and_squeeze() {
    Tip5WithState::init();
    let mut preimage: [BFieldElement; 10] = [BFieldElement::zero(); 10];
    {
        let mut i: usize = 0;
        while i < 10 {
            preimage[i] = tasm::tasm_io_read_stdin___bfe();
            i += 1;
        }
    }

    Tip5WithState::absorb(preimage);
    let squeezed: [BFieldElement; 10] = Tip5WithState::squeeze();
    {
        let mut i: usize = 0;
        while i < 10 {
            tasm::tasm_io_write_to_stdout___bfe(squeezed[i]);
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
    use itertools::Itertools;
    use tasm_lib::triton_vm::program::NonDeterminism;
    use tasm_lib::twenty_first::shared_math::other::random_elements;

    #[test]
    fn absorb_and_squeeze_test() {
        let std_in = random_elements(10);
        let native_output =
            wrap_main_with_io(&absorb_and_squeeze)(std_in.clone(), NonDeterminism::default());
        let entrypoint = EntrypointLocation::disk(
            "sponge_hasher",
            "absorb_once_squeeze_once",
            "absorb_and_squeeze",
        );
        let code = TritonVMTestCase::new(entrypoint.clone()).compile();
        println!("code:\n{}", code.iter().join("\n"));
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
