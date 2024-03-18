use num::Zero;
use tasm_lib::twenty_first::shared_math::other::random_elements;

use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::tests_and_benchmarks::ozk::rust_shadows::wrap_main_with_io;
use crate::tests_and_benchmarks::ozk::rust_shadows::Tip5WithState;
use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
use crate::triton_vm::prelude::*;

fn _main() {
    Tip5WithState::init();
    return;
}

fn initialized_sponge_behaves_correctly_on_small_stack() {
    let b: u64 = 100;
    Tip5WithState::init();
    let a: u64 = 400;
    tasm::tasm_io_write_to_stdout___u64(b);
    tasm::tasm_io_write_to_stdout___u64(a);
    return;
}

fn initialized_sponge_behaves_correctly_deep_in_stack() {
    let a: u128 = 100;
    let b: u128 = 200;
    Tip5WithState::init();
    let mut preimage: [BFieldElement; 10] = [BFieldElement::zero(); 10];
    {
        let mut i: usize = 0;
        while i < 10 {
            preimage[i] = tasm::tasm_io_read_stdin___bfe();
            i += 1;
        }
    }

    let _c: u128 = 300;
    let _d: u128 = 400;
    let _e: u128 = 500;
    let _f: u128 = 600;
    let _g: u128 = 700;
    let _h: u128 = 800;
    let _i: u128 = 900;
    let _j: u128 = 1000;
    tasm::tasm_io_write_to_stdout___u128(b);
    tasm::tasm_io_write_to_stdout___u128(a);
    Tip5WithState::absorb(preimage);

    return;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_compile_call_to_init() {
        let entrypoint = EntrypointLocation::disk("sponge_hasher", "init", "_main");
        TritonVMTestCase::new(entrypoint.clone()).compile();
    }

    #[test]
    fn initialized_sponge_behaves_correctly_on_small_stack_test() {
        let native_output = wrap_main_with_io(&initialized_sponge_behaves_correctly_on_small_stack)(
            Vec::default(),
            NonDeterminism::default(),
        );
        let entrypoint = EntrypointLocation::disk(
            "sponge_hasher",
            "init",
            "initialized_sponge_behaves_correctly_on_small_stack",
        );
        let vm_output = TritonVMTestCase::new(entrypoint).execute().unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn initialized_sponge_behaves_correctly_deep_in_stack_test() {
        let std_in = random_elements(10);
        let native_output = wrap_main_with_io(&initialized_sponge_behaves_correctly_deep_in_stack)(
            std_in.clone(),
            NonDeterminism::default(),
        );
        let entrypoint = EntrypointLocation::disk(
            "sponge_hasher",
            "init",
            "initialized_sponge_behaves_correctly_deep_in_stack",
        );
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
