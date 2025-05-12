use num::One;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn data_from_std_in() {
    let base: XFieldElement = tasm::tasmlib_io_read_stdin___xfe();
    let exponent: u32 = tasm::tasmlib_io_read_stdin___u32();
    tasm::tasmlib_io_write_to_stdout___xfe(base.mod_pow_u32(exponent));

    return;
}

fn _to_the_power_of_zero() {
    let base: XFieldElement = tasm::tasmlib_io_read_stdin___xfe();
    let res: XFieldElement = base.mod_pow_u32(0);
    assert!(res == XFieldElement::one());

    return;
}

fn _to_the_power_of_one() {
    let base: XFieldElement = tasm::tasmlib_io_read_stdin___xfe();
    let res: XFieldElement = base.mod_pow_u32(1);
    assert!(res == base);

    return;
}

fn verify_no_name_clash_xfe_bfe_mod_pow_u32() {
    let bfe: BFieldElement = BFieldElement::new(42);
    let xfe: XFieldElement = bfe.lift() + XFieldElement::one();

    tasm::tasmlib_io_write_to_stdout___bfe(bfe.mod_pow_u32(121));
    tasm::tasmlib_io_write_to_stdout___xfe(xfe.mod_pow_u32(121));

    return;
}

#[cfg(test)]
mod test {
    use rand::random;
    use tasm_lib::twenty_first::math::other::random_elements;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows::wrap_main_with_io;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn xfe_to_the_power_of_zero_test() {
        let entrypoint =
            EntrypointLocation::disk("arithmetic", "xfe_mod_pow_u32", "_to_the_power_of_zero");
        TritonVMTestCase::new(entrypoint)
            .with_std_in(random_elements(3))
            .execute()
            .unwrap();
    }

    #[test]
    fn xfe_to_the_power_of_one_test() {
        let entrypoint =
            EntrypointLocation::disk("arithmetic", "xfe_mod_pow_u32", "_to_the_power_of_one");
        TritonVMTestCase::new(entrypoint)
            .with_std_in(random_elements(3))
            .execute()
            .unwrap();
    }

    #[test]
    fn xfe_mod_pow_u32_random_input() {
        let random_base_element = random_elements(3);
        let random_exponent: u32 = random();
        let std_in = [
            random_base_element,
            vec![BFieldElement::new(random_exponent as u64)],
        ]
        .concat();
        let native_output =
            wrap_main_with_io(&data_from_std_in)(std_in.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("arithmetic", "xfe_mod_pow_u32", "data_from_std_in");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn verify_no_name_clash_xfe_bfe_mod_pow_u32_test() {
        let native_output = wrap_main_with_io(&verify_no_name_clash_xfe_bfe_mod_pow_u32)(
            vec![],
            NonDeterminism::default(),
        );
        let entrypoint = EntrypointLocation::disk(
            "arithmetic",
            "xfe_mod_pow_u32",
            "verify_no_name_clash_xfe_bfe_mod_pow_u32",
        );
        let vm_output = TritonVMTestCase::new(entrypoint).execute().unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
