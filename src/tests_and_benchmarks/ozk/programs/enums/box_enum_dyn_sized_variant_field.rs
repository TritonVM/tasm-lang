use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

enum EnumDynSizedVariantField {
    A(Vec<BFieldElement>),
}

#[allow(clippy::vec_init_then_push)]
fn main() {
    let mut bfes: Vec<BFieldElement> = Vec::<BFieldElement>::default();
    bfes.push(tasm::tasmlib_io_read_stdin___bfe());
    bfes.push(tasm::tasmlib_io_read_stdin___bfe());
    bfes.push(tasm::tasmlib_io_read_stdin___bfe());
    let a: EnumDynSizedVariantField = EnumDynSizedVariantField::A(bfes);

    let boxed: Box<EnumDynSizedVariantField> = Box::<EnumDynSizedVariantField>::new(a);

    match boxed.as_ref() {
        EnumDynSizedVariantField::A(bfes_again) => {
            tasm::tasmlib_io_write_to_stdout___bfe(bfes_again[2]);
            tasm::tasmlib_io_write_to_stdout___bfe(bfes_again[0]);
            tasm::tasmlib_io_write_to_stdout___bfe(bfes_again[1]);
            tasm::tasmlib_io_write_to_stdout___bfe(bfes_again[1]);
        }
    };

    return;
}

#[cfg(test)]
mod test {
    use rand::random;

    use self::tasm::wrap_main_with_io;
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    #[test]
    fn box_enum_dyn_sized_variant_field_test() {
        let std_in = vec![random(), random(), random()];
        let native_output = wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("enums", "box_enum_dyn_sized_variant_field", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
