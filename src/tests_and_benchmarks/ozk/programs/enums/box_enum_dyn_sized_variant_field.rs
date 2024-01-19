use triton_vm::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

enum EnumDynSizedVariantField {
    A(Vec<BFieldElement>),
}

fn main() {
    let mut bfes: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(64);
    bfes.push(tasm::tasm_io_read_stdin___bfe());
    bfes.push(tasm::tasm_io_read_stdin___bfe());
    bfes.push(tasm::tasm_io_read_stdin___bfe());
    let a: EnumDynSizedVariantField = EnumDynSizedVariantField::A(bfes);

    let boxed: Box<EnumDynSizedVariantField> = Box::<EnumDynSizedVariantField>::new(a);

    match boxed.as_ref() {
        EnumDynSizedVariantField::A(bfes_again) => {
            tasm::tasm_io_write_to_stdout___bfe(bfes_again[2]);
            tasm::tasm_io_write_to_stdout___bfe(bfes_again[0]);
            tasm::tasm_io_write_to_stdout___bfe(bfes_again[1]);
            tasm::tasm_io_write_to_stdout___bfe(bfes_again[1]);
        }
    };

    return;
}

#[cfg(test)]
mod test {
    use self::tasm::wrap_main_with_io;
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
    use itertools::Itertools;
    use rand::random;
    use triton_vm::NonDeterminism;

    #[test]
    fn box_enum_dyn_sized_variant_field_test() {
        let std_in = vec![random(), random(), random()];
        let native_output = wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("enums", "box_enum_dyn_sized_variant_field", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
