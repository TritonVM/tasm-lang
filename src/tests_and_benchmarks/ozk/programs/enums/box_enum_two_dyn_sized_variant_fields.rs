use crate::triton_vm::prelude::*;
use crate::twenty_first::shared_math::x_field_element::XFieldElement;
use tasm_lib::Digest;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

enum EnumDynSizedVariantField {
    A(
        BFieldElement,
        Vec<BFieldElement>,
        Digest,
        Vec<XFieldElement>,
    ),
    B(Digest, Digest),
    C([BFieldElement; 3], Vec<Digest>),
}

fn main_a() {
    let mut bfes: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(100);
    bfes.push(tasm::tasm_io_read_stdin___bfe());
    bfes.push(tasm::tasm_io_read_stdin___bfe());
    bfes.push(tasm::tasm_io_read_stdin___bfe());
    bfes.push(tasm::tasm_io_read_stdin___bfe());
    bfes.push(tasm::tasm_io_read_stdin___bfe());

    let mut xfes: Vec<XFieldElement> = Vec::<XFieldElement>::with_capacity(100);
    xfes.push(tasm::tasm_io_read_stdin___xfe());
    xfes.push(tasm::tasm_io_read_stdin___xfe());
    xfes.push(tasm::tasm_io_read_stdin___xfe());
    xfes.push(tasm::tasm_io_read_stdin___xfe());

    let a: EnumDynSizedVariantField = EnumDynSizedVariantField::A(
        tasm::tasm_io_read_stdin___bfe(),
        bfes,
        tasm::tasm_io_read_stdin___digest(),
        xfes,
    );
    let a_boxed: Box<EnumDynSizedVariantField> = Box::<EnumDynSizedVariantField>::new(a);
    match a_boxed.as_ref() {
        EnumDynSizedVariantField::A(bfe_again, bfes_again, digest_again, xfes_again) => {
            tasm::tasm_io_write_to_stdout___bfe(*bfe_again);
            tasm::tasm_io_write_to_stdout___bfe(bfes_again[4]);
            tasm::tasm_io_write_to_stdout___bfe(bfes_again[3]);
            tasm::tasm_io_write_to_stdout___bfe(bfes_again[2]);
            tasm::tasm_io_write_to_stdout___digest(*digest_again);
            tasm::tasm_io_write_to_stdout___xfe(xfes_again[2]);
        }
        EnumDynSizedVariantField::B(_d0, _d1) => {
            panic!();
        }
        EnumDynSizedVariantField::C(_bfe_a, _digests) => {
            panic!();
        }
    };

    return;
}

fn main_b() {
    let b: EnumDynSizedVariantField = EnumDynSizedVariantField::B(
        tasm::tasm_io_read_stdin___digest(),
        tasm::tasm_io_read_stdin___digest(),
    );
    let b_boxed: Box<EnumDynSizedVariantField> = Box::<EnumDynSizedVariantField>::new(b);
    match b_boxed.as_ref() {
        EnumDynSizedVariantField::A(_a, _b, _c, _d) => {
            panic!();
        }
        EnumDynSizedVariantField::B(d0, d1) => {
            tasm::tasm_io_write_to_stdout___digest(*d1);
            tasm::tasm_io_write_to_stdout___digest(*d0);
        }
        EnumDynSizedVariantField::C(_a, _b) => {
            panic!();
        }
    };

    return;
}

fn main_c() {
    let mut digests: Vec<Digest> = Vec::<Digest>::with_capacity(100);
    digests.push(tasm::tasm_io_read_stdin___digest());
    digests.push(tasm::tasm_io_read_stdin___digest());
    digests.push(tasm::tasm_io_read_stdin___digest());
    digests.push(tasm::tasm_io_read_stdin___digest());
    digests.push(tasm::tasm_io_read_stdin___digest());
    let c: EnumDynSizedVariantField = EnumDynSizedVariantField::C(
        [
            tasm::tasm_io_read_stdin___bfe(),
            tasm::tasm_io_read_stdin___bfe(),
            tasm::tasm_io_read_stdin___bfe(),
        ],
        digests,
    );
    let c_boxed: Box<EnumDynSizedVariantField> = Box::<EnumDynSizedVariantField>::new(c);
    match c_boxed.as_ref() {
        EnumDynSizedVariantField::A(_a, _b, _c, _d) => {
            panic!();
        }
        EnumDynSizedVariantField::B(_d0, _d1) => {
            panic!();
        }
        EnumDynSizedVariantField::C(bfe_array_3, digests) => {
            tasm::tasm_io_write_to_stdout___bfe(bfe_array_3[1]);
            tasm::tasm_io_write_to_stdout___bfe(bfe_array_3[0]);
            tasm::tasm_io_write_to_stdout___bfe(bfe_array_3[2]);
            tasm::tasm_io_write_to_stdout___digest(digests[3]);
            tasm::tasm_io_write_to_stdout___digest(digests[4]);
            tasm::tasm_io_write_to_stdout___digest(digests[0]);
            tasm::tasm_io_write_to_stdout___digest(digests[1]);
            tasm::tasm_io_write_to_stdout___digest(digests[2]);
            tasm::tasm_io_write_to_stdout___digest(digests[4]);
            tasm::tasm_io_write_to_stdout___digest(digests[4]);
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

    use crate::twenty_first::shared_math::other::random_elements;

    #[test]
    fn a_box_enum_two_dyn_sized_variant_fields_test() {
        let std_in = random_elements(23);
        let native_output = wrap_main_with_io(&main_a)(std_in.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("enums", "box_enum_two_dyn_sized_variant_fields", "main_a");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.output);
    }

    #[test]
    fn b_box_enum_two_dyn_sized_variant_fields_test() {
        let std_in = random_elements(10);
        let native_output = wrap_main_with_io(&main_b)(std_in.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("enums", "box_enum_two_dyn_sized_variant_fields", "main_b");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.output);
    }

    #[test]
    fn c_box_enum_two_dyn_sized_variant_fields_test() {
        let std_in = random_elements(28);
        let native_output = wrap_main_with_io(&main_c)(std_in.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("enums", "box_enum_two_dyn_sized_variant_fields", "main_c");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
