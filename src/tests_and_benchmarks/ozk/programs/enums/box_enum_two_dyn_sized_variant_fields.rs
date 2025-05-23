use tasm_lib::triton_vm::prelude::*;

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

#[allow(clippy::vec_init_then_push)]
fn main_a() {
    let mut bfes: Vec<BFieldElement> = Vec::<BFieldElement>::default();
    bfes.push(tasm::tasmlib_io_read_stdin___bfe());
    bfes.push(tasm::tasmlib_io_read_stdin___bfe());
    bfes.push(tasm::tasmlib_io_read_stdin___bfe());
    bfes.push(tasm::tasmlib_io_read_stdin___bfe());
    bfes.push(tasm::tasmlib_io_read_stdin___bfe());

    let mut xfes: Vec<XFieldElement> = Vec::<XFieldElement>::default();
    xfes.push(tasm::tasmlib_io_read_stdin___xfe());
    xfes.push(tasm::tasmlib_io_read_stdin___xfe());
    xfes.push(tasm::tasmlib_io_read_stdin___xfe());
    xfes.push(tasm::tasmlib_io_read_stdin___xfe());

    let a: EnumDynSizedVariantField = EnumDynSizedVariantField::A(
        tasm::tasmlib_io_read_stdin___bfe(),
        bfes,
        tasm::tasmlib_io_read_stdin___digest(),
        xfes,
    );
    let a_boxed: Box<EnumDynSizedVariantField> = Box::<EnumDynSizedVariantField>::new(a);
    match a_boxed.as_ref() {
        EnumDynSizedVariantField::A(bfe_again, bfes_again, digest_again, xfes_again) => {
            tasm::tasmlib_io_write_to_stdout___bfe(*bfe_again);
            tasm::tasmlib_io_write_to_stdout___bfe(bfes_again[4]);
            tasm::tasmlib_io_write_to_stdout___bfe(bfes_again[3]);
            tasm::tasmlib_io_write_to_stdout___bfe(bfes_again[2]);
            tasm::tasmlib_io_write_to_stdout___digest(*digest_again);
            tasm::tasmlib_io_write_to_stdout___xfe(xfes_again[2]);
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
        tasm::tasmlib_io_read_stdin___digest(),
        tasm::tasmlib_io_read_stdin___digest(),
    );
    let b_boxed: Box<EnumDynSizedVariantField> = Box::<EnumDynSizedVariantField>::new(b);
    match b_boxed.as_ref() {
        EnumDynSizedVariantField::A(_a, _b, _c, _d) => {
            panic!();
        }
        EnumDynSizedVariantField::B(d0, d1) => {
            tasm::tasmlib_io_write_to_stdout___digest(*d1);
            tasm::tasmlib_io_write_to_stdout___digest(*d0);
        }
        EnumDynSizedVariantField::C(_a, _b) => {
            panic!();
        }
    };

    return;
}

#[allow(clippy::vec_init_then_push)]
fn main_c() {
    let mut digests: Vec<Digest> = Vec::<Digest>::default();
    digests.push(tasm::tasmlib_io_read_stdin___digest());
    digests.push(tasm::tasmlib_io_read_stdin___digest());
    digests.push(tasm::tasmlib_io_read_stdin___digest());
    digests.push(tasm::tasmlib_io_read_stdin___digest());
    digests.push(tasm::tasmlib_io_read_stdin___digest());
    let c: EnumDynSizedVariantField = EnumDynSizedVariantField::C(
        [
            tasm::tasmlib_io_read_stdin___bfe(),
            tasm::tasmlib_io_read_stdin___bfe(),
            tasm::tasmlib_io_read_stdin___bfe(),
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
            tasm::tasmlib_io_write_to_stdout___bfe(bfe_array_3[1]);
            tasm::tasmlib_io_write_to_stdout___bfe(bfe_array_3[0]);
            tasm::tasmlib_io_write_to_stdout___bfe(bfe_array_3[2]);
            tasm::tasmlib_io_write_to_stdout___digest(digests[3]);
            tasm::tasmlib_io_write_to_stdout___digest(digests[4]);
            tasm::tasmlib_io_write_to_stdout___digest(digests[0]);
            tasm::tasmlib_io_write_to_stdout___digest(digests[1]);
            tasm::tasmlib_io_write_to_stdout___digest(digests[2]);
            tasm::tasmlib_io_write_to_stdout___digest(digests[4]);
            tasm::tasmlib_io_write_to_stdout___digest(digests[4]);
        }
    };

    return;
}

#[cfg(test)]
mod test {
    use tasm_lib::twenty_first::math::other::random_elements;

    use self::tasm::wrap_main_with_io;
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    #[test]
    fn a_box_enum_two_dyn_sized_variant_fields_test() {
        let std_in = random_elements(23);
        let native_output = wrap_main_with_io(&main_a)(std_in.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("enums", "box_enum_two_dyn_sized_variant_fields", "main_a");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn b_box_enum_two_dyn_sized_variant_fields_test() {
        let std_in = random_elements(10);
        let native_output = wrap_main_with_io(&main_b)(std_in.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("enums", "box_enum_two_dyn_sized_variant_fields", "main_b");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn c_box_enum_two_dyn_sized_variant_fields_test() {
        let std_in = random_elements(28);
        let native_output = wrap_main_with_io(&main_c)(std_in.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("enums", "box_enum_two_dyn_sized_variant_fields", "main_c");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
