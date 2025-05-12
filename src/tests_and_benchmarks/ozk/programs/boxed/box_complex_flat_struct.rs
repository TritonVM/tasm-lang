use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::prelude::BFieldCodec;
use tasm_lib::twenty_first::prelude::XFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Debug, Clone, PartialEq, Eq, BFieldCodec)]
struct WithListFields {
    bfes: Vec<BFieldElement>,
    xfes: Vec<XFieldElement>,
    bfe: BFieldElement,
    xfe: XFieldElement,
    pair: (Digest, BFieldElement),
    digests: Vec<Digest>,
    bfe_array: [BFieldElement; 6],
}

#[allow(clippy::redundant_field_names)]
#[allow(clippy::vec_init_then_push)]
fn main() {
    let mut bfes: Vec<BFieldElement> = Vec::<BFieldElement>::default();
    bfes.push(BFieldElement::new(4000));
    bfes.push(BFieldElement::new(4001));
    bfes.push(BFieldElement::new(4002));
    let mut xfes: Vec<XFieldElement> = Vec::<XFieldElement>::default();
    xfes.push(tasm::tasmlib_io_read_stdin___xfe());
    xfes.push(tasm::tasmlib_io_read_stdin___xfe());
    xfes.push(tasm::tasmlib_io_read_stdin___xfe());
    xfes.push(tasm::tasmlib_io_read_stdin___xfe());
    let mut digests: Vec<Digest> = Vec::<Digest>::default();
    digests.push(tasm::tasmlib_io_read_stdin___digest());
    digests.push(tasm::tasmlib_io_read_stdin___digest());
    digests.push(tasm::tasmlib_io_read_stdin___digest());
    digests.push(tasm::tasmlib_io_read_stdin___digest());
    digests.push(tasm::tasmlib_io_read_stdin___digest());

    let a: WithListFields = WithListFields {
        bfes: bfes,
        xfes: xfes,
        bfe: BFieldElement::new(132),
        xfe: tasm::tasmlib_io_read_stdin___xfe(),
        pair: (
            tasm::tasmlib_io_read_stdin___digest(),
            tasm::tasmlib_io_read_stdin___bfe(),
        ),
        digests: digests,
        bfe_array: [
            tasm::tasmlib_io_read_stdin___bfe(),
            tasm::tasmlib_io_read_stdin___bfe(),
            tasm::tasmlib_io_read_stdin___bfe(),
            tasm::tasmlib_io_read_stdin___bfe(),
            tasm::tasmlib_io_read_stdin___bfe(),
            tasm::tasmlib_io_read_stdin___bfe(),
        ],
    };

    let boxed: Box<WithListFields> = Box::<WithListFields>::new(a);
    tasm::tasmlib_io_write_to_stdout___digest(boxed.digests[2]);
    tasm::tasmlib_io_write_to_stdout___xfe(boxed.xfes[0]);
    tasm::tasmlib_io_write_to_stdout___xfe(boxed.xfes[1]);
    tasm::tasmlib_io_write_to_stdout___xfe(boxed.xfes[0]);
    tasm::tasmlib_io_write_to_stdout___bfe(boxed.bfes[0]);
    tasm::tasmlib_io_write_to_stdout___bfe(boxed.bfes[2]);
    tasm::tasmlib_io_write_to_stdout___bfe(boxed.bfe_array[1]);
    tasm::tasmlib_io_write_to_stdout___bfe(boxed.bfe_array[4]);

    return;
}

#[cfg(test)]
mod test {
    use tasm_lib::twenty_first::math::other::random_elements;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    #[test]
    fn box_complex_flat_struct_test() {
        let non_determinism = NonDeterminism::default();
        let stdin = random_elements(52);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let entrypoint = EntrypointLocation::disk("boxed", "box_complex_flat_struct", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(stdin)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
