use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use tasm_lib::Digest;
use triton_vm::BFieldElement;
use twenty_first::shared_math::{bfield_codec::BFieldCodec, x_field_element::XFieldElement};

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
fn main() {
    let mut bfes: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(32);
    bfes.push(BFieldElement::new(4000));
    bfes.push(BFieldElement::new(4001));
    bfes.push(BFieldElement::new(4002));
    let mut xfes: Vec<XFieldElement> = Vec::<XFieldElement>::with_capacity(32);
    xfes.push(tasm::tasm_io_read_stdin___xfe());
    xfes.push(tasm::tasm_io_read_stdin___xfe());
    xfes.push(tasm::tasm_io_read_stdin___xfe());
    xfes.push(tasm::tasm_io_read_stdin___xfe());
    let mut digests: Vec<Digest> = Vec::<Digest>::with_capacity(32);
    digests.push(tasm::tasm_io_read_stdin___digest());
    digests.push(tasm::tasm_io_read_stdin___digest());
    digests.push(tasm::tasm_io_read_stdin___digest());
    digests.push(tasm::tasm_io_read_stdin___digest());
    digests.push(tasm::tasm_io_read_stdin___digest());

    let a: WithListFields = WithListFields {
        bfes: bfes,
        xfes: xfes,
        bfe: BFieldElement::new(132),
        xfe: tasm::tasm_io_read_stdin___xfe(),
        pair: (
            tasm::tasm_io_read_stdin___digest(),
            tasm::tasm_io_read_stdin___bfe(),
        ),
        digests: digests,
        bfe_array: [
            tasm::tasm_io_read_stdin___bfe(),
            tasm::tasm_io_read_stdin___bfe(),
            tasm::tasm_io_read_stdin___bfe(),
            tasm::tasm_io_read_stdin___bfe(),
            tasm::tasm_io_read_stdin___bfe(),
            tasm::tasm_io_read_stdin___bfe(),
        ],
    };

    let boxed: Box<WithListFields> = Box::<WithListFields>::new(a);
    tasm::tasm_io_write_to_stdout___digest(boxed.digests[2]);
    tasm::tasm_io_write_to_stdout___xfe(boxed.xfes[0]);
    tasm::tasm_io_write_to_stdout___xfe(boxed.xfes[1]);
    tasm::tasm_io_write_to_stdout___xfe(boxed.xfes[0]);
    tasm::tasm_io_write_to_stdout___bfe(boxed.bfes[0]);
    tasm::tasm_io_write_to_stdout___bfe(boxed.bfes[2]);
    tasm::tasm_io_write_to_stdout___bfe(boxed.bfe_array[1]);
    tasm::tasm_io_write_to_stdout___bfe(boxed.bfe_array[4]);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
    use triton_vm::NonDeterminism;
    use twenty_first::shared_math::other::random_elements;

    #[test]
    fn box_complex_flat_struct_test() {
        let non_determinism = NonDeterminism::default();
        let stdin = random_elements(52);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let entrypoint = EntrypointLocation::disk("boxed", "box_complex_flat_struct", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(stdin)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
