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
    };

    let boxed: Box<WithListFields> = Box::<WithListFields>::new(a);
    tasm::tasm_io_write_to_stdout___digest(boxed.digests[2]);
    tasm::tasm_io_write_to_stdout___xfe(boxed.xfes[0]);
    tasm::tasm_io_write_to_stdout___xfe(boxed.xfes[1]);
    tasm::tasm_io_write_to_stdout___xfe(boxed.xfes[0]);
    tasm::tasm_io_write_to_stdout___bfe(boxed.bfes[0]);
    tasm::tasm_io_write_to_stdout___bfe(boxed.bfes[2]);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use triton_vm::NonDeterminism;
    use twenty_first::shared_math::other::random_elements;

    #[test]
    fn box_complex_flat_struct_test() {
        let non_determinism = NonDeterminism::default();
        let stdin = random_elements(51);

        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        let entrypoint_location =
            EntrypointLocation::disk("boxed", "box_complex_flat_struct", "main");
        let code =
            ozk_parsing::compile_for_test(&entrypoint_location, crate::ast_types::ListType::Unsafe);
        let vm_output =
            execute_compiled_with_stack_and_ins_for_test(&code, vec![], stdin, non_determinism, 0)
                .unwrap()
                .output;
        assert_eq!(native_output, vm_output);
    }
}
