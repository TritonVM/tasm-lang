use tasm_lib::prelude::TasmObject;
use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

const SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS: u64 = 84;

#[derive(TasmObject, BFieldCodec)]
struct OneDField {
    a: Vec<BFieldElement>,
}

fn one_dynamically_sized_field() {
    let test_struct: Box<OneDField> =
        OneDField::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    let ts_digest: Digest = Tip5::hash(&test_struct.a);

    tasm::tasmlib_io_write_to_stdout___digest(ts_digest);

    return;
}

#[derive(TasmObject, BFieldCodec)]
struct OneSField {
    a: Digest,
}

fn one_statically_sized_field() {
    let test_struct: Box<OneSField> =
        OneSField::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    let ts_digest: Digest = Tip5::hash(&test_struct.a);
    tasm::tasmlib_io_write_to_stdout___digest(ts_digest);
    return;
}

#[cfg(test)]
mod test {
    use rand::random;
    use tasm::wrap_main_with_io;
    use tasm_lib::triton_vm::prelude::*;
    use twenty_first::math::other::random_elements;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    #[test]
    fn one_dynamically_sized_field_test() {
        let test_struct = OneDField {
            a: random_elements(2),
        };
        let non_determinism = init_memory_from(
            &test_struct,
            BFieldElement::new(SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS),
        );
        let native_output =
            wrap_main_with_io(&one_dynamically_sized_field)(vec![], non_determinism.clone());

        let entrypoint = EntrypointLocation::disk(
            "algebraic_hasher",
            "hash_boxed_fields",
            "one_dynamically_sized_field",
        );
        let vm_output = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(non_determinism)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn one_statically_sized_field_test() {
        let test_struct = OneSField { a: random() };
        let non_determinism = init_memory_from(
            &test_struct,
            BFieldElement::new(SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS),
        );
        let native_output =
            wrap_main_with_io(&one_statically_sized_field)(vec![], non_determinism.clone());

        let entrypoint = EntrypointLocation::disk(
            "algebraic_hasher",
            "hash_boxed_fields",
            "one_statically_sized_field",
        );
        let vm_output = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(non_determinism)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
