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
    let ts_digest: Digest = Tip5::hash(&test_struct);

    tasm::tasmlib_io_write_to_stdout___digest(ts_digest);

    return;
}

#[derive(TasmObject, BFieldCodec)]
struct TwoDFields {
    a: Vec<BFieldElement>,
    b: Vec<Digest>,
}

fn two_dynamically_sized_fields() {
    let test_struct: Box<TwoDFields> =
        TwoDFields::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    let ts_digest: Digest = Tip5::hash(&test_struct);

    tasm::tasmlib_io_write_to_stdout___digest(ts_digest);

    return;
}

#[derive(TasmObject, BFieldCodec)]
struct TwoSFields {
    a: Digest,
    b: XFieldElement,
}

fn two_sfields() {
    let test_struct: Box<TwoSFields> =
        TwoSFields::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    let ts_digest: Digest = Tip5::hash(&test_struct);
    tasm::tasmlib_io_write_to_stdout___digest(ts_digest);
    return;
}

#[derive(TasmObject, BFieldCodec)]
struct FiveDandSFields {
    a: Vec<BFieldElement>,
    b: Digest,
    c: Vec<Digest>,
    d: BFieldElement,
    e: Vec<u128>,
}

fn five_fields() {
    let test_struct: Box<FiveDandSFields> =
        FiveDandSFields::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    let ts_digest: Digest = Tip5::hash(&test_struct);
    tasm::tasmlib_io_write_to_stdout___digest(ts_digest);
    return;
}

fn hash_boxed_atomic_values() {
    let bfe: BFieldElement = tasm::tasmlib_io_read_stdin___bfe();
    let boxed_bfe: Box<BFieldElement> = Box::<BFieldElement>::new(bfe);
    let hashed_bfe: Digest = Tip5::hash(&boxed_bfe);
    tasm::tasmlib_io_write_to_stdout___digest(hashed_bfe);
    return;
}

#[cfg(test)]
mod test {
    use rand::random;
    use tasm::wrap_main_with_io;
    use tasm_lib::triton_vm::prelude::*;
    use twenty_first::math::other::random_elements;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    use super::*;

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
            "hash_boxed_values",
            "one_dynamically_sized_field",
        );
        let vm_output = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(non_determinism)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn two_dynamically_sized_fields_test() {
        let test_struct = TwoDFields {
            a: random_elements(2),
            b: random_elements(2),
        };
        let non_determinism = init_memory_from(
            &test_struct,
            BFieldElement::new(SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS),
        );
        let native_output =
            wrap_main_with_io(&two_dynamically_sized_fields)(vec![], non_determinism.clone());

        let entrypoint = EntrypointLocation::disk(
            "algebraic_hasher",
            "hash_boxed_values",
            "two_dynamically_sized_fields",
        );
        let vm_output = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(non_determinism)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn two_statically_sized_fields_test() {
        let test_struct = TwoSFields {
            a: random(),
            b: random(),
        };
        let non_determinism = init_memory_from(
            &test_struct,
            BFieldElement::new(SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS),
        );
        let native_output = wrap_main_with_io(&two_sfields)(vec![], non_determinism.clone());

        let entrypoint =
            EntrypointLocation::disk("algebraic_hasher", "hash_boxed_values", "two_sfields");
        let vm_output = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(non_determinism)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn five_d_and_s_fields_test() {
        let test_struct = FiveDandSFields {
            a: random_elements(2),
            b: random(),
            c: random_elements(2),
            d: random(),
            e: random_elements(12),
        };
        let non_determinism = init_memory_from(
            &test_struct,
            BFieldElement::new(SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS),
        );
        let native_output = wrap_main_with_io(&five_fields)(vec![], non_determinism.clone());

        let entrypoint =
            EntrypointLocation::disk("algebraic_hasher", "hash_boxed_values", "five_fields");
        let vm_output = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(non_determinism)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }

    #[test]
    fn hash_boxed_atomic_values_test() {
        let std_in = random_elements(Digest::LEN * 3);
        let native_output =
            wrap_main_with_io(&hash_boxed_atomic_values)(std_in.clone(), NonDeterminism::default());

        let entrypoint = EntrypointLocation::disk(
            "algebraic_hasher",
            "hash_boxed_values",
            "hash_boxed_atomic_values",
        );
        let vm_output = TritonVMTestCase::new(entrypoint.clone())
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
