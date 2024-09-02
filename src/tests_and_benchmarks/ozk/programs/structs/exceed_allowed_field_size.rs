use arbitrary::Arbitrary;
use tasm_lib::prelude::TasmObject;
use tasm_lib::triton_vm::prelude::*;
use twenty_first::prelude::AlgebraicHasher;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(BFieldCodec, PartialEq, Eq, Clone, Debug, Arbitrary)]
enum MyEnum {
    A(u64, Digest),
    B,
    C,
}

#[derive(BFieldCodec, TasmObject, PartialEq, Eq, Clone, Debug, Arbitrary)]
struct TestStruct {
    a: Vec<XFieldElement>,
    b: MyEnum,
    c: u32,
    d: Vec<Digest>,
    e: Digest,
    f: Vec<BFieldElement>,
    g: Digest,
}

fn field_getter() {
    let test_struct: Box<TestStruct> =
        TestStruct::decode(&tasm::load_from_memory(BFieldElement::new(300))).unwrap();

    let d: &Vec<Digest> = &test_struct.d;

    tasm::tasmlib_io_write_to_stdout___u32(d.len() as u32);

    return;
}

fn hash_boxed_struct() {
    let test_struct: Box<TestStruct> =
        TestStruct::decode(&tasm::load_from_memory(BFieldElement::new(300))).unwrap();
    let ts_digest: Digest = Tip5::hash(&test_struct);
    tasm::tasmlib_io_write_to_stdout___digest(ts_digest);

    return;
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use arbitrary::Unstructured;
    use rand::random;
    use rand::rngs::StdRng;
    use rand::RngCore;
    use rand::SeedableRng;
    use tasm::wrap_main_with_io;
    use tasm_lib::memory::encode_to_memory;

    use crate::ast_types::DataType;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    use super::*;

    const OBJ_POINTER: BFieldElement = BFieldElement::new(300);

    /// Return non-determinism and object used in memory-initialization
    fn nd_mem_with_random_obj(seed: [u8; 32]) -> (NonDeterminism, TestStruct) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let mut randomness = [0u8; 1_000_000];
        rng.fill_bytes(&mut randomness);
        let mut unstructured = Unstructured::new(&randomness);
        let ts = TestStruct::arbitrary(&mut unstructured).unwrap();

        let mut beningn_memory = HashMap::default();
        encode_to_memory(&mut beningn_memory, OBJ_POINTER, &ts);
        (NonDeterminism::default().with_ram(beningn_memory), ts)
    }

    #[test]
    fn exceed_allowed_total_length_test() {
        // Positive test
        let (benign_nd, ts) = nd_mem_with_random_obj(random());
        let expected_output = Tip5::hash(&ts).values().to_vec();
        let stdin = vec![];
        let native_output = wrap_main_with_io(&hash_boxed_struct)(stdin.clone(), benign_nd.clone());
        assert_eq!(expected_output, native_output);

        let entrypoint =
            EntrypointLocation::disk("structs", "exceed_allowed_field_size", "hash_boxed_struct");
        let vm_output = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(benign_nd.clone())
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);

        // Negative test: size indicator exceeds max allowed size, but is valid u32.
        const OFFSET_FOR_OF_MALICIOUS_SI: BFieldElement = BFieldElement::new(5);
        let mut malicious_nd_ram_0 = benign_nd.clone();
        malicious_nd_ram_0.ram.insert(
            OBJ_POINTER + OFFSET_FOR_OF_MALICIOUS_SI,
            bfe!(DataType::MAX_DYN_FIELD_SIZE + 1),
        );
        let err0 = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(malicious_nd_ram_0)
            .execute()
            .unwrap_err();
        let err0 = err0.downcast::<InstructionError>().unwrap();
        assert_eq!(InstructionError::AssertionFailed, err0);

        // Negative test: size indicator is negative
        let mut malicious_nd_ram_1 = benign_nd.clone();
        let negative_number = bfe!(-1);
        malicious_nd_ram_1
            .ram
            .insert(OBJ_POINTER + OFFSET_FOR_OF_MALICIOUS_SI, negative_number);
        let err1 = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(malicious_nd_ram_1)
            .execute()
            .unwrap_err();
        let err1 = err1.downcast::<InstructionError>().unwrap();
        assert_eq!(InstructionError::FailedU32Conversion(negative_number), err1);
    }

    #[test]
    fn exceed_allowed_size_indicator_test() {
        // Positive test
        let (benign_nd, ts) = nd_mem_with_random_obj(random());
        let expected_output = vec![bfe!(ts.d.len() as u64)];
        let stdin = vec![];
        let native_output = wrap_main_with_io(&field_getter)(stdin.clone(), benign_nd.clone());
        assert_eq!(expected_output, native_output);

        let entrypoint =
            EntrypointLocation::disk("structs", "exceed_allowed_field_size", "field_getter");
        let vm_output = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(benign_nd.clone())
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);

        // Negative test: size indicator exceeds max allowed size, but is valid u32.
        const OFFSET_FOR_OF_MALICIOUS_SI: BFieldElement = BFieldElement::new(5);
        let mut malicious_nd_ram_0 = benign_nd.clone();
        malicious_nd_ram_0.ram.insert(
            OBJ_POINTER + OFFSET_FOR_OF_MALICIOUS_SI,
            bfe!(DataType::MAX_DYN_FIELD_SIZE + 1),
        );
        let err0 = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(malicious_nd_ram_0)
            .execute()
            .unwrap_err();
        let err0 = err0.downcast::<InstructionError>().unwrap();
        assert_eq!(InstructionError::AssertionFailed, err0);

        // Negative test: size indicator is negative
        let mut malicious_nd_ram_1 = benign_nd.clone();
        let negative_number = bfe!(-1);
        malicious_nd_ram_1
            .ram
            .insert(OBJ_POINTER + OFFSET_FOR_OF_MALICIOUS_SI, negative_number);
        let err1 = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(malicious_nd_ram_1)
            .execute()
            .unwrap_err();
        let err1 = err1.downcast::<InstructionError>().unwrap();
        assert_eq!(InstructionError::FailedU32Conversion(negative_number), err1);
    }
}
