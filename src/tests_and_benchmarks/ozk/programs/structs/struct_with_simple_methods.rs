use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::twenty_first::shared_math::bfield_codec::BFieldCodec;
use triton_vm::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

const SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS: u64 = 2;

#[derive(TasmObject, BFieldCodec)]
struct TestStruct {
    a: BFieldElement,
    b: BFieldElement,
    c: u32,
    d: u64,
}

impl TestStruct {
    fn ab_sum(&self) -> BFieldElement {
        return self.a + self.b;
    }

    fn cd_sum(&self, other_value: u64) -> u128 {
        return self.c as u128 + self.d as u128 + other_value as u128;
    }
}

fn main() {
    let test_struct: Box<TestStruct> =
        TestStruct::decode(&tasm::load_from_memory(BFieldElement::new(2))).unwrap();
    let other_value: u64 = 2023;
    tasm::tasm_io_write_to_stdout___bfe(test_struct.ab_sum());
    tasm::tasm_io_write_to_stdout___u128(test_struct.cd_sum(other_value));
    return;
}

#[cfg(test)]
mod test {

    use rand::random;
    use triton_vm::BFieldElement;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    use super::*;

    #[test]
    fn basic_struct_method_test() {
        let ts = TestStruct {
            a: random(),
            b: random(),
            c: random(),
            d: random(),
        };
        let non_determinism = init_memory_from(
            &ts,
            BFieldElement::new(SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS),
        );
        let stdin = vec![];
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        let expected_output = [ts.ab_sum().encode(), ts.cd_sum(2023).encode()].concat();
        assert_eq!(native_output, expected_output);

        let entrypoint = EntrypointLocation::disk("structs", "struct_with_simple_methods", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_non_determinism(non_determinism)
            .expect_stack_difference(0)
            .execute()
            .unwrap();

        assert_eq!(expected_output, vm_output.output);
    }
}
