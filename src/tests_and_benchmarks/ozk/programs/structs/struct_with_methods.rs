use num::Zero;
use tasm_lib::prelude::TasmObject;
use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

// Needs to be high enough to not interfere with the static memory allocator
const SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS: u64 = 200;

#[derive(TasmObject, BFieldCodec)]
struct TestStruct {
    a: Vec<BFieldElement>,
    b: u64,
    c: u64,
}

impl TestStruct {
    fn a_length(&self) -> u32 {
        return self.a.len() as u32;
    }

    fn bc_count_ones_and_add(&self, other_value: u32) -> u32 {
        return self.b.count_ones() + self.c.count_ones() + other_value;
    }

    fn lift_element_zero(&self) -> XFieldElement {
        return self.a[0].lift();
    }

    fn call_other_methods(&self, other_value: u32) -> u32 {
        return self.a_length() + self.bc_count_ones_and_add(other_value);
    }

    fn call_all_methods(&self, other_value: u32, repeat_count: u32) -> XFieldElement {
        let a: u32 = self.a_length();
        let b: u32 = self.bc_count_ones_and_add(other_value);
        let c: XFieldElement = self.lift_element_zero();
        let d: u32 = self.call_other_methods(other_value);
        let a_xfe: XFieldElement = BFieldElement::new(a as u64).lift();
        let b_xfe: XFieldElement = BFieldElement::new(b as u64).lift();
        let d_xfe: XFieldElement = BFieldElement::new(d as u64).lift();

        let e: XFieldElement = d_xfe
            + if repeat_count > 0u32 {
                self.call_all_methods(other_value + 1, repeat_count - 1)
            } else {
                XFieldElement::zero()
            };

        return e + a_xfe + b_xfe + c + d_xfe;
    }
}

fn main() {
    let test_struct: Box<TestStruct> =
        TestStruct::decode(&tasm::load_from_memory(BFieldElement::new(200))).unwrap();
    tasm::tasmlib_io_write_to_stdout___u32(test_struct.bc_count_ones_and_add(200));
    tasm::tasmlib_io_write_to_stdout___u32(test_struct.bc_count_ones_and_add(8043));
    tasm::tasmlib_io_write_to_stdout___u32(test_struct.a_length());
    tasm::tasmlib_io_write_to_stdout___xfe(test_struct.lift_element_zero());
    tasm::tasmlib_io_write_to_stdout___u32(test_struct.call_other_methods(4));
    tasm::tasmlib_io_write_to_stdout___xfe(test_struct.call_all_methods(200, 7));
    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use rand::distr::StandardUniform;
    use rand::prelude::Distribution;
    use rand::random;
    use rand::Rng;
    use tasm_lib::twenty_first::math::other::random_elements;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;

    impl Distribution<TestStruct> for StandardUniform {
        fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> TestStruct {
            let a_length = rng.random_range(2..20);
            TestStruct {
                a: random_elements(a_length),
                b: random(),
                c: random(),
            }
        }
    }

    #[test]
    fn struct_with_methods_test() {
        let test_struct: TestStruct = random();
        let non_determinism = init_memory_from(
            &test_struct,
            BFieldElement::new(SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS),
        );
        let stdin = vec![];

        let expected_output = [
            test_struct.bc_count_ones_and_add(200).encode(),
            test_struct.bc_count_ones_and_add(8043).encode(),
            test_struct.a_length().encode(),
            test_struct.lift_element_zero().encode(),
            test_struct.call_other_methods(4).encode(),
            test_struct.call_all_methods(200, 7).encode(),
        ]
        .concat();

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let entrypoint_location =
            EntrypointLocation::disk("structs", "struct_with_methods", "main");
        let test_program = ozk_parsing::compile_for_test(&entrypoint_location);
        println!("executing:\n{}", test_program.iter().join("\n"));
        let expected_stack_diff = 0;
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.public_output);
    }
}
