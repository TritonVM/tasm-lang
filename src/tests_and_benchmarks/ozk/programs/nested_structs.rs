// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use rand::prelude::Distribution;
use rand::random;
use rand::{distributions::Standard, Rng};
use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::{BFieldElement, Digest};
use twenty_first::shared_math::bfield_codec::BFieldCodec;
use twenty_first::shared_math::other::random_elements;

#[derive(TasmObject, BFieldCodec)]
struct InnerInnerStruct {
    pub a: Digest,
    pub b: Vec<BFieldElement>,
    pub c: u64,
    pub d: u128,
}

impl Distribution<InnerInnerStruct> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> InnerInnerStruct {
        let b_length = rng.gen_range(0..=10);
        InnerInnerStruct {
            a: random(),
            b: random_elements(b_length),
            c: random(),
            d: random(),
        }
    }
}

#[derive(TasmObject, BFieldCodec)]
struct InnerStruct {
    pub a: Digest,
    pub b: InnerInnerStruct,
    pub c: Vec<InnerInnerStruct>,
}

impl Distribution<InnerStruct> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> InnerStruct {
        let c_length = rng.gen_range(2..=14);
        InnerStruct {
            a: random(),
            b: random(),
            c: random_elements(c_length),
        }
    }
}

#[derive(TasmObject, BFieldCodec)]
struct TestStuctNested {
    pub a: InnerStruct,
    pub b: Vec<InnerStruct>,
    pub c: InnerInnerStruct,
    pub d: Vec<InnerInnerStruct>,
}

impl Distribution<TestStuctNested> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> TestStuctNested {
        let b_length = rng.gen_range(0..=10);
        let d_length = rng.gen_range(0..=10);
        TestStuctNested {
            a: random(),
            b: random_elements(b_length),
            c: random(),
            d: random_elements(d_length),
        }
    }
}

fn main() {
    let test_struct: Box<TestStuctNested> =
        TestStuctNested::decode(&tasm::load_from_memory(BFieldElement::new(300))).unwrap();

    tasm::tasm_io_write_to_stdout_digest(test_struct.a.a);
    tasm::tasm_io_write_to_stdout_u64(test_struct.a.b.c);
    tasm::tasm_io_write_to_stdout_u64(test_struct.a.c[1].c);

    return;
}

mod tests {
    use std::collections::HashMap;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::{
        execute_compiled_with_stack_memory_and_ins_for_test, init_memory_from,
    };

    use super::*;
    use itertools::Itertools;
    use rand::random;

    #[test]
    fn nested_structs_test() {
        let test_struct: TestStuctNested = random();
        let non_determinism = init_memory_from(&test_struct, BFieldElement::new(300));
        let input = vec![];

        let expected_output = vec![
            test_struct.a.a.encode(),
            test_struct.a.b.c.encode(),
            test_struct.a.c[1].c.encode(),
        ]
        .concat();

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(input.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test("nested_structs");
        println!("executing:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &mut HashMap::default(),
            input,
            non_determinism,
            0,
        )
        .unwrap();
        // assert_eq!(expected_output, vm_output.output);
        if expected_output != vm_output.output {
            panic!(
                "expected_output:\n {}, got:\n{}. Code was:\n{}",
                expected_output.iter().join(", "),
                vm_output.output.iter().join(", "),
                test_program.iter().join("\n")
            );
        }
    }
}
