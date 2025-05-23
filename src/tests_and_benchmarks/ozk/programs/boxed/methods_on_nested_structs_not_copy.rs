use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

struct OuterStruct(u64, MiddleStruct, InnerStruct);

impl OuterStruct {
    fn add(&self, extra_arg: u64) -> u128 {
        return self.0 as u128 + self.1 .2 as u128 + extra_arg as u128;
    }
}

struct MiddleStruct(Digest, InnerStruct, u32);

impl MiddleStruct {
    fn add(&self) -> u128 {
        return self.1 .0 as u128 + self.2 as u128 + 4;
    }
}

struct InnerStruct(u64, u32);

impl InnerStruct {
    fn add(&self, extra_arg: u128) -> u128 {
        let mid: u128 = self.0 as u128 + 14;
        return mid + self.1 as u128 - extra_arg;
    }
}

// Three-level deep nested tuple-struct.
// Methods on all levels.
// Verify that all methods work.
fn main() {
    let inner_a: InnerStruct = InnerStruct(1u64 << 41, 1u32 << 21);
    let inner_b: InnerStruct = InnerStruct(1u64 << 42, 1u32 << 22);
    let middle_struct: MiddleStruct = MiddleStruct(
        Digest::new([
            BFieldElement::new(2u64),
            BFieldElement::new(4u64),
            BFieldElement::new(8u64),
            BFieldElement::new(16u64),
            BFieldElement::new(32u64),
        ]),
        inner_b,
        1 << 31,
    );
    let outer_struct: OuterStruct = OuterStruct(44, middle_struct, inner_a);
    assert!(1u64 << 41 == outer_struct.2 .0);
    assert!(1u32 << 21 == outer_struct.2 .1);
    assert!(1u32 << 31 == outer_struct.1 .2);

    let outer_struct_boxed: Box<OuterStruct> = Box::<OuterStruct>::new(outer_struct);
    assert!(1u64 << 41 == outer_struct_boxed.2 .0);
    assert!(1u32 << 21 == outer_struct_boxed.2 .1);
    assert!(1u32 << 31 == outer_struct_boxed.1 .2);

    tasm::tasmlib_io_write_to_stdout___u128(outer_struct_boxed.add(200));
    tasm::tasmlib_io_write_to_stdout___u128(outer_struct_boxed.1.add());
    tasm::tasmlib_io_write_to_stdout___u128(outer_struct_boxed.2.add(300));

    tasm::tasmlib_io_write_to_stdout___digest(outer_struct_boxed.1 .0);

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use tasm_lib::twenty_first::prelude::BFieldCodec;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn methods_on_nested_structs_test() {
        // Test function on host machine
        let stdin = vec![];
        let non_determinism = NonDeterminism::new(vec![]);
        let expected_output = [
            (44u128 + (1u128 << 31) + 200).encode(),
            ((1u128 << 42) + (1 << 31) + 4).encode(),
            ((1u128 << 41) + 14 + (1 << 21) - 300).encode(),
            vec![
                BFieldElement::new(2u64),
                BFieldElement::new(4u64),
                BFieldElement::new(8u64),
                BFieldElement::new(16u64),
                BFieldElement::new(32u64),
            ],
        ]
        .concat();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let entrypoint_location =
            EntrypointLocation::disk("boxed", "methods_on_nested_structs_not_copy", "main");
        let test_program = ozk_parsing::compile_for_test(&entrypoint_location);
        let expected_stack_diff = 0;
        println!("test_program:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            NonDeterminism::new(vec![]),
            expected_stack_diff,
        )
        .unwrap();
        if expected_output != vm_output.public_output {
            panic!(
                "expected:\n{}\n\ngot:\n{}",
                expected_output.iter().join(","),
                vm_output.public_output.iter().join(",")
            );
        }
    }
}
