use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    // Store two BFieldElements in memory. Then read them out again.
    let a: BFieldElement = tasm::tasmlib_io_read_stdin___bfe();
    let b: BFieldElement = BFieldElement::new((1u64 << 40) + 132);
    let boxed_a: Box<BFieldElement> = Box::<BFieldElement>::new(a);
    let boxed_b: Box<BFieldElement> = Box::<BFieldElement>::new(b);

    assert!(a == *boxed_a);
    assert!(b == *boxed_b);

    tasm::tasmlib_io_write_to_stdout___bfe(*boxed_b);
    tasm::tasmlib_io_write_to_stdout___bfe(*boxed_a);

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use rand::random;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn boxed_bfe_test() {
        // Test function on host machine
        let rand: BFieldElement = random();
        let stdin = vec![rand];
        let non_determinism = NonDeterminism::new(vec![]);
        // let expected_output = vec![BFieldElement::new((1u64 << 40) + 132), rand];
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        // assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let entrypoint_location = EntrypointLocation::disk("boxed", "bfe", "main");
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
        if native_output != vm_output.public_output {
            panic!(
                "expected:\n{}\n\ngot:\n{}",
                native_output.iter().join(","),
                vm_output.public_output.iter().join(",")
            );
        }
    }
}
