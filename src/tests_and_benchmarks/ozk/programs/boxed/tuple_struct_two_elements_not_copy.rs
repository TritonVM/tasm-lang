use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Clone)]
struct TupleStruct2(u32, u64); // TODO: Use (u128, XFieldElement) here instead

fn main() {
    let a: u32 = tasm::tasm_io_read_stdin___u32();
    let b: u64 = tasm::tasm_io_read_stdin___u64();
    let ts: TupleStruct2 = TupleStruct2(a, b);
    let boxed_ts: Box<TupleStruct2> = Box::<TupleStruct2>::new(ts);

    assert!(a == boxed_ts.0);
    assert!(b == boxed_ts.1);
    let ts_again: TupleStruct2 = *boxed_ts;
    let b_again: u64 = ts_again.1;
    let a_again: u32 = ts_again.0;

    assert!(a == a_again);
    assert!(b == b_again);

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use rand::random;
    use tasm_lib::triton_vm::prelude::*;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn tuple_struct_two_elements_not_copy_test() {
        // Test function on host machine
        let a: u32 = random();
        let mut a_encoded_reverse = a.encode();
        a_encoded_reverse.reverse();
        let b: u64 = random();
        let mut b_encoded_reverse = b.encode();
        b_encoded_reverse.reverse();

        let stdin = [a_encoded_reverse, b_encoded_reverse].concat();
        let non_determinism = NonDeterminism::new(vec![]);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        // Test function in Triton VM
        let entrypoint_location =
            EntrypointLocation::disk("boxed", "tuple_struct_two_elements_not_copy", "main");
        let test_program = ozk_parsing::compile_for_test(&entrypoint_location);
        let expected_stack_diff = 0;
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            NonDeterminism::new(vec![]),
            expected_stack_diff,
        );
        match vm_output {
            Ok(vm_output) => {
                if native_output != vm_output.output {
                    panic!(
                        "expected:\n{}\n\ngot:\n{}\nCode was:\n{}\n",
                        native_output.iter().join(","),
                        vm_output.output.iter().join(","),
                        test_program.iter().join("\n"),
                    );
                }
            }
            Err(err) => panic!("{err}\n\nCode was:\n{}", test_program.iter().join("\n"),),
        }
    }
}
