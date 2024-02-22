use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Clone, Copy)]
struct TupleStruct1(u128);

fn main() {
    let a: u128 = tasm::tasm_io_read_stdin___u128();
    let b: u128 = tasm::tasm_io_read_stdin___u128();
    let a_copied: u128 = a;
    let ts_a: TupleStruct1 = TupleStruct1(a);
    let ts_b: TupleStruct1 = TupleStruct1(b);
    let boxed_ts_a: Box<TupleStruct1> = Box::<TupleStruct1>::new(ts_a);
    let boxed_ts_b: Box<TupleStruct1> = Box::<TupleStruct1>::new(ts_b);
    assert!(a_copied == a);
    assert!(ts_a.0 == a);

    let ts_again_a: TupleStruct1 = *boxed_ts_a;
    let ts_again_b: TupleStruct1 = *boxed_ts_b;
    let b_again: u128 = ts_again_b.0;
    let a_again: u128 = ts_again_a.0;

    assert!(a == a_again);
    tasm::tasm_io_write_to_stdout___u128(a);
    tasm::tasm_io_write_to_stdout___u128(a_again);
    assert!(b == b_again);

    tasm::tasm_io_write_to_stdout___u128(b);
    tasm::tasm_io_write_to_stdout___u128(b_again);

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
    fn tuple_struct_test() {
        // Test function on host machine
        let a: u128 = random();
        let b: u128 = random();
        let mut a_encoded_reverse = a.encode();
        a_encoded_reverse.reverse();
        let mut b_encoded_reverse = b.encode();
        b_encoded_reverse.reverse();

        let expected_output = [a.encode(), a.encode(), b.encode(), b.encode()].concat();
        let stdin = [a_encoded_reverse, b_encoded_reverse].concat();
        let non_determinism = NonDeterminism::new(vec![]);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let entrypoint_location =
            EntrypointLocation::disk("boxed", "tuple_struct_one_element", "main");
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
        if expected_output != vm_output.output {
            panic!(
                "expected:\n{}\n\ngot:\n{}",
                expected_output.iter().join(","),
                vm_output.output.iter().join(",")
            );
        }
    }
}
