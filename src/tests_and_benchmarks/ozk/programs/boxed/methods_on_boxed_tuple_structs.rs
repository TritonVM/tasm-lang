use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

struct TupleStructA(u128, u64);

impl TupleStructA {
    fn _half_value(primary_value: u128) -> TupleStructA {
        assert!(primary_value < (1u128 << 65));
        return TupleStructA(primary_value, (primary_value / 2) as u64);
    }

    fn sum(&self) -> u128 {
        return self.0 + self.1 as u128;
    }
}

fn main() {
    let a_0: u128 = tasm::tasm_io_read_stdin___u128();
    let a_1: u64 = tasm::tasm_io_read_stdin___u64();
    let tsa: TupleStructA = TupleStructA(a_0, a_1);
    let tsa_boxed: Box<TupleStructA> = Box::<TupleStructA>::new(tsa);
    tasm::tasm_io_write_to_stdout___u128(tsa_boxed.sum());

    return;
}

#[cfg(test)]
mod test {

    use itertools::Itertools;
    use rand::random;
    use triton_vm::twenty_first::shared_math::bfield_codec::BFieldCodec;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn method_on_boxed_tuple() {
        let a_0: u128 = random();
        let a_1: u64 = random();
        let mut a0_encoded_reverse = a_0.encode();
        a0_encoded_reverse.reverse();
        let mut a1_encoded_reverse = a_1.encode();
        a1_encoded_reverse.reverse();
        let expected_output = (a_0 + a_1 as u128).encode();
        let stdin = [a0_encoded_reverse, a1_encoded_reverse].concat();
        let non_determinism = NonDeterminism::new(vec![]);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let entrypoint_location =
            EntrypointLocation::disk("boxed", "methods_on_boxed_tuple_structs", "main");
        let test_program =
            ozk_parsing::compile_for_test(&entrypoint_location, crate::ast_types::ListType::Unsafe);
        let expected_stack_diff = 0;
        println!("test_program:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
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
