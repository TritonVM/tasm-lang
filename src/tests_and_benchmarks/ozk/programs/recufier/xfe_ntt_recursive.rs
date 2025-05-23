#[cfg(test)]
mod test {
    use itertools::Itertools;
    use num::One;
    use tasm_lib::triton_vm::prelude::*;
    use tasm_lib::twenty_first::math::ntt;
    use tasm_lib::twenty_first::math::other::random_elements;
    use tasm_lib::twenty_first::math::traits::PrimitiveRootOfUnity;
    use tasm_lib::twenty_first::prelude::ModPowU32;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn recursive_xfe_ntt_test() {
        // Test function on host machine
        for input_length in [2, 4, 8, 16, 32, 64] {
            let xfes: Vec<XFieldElement> = random_elements(input_length);
            let omega = BFieldElement::primitive_root_of_unity(input_length as u64).unwrap();
            let stdin = vec![omega];
            let non_determinism =
                init_memory_from(&xfes, BFieldElement::new(0x1000_0000_0000_0000u64));
            let mut expected_output = xfes.clone();
            ntt::ntt(&mut expected_output);
            let expected_output = expected_output
                .iter()
                .flat_map(|x| x.coefficients.to_vec())
                .collect_vec();
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
            // assert_eq!(native_output, expected_output);
            if native_output != expected_output {
                panic!(
                    "native_output:\n{}\n\nexpected_output:\n{}",
                    native_output.iter().join(","),
                    expected_output.iter().join(",")
                )
            }

            // Test function in Triton VM
            let entrypoint_location =
                EntrypointLocation::disk("recufier", "xfe_ntt_recursive", "test::main");
            let rust_ast = entrypoint_location.extract_entrypoint();
            let expected_stack_diff = 0;
            let (code, _fn_name) = compile_for_run_test(&rust_ast);
            let vm_output = execute_compiled_with_stack_and_ins_for_test(
                &code,
                vec![],
                stdin,
                non_determinism,
                expected_stack_diff,
            )
            .unwrap();
            assert_eq!(native_output, vm_output.public_output);
        }
    }

    /// Dual-compiled function that implements NTT efficiently
    #[allow(clippy::ptr_arg)]
    #[allow(clippy::vec_init_then_push)]
    fn main() {
        fn base_case(x: &Vec<XFieldElement>, omega: BFieldElement) -> Vec<XFieldElement> {
            let mut ret: Vec<XFieldElement> = Vec::<XFieldElement>::default();
            ret.push(x[0] + x[1]);
            ret.push(x[0] + x[1] * omega);

            return ret;
        }

        fn xfe_ntt(x: Vec<XFieldElement>, omega: BFieldElement) -> Vec<XFieldElement> {
            let size: usize = x.len();
            let res: Vec<XFieldElement> = if size == 2 {
                base_case(&x, omega)
            } else {
                // Split by parity
                let mut x_even: Vec<XFieldElement> = Vec::<XFieldElement>::default();
                let mut x_odd: Vec<XFieldElement> = Vec::<XFieldElement>::default();
                let mut i: usize = 0;
                while i < size {
                    if i % 2 == 0 {
                        x_even.push(x[i]);
                    } else {
                        x_odd.push(x[i]);
                    }
                    i += 1;
                }

                // Recursive call
                let omega_squared: BFieldElement = omega * omega;
                let even: Vec<XFieldElement> = xfe_ntt(x_even, omega_squared);
                let odd: Vec<XFieldElement> = xfe_ntt(x_odd, omega_squared);

                // Calculate all values omega^j, for j=0..size
                let mut factor_values: Vec<BFieldElement> = Vec::<BFieldElement>::default();
                i = 0;
                let mut pow: BFieldElement = BFieldElement::one();
                while i < size {
                    factor_values.push(pow);
                    pow *= omega;
                    i += 1;
                }

                // Split by middle
                let mut fst_half_factors: Vec<BFieldElement> = Vec::<BFieldElement>::default();
                let mut snd_half_factors: Vec<BFieldElement> = Vec::<BFieldElement>::default();
                i = 0;
                while i != size / 2 {
                    fst_half_factors.push(factor_values[i]);
                    i += 1;
                }
                while i != size {
                    snd_half_factors.push(factor_values[i]);
                    i += 1;
                }

                // hadamard products
                let mut res: Vec<XFieldElement> = Vec::<XFieldElement>::default();
                i = 0;
                while i != size / 2 {
                    res.push(even[i] + odd[i] * fst_half_factors[i]);
                    i += 1;
                }
                i = 0;
                while i != size / 2 {
                    res.push(even[i] + odd[i] * snd_half_factors[i]);
                    i += 1;
                }

                res
            };

            return res;
        }

        fn xfe_intt(x: Vec<XFieldElement>, omega: BFieldElement) -> Vec<XFieldElement> {
            let length: usize = x.len();
            let xfe_length: XFieldElement = BFieldElement::new(length as u64).lift();
            let omega_inv: BFieldElement = BFieldElement::one() / omega;
            let mut res: Vec<XFieldElement> = xfe_ntt(x, omega_inv);

            let mut i: usize = 0;
            while i < length {
                res[i] = res[i] / xfe_length;
                i += 1;
            }

            return res;
        }

        // NTT is equivalent to polynomial evaluation over the field generated by the `omega` generator
        // where the input values are interpreted as coefficients. So an input of `[C, 0]` must output
        // `[C, C]`, as the output is $P(x) = C$.
        let omega: BFieldElement = tasm::tasmlib_io_read_stdin___bfe();
        let input: Box<Vec<XFieldElement>> = Vec::<XFieldElement>::decode(&tasm::load_from_memory(
            BFieldElement::new(0x1000_0000_0000_0000u64),
        ))
        .unwrap();
        let output: Vec<XFieldElement> = xfe_ntt(*input, omega);
        let size: usize = output.len();
        assert!(BFieldElement::one() == omega.mod_pow_u32(size as u32));

        let mut i: usize = 0;

        while i < size {
            tasm::tasmlib_io_write_to_stdout___xfe(output[i]);
            i += 1;
        }

        // We only output the NTT for the test, but we test that `xfe_intt` produces the
        // inverse of `xfe_ntt`.
        let input_again: Vec<XFieldElement> = xfe_intt(output, omega);
        let input_copied: Box<Vec<XFieldElement>> = Vec::<XFieldElement>::decode(
            &tasm::load_from_memory(BFieldElement::new(0x1000_0000_0000_0000u64)),
        )
        .unwrap();
        i = 0;
        while i < size {
            assert!(input_copied[i] == input_again[i]);
            i += 1;
        }

        return;
    }
}
