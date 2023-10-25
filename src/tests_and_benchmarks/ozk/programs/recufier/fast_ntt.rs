#![allow(clippy::manual_swap)]

// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use num::One;
use triton_vm::BFieldElement;
use twenty_first::shared_math::{
    bfield_codec::BFieldCodec, traits::ModPowU32, x_field_element::XFieldElement,
};

#[allow(clippy::ptr_arg)]
#[allow(clippy::vec_init_then_push)]
fn main() {
    fn xfe_ntt(x: &mut Vec<XFieldElement>, omega: BFieldElement, log_2_of_n: u32) {
        fn bitreverse(mut n: u32, l: u32) -> u32 {
            let mut r: u32 = 0;
            let mut i: u32 = 0;
            while i < l {
                r = (r << 1) | (n & 1);
                n >>= 1;
                i += 1;
            }

            return r;
        }

        let n: u32 = x.len() as u32;

        {
            let mut k: u32 = 0;
            while k != n {
                let rk: u32 = bitreverse(k, log_2_of_n);
                if k < rk {
                    // TODO: Use `swap` here instead, once it's implemented in `tasm-lib`
                    // That will give us a shorter cycle count
                    // x.swap(rk as usize, k as usize);
                    let rk_val: XFieldElement = x[rk as usize];
                    x[rk as usize] = x[k as usize];
                    x[k as usize] = rk_val;
                }

                k += 1;
            }
        }

        let mut m: u32 = 1;

        let mut outer_count: u32 = 0;
        while outer_count != log_2_of_n {
            // for _ in 0..log_2_of_n {
            let w_m: BFieldElement = omega.mod_pow_u32(n / (2 * m));
            let mut k: u32 = 0;
            while k < n {
                let mut w: BFieldElement = BFieldElement::one();
                let mut j: u32 = 0;
                while j != m {
                    // for j in 0..m {
                    let u: XFieldElement = x[(k + j) as usize];
                    let mut v: XFieldElement = x[(k + j + m) as usize];
                    v *= w;
                    x[(k + j) as usize] = u + v;
                    x[(k + j + m) as usize] = u - v;
                    w *= w_m;

                    j += 1;
                }

                k += 2 * m;
            }

            m *= 2;

            outer_count += 1;
        }

        return;
    }

    fn xfe_intt(x: &mut Vec<XFieldElement>, omega: BFieldElement, log_2_of_n: u32) {
        {
            let omega_inv: BFieldElement = BFieldElement::one() / omega;
            xfe_ntt(x, omega_inv, log_2_of_n);
        }

        let n_inv: BFieldElement = {
            let n: BFieldElement = BFieldElement::new(x.len() as u64);
            BFieldElement::one() / n
        };

        let mut i: usize = 0;
        let len: usize = x.len();
        while i < len {
            x[i] *= n_inv;
            i += 1;
        }

        return;
    }

    // NTT is equivalent to polynomial evaluation over the field generated by the `omega` generator
    // where the input values are interpreted as coefficients. So an input of `[C, 0]` must output
    // `[C, C]`, as the output is $P(x) = C$.
    let omega: BFieldElement = tasm::tasm_io_read_stdin___bfe();
    let input_output_boxed: Box<Vec<XFieldElement>> = Vec::<XFieldElement>::decode(
        &tasm::load_from_memory(BFieldElement::new(0x1000_0000_0000_0000u64)),
    )
    .unwrap();
    let mut input_output: Vec<XFieldElement> = *input_output_boxed;
    let size: usize = input_output.len();
    let log_2_size: u32 = u32::BITS - (size as u32).leading_zeros() - 1;
    xfe_ntt(&mut input_output, omega, log_2_size);
    assert!(BFieldElement::one() == omega.mod_pow_u32(size as u32));

    let mut i: usize = 0;

    while i < size {
        tasm::tasm_io_write_to_stdout___xfe(input_output[i]);
        i += 1;
    }

    // We only output the NTT for the test, but we test that `xfe_intt` produces the
    // inverse of `xfe_ntt`.
    xfe_intt(&mut input_output, omega, log_2_size);
    let input_copied: Box<Vec<XFieldElement>> = Vec::<XFieldElement>::decode(
        &tasm::load_from_memory(BFieldElement::new(0x1000_0000_0000_0000u64)),
    )
    .unwrap();
    i = 0;
    while i < size {
        assert!(input_copied[i] == input_output[i]);
        i += 1;
    }

    return;
}

mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::{
        ast_types,
        tests_and_benchmarks::{
            ozk::{ozk_parsing, rust_shadows},
            test_helpers::shared_test::*,
        },
    };
    use itertools::Itertools;
    use triton_vm::BFieldElement;
    use twenty_first::shared_math::{
        ntt,
        other::{log_2_floor, random_elements},
        traits::PrimitiveRootOfUnity,
    };

    #[test]
    fn fast_xfe_ntt_test() {
        for input_length in [2, 4, 8, 16, 32, 64, 128] {
            let xfes: Vec<XFieldElement> = random_elements(input_length);
            let omega = BFieldElement::primitive_root_of_unity(input_length as u64).unwrap();
            let stdin = vec![omega];
            let non_determinism =
                init_memory_from(&xfes, BFieldElement::new(0x1000_0000_0000_0000u64));

            // Test function on host machine
            let mut expected_output = xfes.clone();
            ntt::ntt(
                &mut expected_output,
                omega,
                log_2_floor(input_length as u128) as u32,
            );
            let expected_output = expected_output
                .iter()
                .flat_map(|x| x.coefficients.to_vec())
                .collect_vec();
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
            if native_output != expected_output {
                panic!(
                    "native_output:\n{}\n\nexpected_output:\n{}",
                    native_output.iter().join(","),
                    expected_output.iter().join(",")
                )
            }

            // Test function in Triton VM
            let (rust_ast, _, _) =
                ozk_parsing::parse_function_and_structs("recufier", "fast_ntt", "main");
            let expected_stack_diff = 0;
            let (code, _fn_name) = compile_for_run_test(&rust_ast, ast_types::ListType::Unsafe);
            let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
                &code,
                vec![],
                &mut HashMap::default(),
                stdin,
                non_determinism,
                expected_stack_diff,
            )
            .unwrap();
            assert_eq!(native_output, vm_output.output);
        }
    }
}

mod benches {
    use super::*;
    use crate::tests_and_benchmarks::{
        benchmarks::{execute_and_write_benchmark, profile, BenchmarkInput},
        ozk::ozk_parsing,
        test_helpers::shared_test::*,
    };
    use triton_vm::BFieldElement;
    use twenty_first::shared_math::{other::random_elements, traits::PrimitiveRootOfUnity};

    #[test]
    fn fast_ntt_bench() {
        fn get_input(length: usize) -> BenchmarkInput {
            let xfes: Vec<XFieldElement> = random_elements(length);
            let omega = BFieldElement::primitive_root_of_unity(length as u64).unwrap();
            let std_in = vec![omega];
            let non_determinism =
                init_memory_from(&xfes, BFieldElement::new(0x1000_0000_0000_0000u64));

            BenchmarkInput {
                std_in,
                non_determinism,
                ..Default::default()
            }
        }

        let code = ozk_parsing::compile_for_test(
            "recufier",
            "fast_ntt",
            "main",
            crate::ast_types::ListType::Unsafe,
        );

        let common_case_input = get_input(32);
        let worst_case_input = get_input(128);

        let name = "fast_ntt".to_owned();
        execute_and_write_benchmark(
            name.clone(),
            code.clone(),
            common_case_input.clone(),
            worst_case_input,
            0,
        );
        profile(name, code, common_case_input);
    }
}
