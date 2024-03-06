use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Clone, Copy, Debug)]
struct DazeFieldElement(u64);

impl DazeFieldElement {
    fn new(value: u64) -> DazeFieldElement {
        return DazeFieldElement(DazeFieldElement::montyred(
            value as u128 * 0xFFFFFFFE00000001u128,
        ));
    }

    fn montyred(x: u128) -> u64 {
        let xl: u64 = x as u64;
        let xh: u64 = (x >> 64) as u64;
        let add_res: (u64, bool) = xl.overflowing_add(xl << 32);

        let b: u64 = add_res
            .0
            .wrapping_sub(add_res.0 >> 32)
            .wrapping_sub(add_res.1 as u64);

        let sub_res: (u64, bool) = xh.overflowing_sub(b);

        return sub_res
            .0
            .wrapping_sub((1 + !0xffff_ffff_0000_0001u64) * sub_res.1 as u64);
    }

    fn canonical_representation(self) -> u64 {
        return DazeFieldElement::montyred(self.0 as u128);
    }

    fn valued(self) -> u64 {
        return self.canonical_representation();
    }

    fn mul(self, rhs: DazeFieldElement) -> DazeFieldElement {
        return DazeFieldElement(DazeFieldElement::montyred(
            tasm::tasm_arithmetic_u64_mul_two_u64s_to_u128_u64(self.0, rhs.0),
        ));
    }
}

fn main() {
    let a: DazeFieldElement = DazeFieldElement::new(tasm::tasm_io_read_stdin___bfe().value());
    let b: DazeFieldElement = DazeFieldElement::new(tasm::tasm_io_read_stdin___bfe().value());
    let res: DazeFieldElement = a.mul(b);
    tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(res.valued()));
    tasm::tasm_io_write_to_stdout___u64(res.valued());

    return;
}

#[cfg(test)]
mod test {
    use tasm_lib::triton_vm::prelude::*;

    use proptest::collection::vec;
    use proptest::prelude::*;
    use proptest_arbitrary_interop::arb;
    use test_strategy::proptest;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[proptest(cases = 20)]
    fn dazefield_element_test(#[strategy(vec(arb(), 2))] std_in: Vec<BFieldElement>) {
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());

        let res = std_in[0] * std_in[1];
        let expected_output = [vec![res], res.value().encode()].concat();
        assert_eq!(native_output, expected_output);

        let entrypoint_location =
            ozk_parsing::EntrypointLocation::disk("arithmetic", "dazefield_element_mul", "main");
        let vm_output = TritonVMTestCase::new(entrypoint_location)
            .with_std_in(std_in)
            .execute()
            .unwrap();

        prop_assert_eq!(expected_output, vm_output.output);
    }
}

mod benches {
    use tasm_lib::triton_vm::prelude::*;

    use crate::tests_and_benchmarks::benchmarks::execute_and_write_benchmark;
    use crate::tests_and_benchmarks::benchmarks::profile;
    use crate::tests_and_benchmarks::benchmarks::BenchmarkInput;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;

    #[test]
    fn dazefield_element_bench() {
        let worst_case_input = BenchmarkInput {
            std_in: vec![
                BFieldElement::new(1u64 << 40),
                BFieldElement::new(1u64 << 40),
            ],
            ..Default::default()
        };
        let common_case = BenchmarkInput {
            std_in: vec![
                BFieldElement::new(BFieldElement::MAX),
                BFieldElement::new(BFieldElement::MAX),
            ],
            ..Default::default()
        };

        let entrypoint_location =
            ozk_parsing::EntrypointLocation::disk("arithmetic", "dazefield_element_mul", "main");
        let code = ozk_parsing::compile_for_test(&entrypoint_location);

        let name = "dazefield_element_mul".to_owned();
        execute_and_write_benchmark(
            name.clone(),
            code.clone(),
            common_case.clone(),
            worst_case_input,
            0,
        );
        profile(name, code, common_case);
    }
}
