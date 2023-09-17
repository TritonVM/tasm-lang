use triton_vm::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Debug)]
struct DazeFieldElement(u64);

impl DazeFieldElement {
    fn montyred(x: u128) -> u64 {
        // See reference above for a description of the following implementation.
        let xl = x as u64;
        let xh = (x >> 64) as u64;
        let (a, e) = xl.overflowing_add(xl << 32);

        let b = a.wrapping_sub(a >> 32).wrapping_sub(e as u64);

        let (r, c) = xh.overflowing_sub(b);

        return r.wrapping_sub((1 + !0xffff_ffff_0000_0001u64) * c as u64);
    }

    fn new(value: u64) -> DazeFieldElement {
        return Self(Self::montyred(
            tasm::tasm_arithmetic_u64_mul_two_u64s_to_u128_u64(value, 0xFFFFFFFE00000001),
        ));
    }

    fn canonical_representation(&self) -> u64 {
        return Self::montyred(self.0 as u128);
    }

    fn value(&self) -> u64 {
        return self.canonical_representation();
    }

    fn mul(self, rhs: DazeFieldElement) -> Self {
        return Self(Self::montyred(
            tasm::tasm_arithmetic_u64_mul_two_u64s_to_u128_u64(self.0, rhs.0),
        ));
        // Self(Self::montyred((self.0 as u128) * (rhs.0 as u128)))
    }
}

fn main() {
    let a: DazeFieldElement = DazeFieldElement::new(1_000_000_000u64);
    let b: DazeFieldElement = DazeFieldElement::new(1_000_000_001u64);
    let prod: DazeFieldElement = a.mul(b);

    let a_bfe: BFieldElement = BFieldElement::new(1_000_000_000u64);
    let b_bfe: BFieldElement = BFieldElement::new(1_000_000_001u64);
    let prod_bfe: BFieldElement = a_bfe * b_bfe;

    tasm::tasm_io_write_to_stdout_u64(prod.value());

    return;
}

mod tests {
    use super::*;
    use crate::tests_and_benchmarks::{
        ozk::{ozk_parsing, rust_shadows},
        test_helpers::shared_test::*,
    };
    use triton_vm::{BFieldElement, NonDeterminism};
    use twenty_first::shared_math::bfield_codec::BFieldCodec;

    #[test]
    fn bfe_mul_test() {
        // Test function on host machine
        let stdin = vec![];
        let non_determinism = NonDeterminism::new(vec![]);
        let expected_output = (BFieldElement::new(1_000_000_000u64)
            * BFieldElement::new(1_000_000_001u64))
        .value()
        .encode();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test(
            "arithmetic",
            "bfe_mul",
            crate::ast_types::ListType::Unsafe,
        );
        // let (parsed, _, _) = ozk_parsing::parse_main_and_structs("arithmetic", "bfe_mul");
        // let expected_stack_diff = 0;
        // let stack_start = vec![];
        // let vm_output =
        //     execute_with_stack_safe_lists(&parsed, stack_start, expected_stack_diff).unwrap();
        // assert_eq!(expected_output, vm_output.output);
    }
}
