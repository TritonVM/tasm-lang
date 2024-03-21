use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Clone, Copy, Debug)]
struct DazeFieldElement(u64);

impl DazeFieldElement {
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
}

fn main() {
    // These values were found using the `twenty-first` repository
    assert!(18446744065119617025u64 == DazeFieldElement::montyred(1));
    assert!(0u64 == DazeFieldElement::montyred(0xFFFFFFFF00000001));
    assert!(4294967296u64 == DazeFieldElement::montyred(0xFFFFFFFF00000000));
    assert!(8589934592u64 == DazeFieldElement::montyred(0xFFFFFFFEFFFFFFFF));
    assert!(67108864u64 == DazeFieldElement::montyred(1u128 << 90));
    assert!(67108865u64 != DazeFieldElement::montyred(1u128 << 90));
    assert!(67108863u64 != DazeFieldElement::montyred(1u128 << 90));
    assert!(9223372036854775808u64 == DazeFieldElement::montyred(1 << 127));
    assert!(8589934591u64 == DazeFieldElement::montyred(u128::MAX));
    assert!(12884901887u64 == DazeFieldElement::montyred(u128::MAX - 1));
    assert!(4294967297u64 == DazeFieldElement::montyred(u64::MAX as u128));
    assert!(8589934593u64 == DazeFieldElement::montyred(u64::MAX as u128 - 1));

    tasm::tasm_io_write_to_stdout___u64(DazeFieldElement::montyred(1u128 << 90));
    tasm::tasm_io_write_to_stdout___u64(DazeFieldElement::montyred(1));
    tasm::tasm_io_write_to_stdout___u64(DazeFieldElement::montyred(1000));
    tasm::tasm_io_write_to_stdout___u64(DazeFieldElement::montyred(u64::MAX as u128));
    tasm::tasm_io_write_to_stdout___u64(DazeFieldElement::montyred(((1u128 << 40) as u64) as u128));
    tasm::tasm_io_write_to_stdout___u64(DazeFieldElement::montyred(
        (((1u128 << 40) + 1) as u64) as u128,
    ));
    tasm::tasm_io_write_to_stdout___u64(DazeFieldElement::montyred(0xFFFFFFFF00000000));
    tasm::tasm_io_write_to_stdout___u64(DazeFieldElement::montyred(2u128 * 0xFFFFFFFE00000001u128));

    // Use daze-field element value to suppress compiler-warning
    let dfe: DazeFieldElement = DazeFieldElement(404);
    tasm::tasm_io_write_to_stdout___u64(dfe.0);

    return;
}

#[cfg(test)]
mod test {

    use tasm_lib::triton_vm::prelude::*;

    use itertools::Itertools;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn montyred_test() {
        let stdin = vec![];
        let non_determinism = NonDeterminism::default();
        let expected_output = [
            BFieldElement::montyred(1u128 << 90).encode(),
            BFieldElement::montyred(1).encode(),
            BFieldElement::montyred(1000).encode(),
            BFieldElement::montyred(u64::MAX as u128).encode(),
            BFieldElement::montyred(1 << 40).encode(),
            BFieldElement::montyred((1 << 40) + 1).encode(),
            BFieldElement::montyred(0xFFFFFFFF00000000).encode(),
            BFieldElement::montyred(2u128 * 0xFFFFFFFE00000001u128).encode(),
            404u64.encode(),
        ]
        .concat();
        let native_output = rust_shadows::wrap_main_with_io(&main)(stdin, non_determinism);
        assert_eq!(native_output, expected_output);

        let entrypoint_location =
            ozk_parsing::EntrypointLocation::disk("arithmetic", "montyred", "main");
        let vm_output = TritonVMTestCase::new(entrypoint_location)
            .execute()
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
