fn main() {
    assert!(2u64 == 3u64.wrapping_sub(1u64));
    assert!(
        0x0000_0004_0000_0003u64 == 0x0000_000b_0000_0005u64.wrapping_sub(0x0000_0007_0000_0002u64)
    );

    return;
}

#[cfg(test)]
mod test {

    use itertools::Itertools;
    use tasm_lib::triton_vm::prelude::*;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn wrapping_sub_test() {
        let stdin = vec![];
        let non_determinism = NonDeterminism::default();
        let expected_output = vec![];
        let native_output = rust_shadows::wrap_main_with_io(&main)(stdin, non_determinism);
        assert_eq!(native_output, expected_output);

        let entrypoint_location =
            ozk_parsing::EntrypointLocation::disk("arithmetic", "wrapping_sub", "main");
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
