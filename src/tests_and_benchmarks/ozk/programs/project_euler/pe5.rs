// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[allow(clippy::needless_else)]
fn main() {
    // https://projecteuler.net/problem=5
    // 2 = 2, 3 = 3, 5 = 5, 6 = 2*3,
    // 7 = 7, 8 = 2^3, 9 = 3^2, 10 = 2*5, 11 = 11, 12 = 3*2^2, 13 = 13, 14 = 2*7, 15 = 3 * 5,
    // 16 = 2^4, 17 = 17, 18 = 2*3^2, 19 = 19, 20 = 2^2*5
    let ret: u32 = 2u32.pow(4) * 3u32.pow(2) * 5 * 7 * 11 * 13 * 17 * 19;
    tasm::tasm_io_write_to_stdout_u32(ret);

    return;
}

mod tests {
    use super::*;
    use crate::tests_and_benchmarks::{
        ozk::{ozk_parsing, rust_shadows},
        test_helpers::shared_test::*,
    };
    use itertools::Itertools;
    use triton_vm::NonDeterminism;

    #[test]
    fn pe5_test() {
        // Test function on host machine
        let stdin = vec![];
        let non_determinism = NonDeterminism::new(vec![]);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        // Test function in Triton VM
        let (parsed, _, _) = ozk_parsing::parse_main_and_structs("project_euler", "pe5");
        let expected_stack_diff = 0;
        let stack_start = vec![];
        let vm_output =
            execute_with_stack_safe_lists(&parsed, stack_start, expected_stack_diff).unwrap();
        assert_eq!(native_output, vm_output.output);

        println!("vm_output.output: {}", vm_output.output.iter().join(","));
    }
}
