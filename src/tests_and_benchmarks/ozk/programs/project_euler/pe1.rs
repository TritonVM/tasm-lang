// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[allow(clippy::needless_else)]
fn main() {
    let mut i: u32 = 1;
    let mut acc: u32 = 0;

    while i < 1000 {
        if i % 3 == 0 || i % 5 == 0 {
            acc += i;
        } else {
        }

        i += 1;
    }

    tasm::tasm_io_write_to_stdout_u32(acc);

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
    fn pe1_test() {
        // Test function on host machine
        let stdin = vec![];
        let non_determinism = NonDeterminism::new(vec![]);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        // Test function in Triton VM
        let (parsed, _, _) = ozk_parsing::parse_main_and_structs("project_euler", "pe1");
        let expected_stack_diff = 0;
        let stack_start = vec![];
        let vm_output =
            execute_with_stack_safe_lists(&parsed, stack_start, expected_stack_diff).unwrap();
        assert_eq!(native_output, vm_output.output);

        println!("vm_output.output: {}", vm_output.output.iter().join(","));
    }
}
