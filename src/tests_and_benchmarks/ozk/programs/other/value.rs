use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use triton_vm::BFieldElement;

pub(crate) fn main() {
    let a: BFieldElement = BFieldElement::new(17u64);
    let av: u64 = a.value();
    let b: BFieldElement = BFieldElement::new(av);
    tasm::tasm_io_write_to_stdout_bfe(b);
    return;
}

mod tests {
    use super::*;
    use crate::tests_and_benchmarks::{
        ozk::{ozk_parsing, rust_shadows},
        test_helpers::shared_test::*,
    };
    use triton_vm::{BFieldElement, NonDeterminism};

    #[test]
    fn value_test() {
        // Test function on host machine
        let stdin : Vec<BFieldElement> = vec![];
        let non_determinism: NonDeterminism<BFieldElement>  = NonDeterminism::new(vec![]);
        let expected_output = vec![BFieldElement::new(17u64)];
        let native_output = rust_shadows::wrap_main_with_io(&main)(
            stdin.clone(),
            non_determinism.clone(),
        );

        assert_eq!(native_output, expected_output);

        // Test function in Triton VM

        let (parsed, _, _) = ozk_parsing::parse_main_and_structs("other", "value");
        let expected_stack_diff = 0;
        let stack_start = vec![];
        let vm_output =  execute_with_stack_safe_lists(&parsed, stack_start, expected_stack_diff).unwrap();
        assert_eq!(expected_output, *vm_output.output);
    }

}
