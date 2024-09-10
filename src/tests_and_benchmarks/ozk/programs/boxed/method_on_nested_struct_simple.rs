#[derive(Clone, Copy)]
struct OuterStruct(InnerStruct);

#[derive(Clone, Copy)]
struct InnerStruct(u32);

impl InnerStruct {
    fn double(self) -> u32 {
        return self.0 + self.0;
    }
}

fn main() {
    let inner_a: InnerStruct = InnerStruct(2001);
    let outer_struct: OuterStruct = OuterStruct(inner_a);
    let outer_struct_boxed: Box<OuterStruct> = Box::<OuterStruct>::new(outer_struct);
    assert!(4002 == outer_struct_boxed.0.double());

    return;
}

#[cfg(test)]
mod test {
    use tasm_lib::triton_vm::prelude::*;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn methods_on_nested_structs_simple_test() {
        let stdin = vec![];
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        let entrypoint_location =
            EntrypointLocation::disk("boxed", "method_on_nested_struct_simple", "main");
        let test_program = ozk_parsing::compile_for_test(&entrypoint_location);

        let expected_stack_diff = 0;
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap()
        .public_output;

        assert_eq!(native_output, vm_output);
    }
}
