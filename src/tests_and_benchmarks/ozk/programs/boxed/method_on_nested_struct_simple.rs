#[derive(Clone, Copy)]
struct OuterStruct(InnerStruct);

#[derive(Clone, Copy)]
struct InnerStruct(u32);

impl InnerStruct {
    fn _double(self) -> u32 {
        return self.0 + self.0;
    }
}

fn _main() {
    let inner_a: InnerStruct = InnerStruct(2001);
    let outer_struct: OuterStruct = OuterStruct(inner_a);
    let outer_struct_boxed: Box<OuterStruct> = Box::<OuterStruct>::new(outer_struct);
    assert!(4002 == outer_struct_boxed.0._double());

    return;
}

#[cfg(test)]
mod test {
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use triton_vm::NonDeterminism;

    #[test]
    fn methods_on_nested_structs_simple_test() {
        let entrypoint_location =
            EntrypointLocation::disk("boxed", "method_on_nested_struct_simple", "_main");
        let test_program =
            ozk_parsing::compile_for_test(&entrypoint_location, crate::ast_types::ListType::Unsafe);

        let stdin = vec![];
        let non_determinism = NonDeterminism::new(vec![]);
        let expected_stack_diff = 0;
        execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap();
    }
}
