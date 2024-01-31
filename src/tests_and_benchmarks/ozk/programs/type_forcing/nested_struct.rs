use super::simple_struct::*;

struct NestedStruct {
    my_struct: SimpleStruct,
}

impl NestedStruct {
    fn nested_test_struct() -> NestedStruct {
        let my_struct: SimpleStruct = SimpleStruct::test_struct();
        let nested_struct: NestedStruct = NestedStruct { my_struct };
        return nested_struct;
    }
}

fn access_nested_struct_list_len() {
    let my_struct: NestedStruct = NestedStruct::nested_test_struct();
    let len: usize = my_struct.my_struct.my_list.len();
    assert!(1 == len);
    return;
}

fn access_boxed_nested_struct_list_len() {
    let my_struct: NestedStruct = NestedStruct::nested_test_struct();
    let boxed_struct: Box<NestedStruct> = Box::<NestedStruct>::new(my_struct);
    let len: usize = boxed_struct.my_struct.my_list.len();
    assert!(1 == len);
    return;
}

#[cfg(test)]
mod tests {
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
    use crate::triton_vm::prelude::*;

    use super::*;

    #[test]
    fn accessing_list_element_on_a_nested_struct_produces_expected_value() {
        let native_output = rust_shadows::wrap_main_with_io(&access_nested_struct_list_len)(
            vec![],
            NonDeterminism::default(),
        );

        let entrypoint = EntrypointLocation::disk(
            "type_forcing",
            "nested_struct",
            "access_nested_struct_list_len",
        );
        let vm_output = TritonVMTestCase::new(entrypoint).execute().unwrap();
        assert_eq!(native_output, vm_output.output);
    }

    #[test]
    fn accessing_list_element_on_a_boxed_nested_struct_produces_expected_value() {
        let native_output = rust_shadows::wrap_main_with_io(&access_boxed_nested_struct_list_len)(
            vec![],
            NonDeterminism::default(),
        );

        let entrypoint = EntrypointLocation::disk(
            "type_forcing",
            "nested_struct",
            "access_boxed_nested_struct_list_len",
        );
        let vm_output = TritonVMTestCase::new(entrypoint).execute().unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
