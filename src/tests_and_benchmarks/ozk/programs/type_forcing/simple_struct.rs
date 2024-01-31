use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
use crate::tests_and_benchmarks::ozk::rust_shadows;
use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
use crate::triton_vm::prelude::*;

pub(crate) struct SimpleStruct {
    pub my_list: Vec<u32>,
}

impl SimpleStruct {
    pub fn test_struct() -> SimpleStruct {
        let mut list: Vec<u32> = Vec::<u32>::with_capacity(1);
        list.push(42);

        let my_struct: SimpleStruct = SimpleStruct { my_list: list };
        return my_struct;
    }
}

fn access_simple_struct_list() {
    let my_struct: SimpleStruct = SimpleStruct::test_struct();
    let must_be_42: u32 = my_struct.my_list[0];
    assert!(42 == must_be_42);
    return;
}

fn access_boxed_simple_struct_list() {
    let my_struct: SimpleStruct = SimpleStruct::test_struct();
    let boxed_struct: Box<SimpleStruct> = Box::<SimpleStruct>::new(my_struct);
    let must_be_42: u32 = boxed_struct.my_list[0];
    assert!(42 == must_be_42);
    return;
}

fn access_simple_struct_list_len() {
    let my_struct: SimpleStruct = SimpleStruct::test_struct();
    let len: usize = my_struct.my_list.len();
    assert!(1 == len);
    return;
}

fn access_boxed_simple_struct_list_len() {
    let my_struct: SimpleStruct = SimpleStruct::test_struct();
    let boxed_struct: Box<SimpleStruct> = Box::<SimpleStruct>::new(my_struct);
    let len: usize = boxed_struct.my_list.len();
    assert!(1 == len);
    return;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accessing_list_element_on_a_struct_produces_expected_value() {
        let native_output = rust_shadows::wrap_main_with_io(&access_simple_struct_list)(
            vec![],
            NonDeterminism::default(),
        );

        let entrypoint =
            EntrypointLocation::disk("type_forcing", "simple_struct", "access_simple_struct_list");
        let vm_output = TritonVMTestCase::new(entrypoint).execute().unwrap();
        assert_eq!(native_output, vm_output.output);
    }

    #[test]
    fn accessing_list_element_on_a_boxed_struct_produces_expected_value() {
        let native_output = rust_shadows::wrap_main_with_io(&access_boxed_simple_struct_list)(
            vec![],
            NonDeterminism::default(),
        );

        let entrypoint = EntrypointLocation::disk(
            "type_forcing",
            "simple_struct",
            "access_boxed_simple_struct_list",
        );
        let vm_output = TritonVMTestCase::new(entrypoint).execute().unwrap();
        assert_eq!(native_output, vm_output.output);
    }

    #[test]
    fn accessing_list_len_on_a_struct_produces_expected_value() {
        let native_output = rust_shadows::wrap_main_with_io(&access_simple_struct_list_len)(
            vec![],
            NonDeterminism::default(),
        );

        let entrypoint = EntrypointLocation::disk(
            "type_forcing",
            "simple_struct",
            "access_simple_struct_list_len",
        );
        let vm_output = TritonVMTestCase::new(entrypoint).execute().unwrap();
        assert_eq!(native_output, vm_output.output);
    }

    #[test]
    fn accessing_list_len_on_a_boxed_struct_produces_expected_value() {
        let native_output = rust_shadows::wrap_main_with_io(&access_boxed_simple_struct_list_len)(
            vec![],
            NonDeterminism::default(),
        );

        let entrypoint = EntrypointLocation::disk(
            "type_forcing",
            "simple_struct",
            "access_boxed_simple_struct_list_len",
        );
        let vm_output = TritonVMTestCase::new(entrypoint).execute().unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
