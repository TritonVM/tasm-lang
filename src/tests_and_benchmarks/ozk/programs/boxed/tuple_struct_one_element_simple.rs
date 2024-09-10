#[derive(Clone, Copy)]
struct TupleStruct1(u32);

fn main() {
    let a: u32 = 112;
    let ts_a: TupleStruct1 = TupleStruct1(a);
    let boxed_ts_a: Box<TupleStruct1> = Box::<TupleStruct1>::new(ts_a);
    assert!(ts_a.0 == a);
    assert!(boxed_ts_a.0 == a);

    let ts_again_a: TupleStruct1 = *boxed_ts_a;
    let a_again: u32 = ts_again_a.0;
    assert!(a == a_again);

    return;
}

#[cfg(test)]
mod test {
    use tasm_lib::triton_vm::prelude::NonDeterminism;

    use super::*;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn tuple_struct_one_element_simple_test() {
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(vec![], NonDeterminism::default());

        let entrypoint =
            EntrypointLocation::disk("boxed", "tuple_struct_one_element_simple", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .execute()
            .unwrap()
            .public_output;

        assert_eq!(native_output, vm_output);
    }
}
