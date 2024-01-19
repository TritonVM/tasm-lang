#[derive(Clone, Copy)]
struct TupleStruct1(u32);

fn _main() {
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
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn tuple_struct_one_element_simple_test() {
        let entrypoint =
            EntrypointLocation::disk("boxed", "tuple_struct_one_element_simple", "_main");
        TritonVMTestCase::new(entrypoint)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
    }
}
