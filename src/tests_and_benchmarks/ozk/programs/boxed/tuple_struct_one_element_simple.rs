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
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use triton_vm::NonDeterminism;

    #[test]
    fn tuple_struct_one_element_simple_test() {
        let stdin = vec![];
        let test_program = ozk_parsing::compile_for_test(
            "boxed",
            "tuple_struct_one_element_simple",
            "_main",
            crate::ast_types::ListType::Unsafe,
        );
        execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            NonDeterminism::new(vec![]),
            0,
        )
        .unwrap();
    }
}
