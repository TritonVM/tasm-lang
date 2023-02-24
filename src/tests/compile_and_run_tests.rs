use std::collections::HashMap;

use num::{One, Zero};
use rand::{thread_rng, Rng, RngCore};

use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::ast;
use crate::tests::programs::*;
use crate::tests::shared_test::*;

#[test]
fn simple_sub_test() {
    compare_prop_with_stack(
        &simple_sub(),
        vec![u32_lit(100), u32_lit(51)],
        vec![u32_lit(49)],
    );
}

#[test]
fn operator_evaluation_ordering_test() {
    compare_prop_with_stack(
        &operator_evaluation_ordering_with_div_u32(),
        vec![],
        vec![u32_lit(94)],
    );

    compare_prop_with_stack(
        &operator_evaluation_ordering_with_div_u64(),
        vec![],
        vec![u64_lit(94)],
    );

    compare_prop_with_stack(
        &operator_evaluation_ordering_with_mul(),
        vec![],
        vec![u32_lit(60)],
    );
}

#[test]
fn add_u64_run_test() {
    compare_prop_with_stack(
        &add_u64_rast(),
        vec![
            u64_lit((1 << 33) + (1 << 16)),
            u64_lit((1 << 33) + (1 << 16)),
        ],
        vec![u64_lit((1 << 34) + (1 << 17))],
    );
    for _ in 0..10 {
        let lhs = thread_rng().gen_range(0..u64::MAX / 2);
        let rhs = thread_rng().gen_range(0..u64::MAX / 2);
        compare_prop_with_stack(
            &add_u64_rast(),
            vec![u64_lit(lhs), u64_lit(rhs)],
            vec![u64_lit(lhs + rhs)],
        )
    }
}

#[test]
fn sub_u32_run_test() {
    let input_args_1 = vec![u32_lit(200), u32_lit(95)];
    let expected_outputs_1 = vec![u32_lit(105)];
    compare_prop_with_stack(&sub_u32_rast_1(), input_args_1, expected_outputs_1);

    let input_args_2 = vec![u32_lit(95), u32_lit(200)];
    let expected_outputs_2 = vec![u32_lit(105)];
    compare_prop_with_stack(&sub_u32_rast_2(), input_args_2, expected_outputs_2);
}

#[test]
fn sub_u64_run_test() {
    let input_args_1 = vec![u64_lit(200), u64_lit(95)];
    let expected_outputs_1 = vec![u64_lit(105)];
    compare_prop_with_stack(&sub_u64_rast_1(), input_args_1, expected_outputs_1);

    let input_args_2 = vec![u64_lit(95), u64_lit(200)];
    let expected_outputs_2 = vec![u64_lit(105)];
    compare_prop_with_stack(&sub_u64_rast_2(), input_args_2, expected_outputs_2);

    let input_args_3 = vec![u64_lit(1), u64_lit(1 << 32)];
    let expected_outputs_3 = vec![u64_lit(u32::MAX as u64)];
    compare_prop_with_stack(&sub_u64_rast_2(), input_args_3, expected_outputs_3);

    let lhs = thread_rng().gen_range(0..u64::MAX);
    let rhs = thread_rng().gen_range(0..=lhs);
    let input_args_4 = vec![u64_lit(rhs), u64_lit(lhs)];
    let expected_outputs_4 = vec![u64_lit(lhs - rhs)];
    compare_prop_with_stack(&sub_u64_rast_2(), input_args_4, expected_outputs_4);
}

#[test]
fn right_child_run_test() {
    compare_prop_with_stack(&right_child_rast(), vec![u64_lit(120)], vec![u64_lit(119)]);
    let mut rng = thread_rng();
    let rand = rng.next_u64();
    compare_prop_with_stack(
        &right_child_rast(),
        vec![u64_lit(rand)],
        vec![u64_lit(rand - 1)],
    );
}

#[test]
fn left_child_run_test() {
    let inputs0 = vec![u64_lit(120), u32_lit(2)];
    let outputs0 = vec![u64_lit(116)];
    compare_prop_with_stack(&left_child_rast(), inputs0, outputs0);

    let inputs1 = vec![u64_lit(31), u32_lit(4)];
    let outputs1 = vec![u64_lit(15)];
    compare_prop_with_stack(&left_child_rast(), inputs1, outputs1);
}

// right_lineage_length_test
#[test]
fn right_lineage_length_run_test() {
    fn prop_right_lineage_length_run(node_index: u64, expected: u32) {
        let inputs = vec![u64_lit(node_index)];
        let outputs = vec![u32_lit(expected)];
        compare_prop_with_stack(
            &right_lineage_length_stmt_rast(),
            inputs.clone(),
            outputs.clone(),
        );
        compare_prop_with_stack(&right_lineage_length_expr_rast(), inputs, outputs);
    }

    prop_right_lineage_length_run(1, 0);
    prop_right_lineage_length_run(2, 1);
    prop_right_lineage_length_run(3, 0);
    prop_right_lineage_length_run(4, 0);
    prop_right_lineage_length_run(5, 2);
    prop_right_lineage_length_run(6, 1);
    prop_right_lineage_length_run(7, 0);
    prop_right_lineage_length_run(8, 0);
    prop_right_lineage_length_run(9, 1);
    prop_right_lineage_length_run(10, 0);
    prop_right_lineage_length_run(11, 0);
    prop_right_lineage_length_run(12, 3);
    prop_right_lineage_length_run(13, 2);
    prop_right_lineage_length_run(14, 1);
    prop_right_lineage_length_run(15, 0);
    prop_right_lineage_length_run(16, 0);
    prop_right_lineage_length_run(17, 1);
    prop_right_lineage_length_run(18, 0);
    prop_right_lineage_length_run(19, 0);
    prop_right_lineage_length_run(20, 2);
    prop_right_lineage_length_run(21, 1);
    prop_right_lineage_length_run(22, 0);
    prop_right_lineage_length_run(23, 0);
    prop_right_lineage_length_run(24, 1);
    prop_right_lineage_length_run(25, 0);
    prop_right_lineage_length_run(26, 0);
    prop_right_lineage_length_run(27, 4);
    prop_right_lineage_length_run(28, 3);
    prop_right_lineage_length_run(29, 2);
    prop_right_lineage_length_run(30, 1);
    prop_right_lineage_length_run(31, 0);
    prop_right_lineage_length_run(32, 0);
    prop_right_lineage_length_run(33, 1);
}

#[test]
fn lt_u32_test() {
    compare_prop_with_stack(&lt_u32(), vec![], vec![ast::ExprLit::Bool(true)]);
}

#[test]
fn simple_while_loop_run_test() {
    compare_prop_with_stack(&simple_while_loop(), vec![], vec![u32_lit(5050)]);
}

#[test]
fn complicated_while_loop_test() {
    compare_prop_with_stack(
        &longer_while_loop(),
        vec![u32_lit(1000)],
        vec![u64_lit(2641)],
    );
    compare_prop_with_stack(
        &while_loop_with_declarations(),
        vec![u32_lit(2000)],
        vec![u64_lit(3641)],
    );
    compare_prop_with_stack(
        &while_loop_with_declarations(),
        vec![u32_lit(2001)],
        vec![u64_lit(3642)],
    );
}

#[test]
fn code_block_run_test() {
    fn prop_code_block(input: u64) {
        let inputs = vec![u64_lit(input)];
        let outputs = vec![u32_lit(2 * (input as u32) + 2)];
        compare_prop_with_stack(&code_block(), inputs, outputs);
    }

    let mut rng = thread_rng();
    for _ in 0..10 {
        prop_code_block(rng.gen_range(0..(1u64 << 30)));
    }
}

#[test]
fn simple_list_support_run_test() {
    use tasm_lib::rust_shadowing_helper_functions::unsafe_list::unsafe_list_new;
    use tasm_lib::rust_shadowing_helper_functions::unsafe_list::unsafe_list_push;
    use tasm_lib::rust_shadowing_helper_functions::unsafe_list::unsafe_list_set_length;

    let inputs = vec![];
    let outputs = vec![
        ast::ExprLit::BFE(BFieldElement::zero()),
        u32_lit(2),
        u64_lit(4000),
        u64_lit(2000),
    ];

    let mut memory = HashMap::default();
    let list_pointer = BFieldElement::zero();
    unsafe_list_new(list_pointer, &mut memory);

    let elem_1 = vec![BFieldElement::new(2000), BFieldElement::new(0)];
    unsafe_list_push(list_pointer, elem_1.clone(), &mut memory, elem_1.len());

    let elem_2 = vec![BFieldElement::new(5000), BFieldElement::new(0)];
    unsafe_list_push(list_pointer, elem_2.clone(), &mut memory, elem_2.len());

    let elem_3 = vec![BFieldElement::new(4000), BFieldElement::new(0)];
    unsafe_list_push(list_pointer, elem_3.clone(), &mut memory, elem_3.len());

    unsafe_list_set_length(list_pointer, 2, &mut memory);

    let input_memory = HashMap::default();
    compare_prop_with_stack_and_memory(
        &simple_list_support(),
        inputs,
        outputs,
        input_memory,
        memory,
    );
}

#[test]
fn tuple_support_run_test() {
    let outputs = vec![ast::ExprLit::Bool(true), u32_lit(42), u64_lit(100)];

    compare_prop_with_stack(&tuple_support(), vec![], outputs);
}

#[test]
fn simple_list_support_test() {
    use tasm_lib::rust_shadowing_helper_functions::unsafe_list::unsafe_list_new;
    use tasm_lib::rust_shadowing_helper_functions::unsafe_list::unsafe_list_push;

    let mut memory = HashMap::default();
    let list_pointer = BFieldElement::one();
    unsafe_list_new(list_pointer, &mut memory);

    let elem_1 = vec![BFieldElement::new(2000), BFieldElement::new(0)];
    unsafe_list_push(list_pointer, elem_1.clone(), &mut memory, elem_1.len());

    let mut expected_final_memory = memory.clone();
    for i in 0..10 {
        let elem_i = vec![BFieldElement::new(i), BFieldElement::new(0)];
        unsafe_list_push(
            list_pointer,
            elem_i.clone(),
            &mut expected_final_memory,
            elem_i.len(),
        );
    }

    compare_prop_with_stack_and_memory(
        &mut_list_argument(),
        vec![bfe_lit(list_pointer)],
        vec![],
        memory,
        expected_final_memory,
    );
}
