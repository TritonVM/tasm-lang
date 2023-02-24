use super::{
    programs::{arithmetic::*, mmr::*, other::*},
    shared_test::graft_check_compile_prop,
};

#[test]
fn inferred_literals_test() {
    graft_check_compile_prop(&inferred_literals());
}

#[test]
fn nop_test() {
    graft_check_compile_prop(&nop_rast());
}

#[test]
fn add_u32_test() {
    graft_check_compile_prop(&add_u32_rast());
}

#[test]
fn add_u32_overwrite_rast_test() {
    graft_check_compile_prop(&add_u32_overwrite_rast());
}

#[test]
fn sub_u32_rast_1_test() {
    graft_check_compile_prop(&sub_u32_rast_1());
}

#[test]
fn sub_u32_rast_2_test() {
    graft_check_compile_prop(&sub_u32_rast_2());
}

#[test]
fn sub_u64_rast_1_test() {
    graft_check_compile_prop(&sub_u64_rast_1());
}

#[test]
fn sub_u64_rast_2_test() {
    graft_check_compile_prop(&sub_u64_rast_2());
}
#[test]
fn add_bfe_test() {
    graft_check_compile_prop(&add_bfe_rast());
}

#[test]
fn add_xfe_test() {
    graft_check_compile_prop(&add_xfe_rast());
}

#[test]
fn add_u64_test() {
    graft_check_compile_prop(&add_u64_rast());
}

#[test]
fn and_bool_test() {
    graft_check_compile_prop(&and_bool_rast());
}

#[test]
fn bitwise_and_u32_test() {
    graft_check_compile_prop(&bitwise_and_u32_rast());
}

#[test]
fn bitwise_and_u64_test() {
    graft_check_compile_prop(&bitwise_and_u64_rast());
}

#[test]
fn right_child_test() {
    graft_check_compile_prop(&right_child_rast());
}

#[test]
fn left_child_test() {
    graft_check_compile_prop(&left_child_rast());
}

#[test]
fn right_lineage_length_test() {
    graft_check_compile_prop(&right_lineage_length_stmt_rast());
    graft_check_compile_prop(&right_lineage_length_expr_rast());
}

#[test]
fn lt_u32_test() {
    graft_check_compile_prop(&lt_u32());
}

#[test]
fn simple_while_loop_test() {
    graft_check_compile_prop(&simple_while_loop());
}

#[test]
fn complicated_while_loop_test() {
    graft_check_compile_prop(&longer_while_loop());
}

#[test]
fn while_loop_with_declarations_test() {
    graft_check_compile_prop(&while_loop_with_declarations());
}

#[test]
fn code_block_test() {
    graft_check_compile_prop(&code_block());
}

#[test]
fn simple_list_support_test() {
    graft_check_compile_prop(&simple_list_support());
}

#[test]
fn tuple_support_test() {
    graft_check_compile_prop(&tuple_support());
}

#[test]
fn mut_list_argument_test() {
    graft_check_compile_prop(&mut_list_argument());
}

#[should_panic]
#[test]
fn missing_mut_keyword_test() {
    graft_check_compile_prop(&missing_mut_keyword());
}
