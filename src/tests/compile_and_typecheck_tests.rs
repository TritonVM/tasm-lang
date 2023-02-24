use super::{programs::*, shared_test::graft_check_compile_prop};

#[test]
fn inferred_literals_test() {
    graft_check_compile_prop(&other::inferred_literals());
}

#[test]
fn nop_test() {
    graft_check_compile_prop(&other::nop_rast());
}

#[test]
fn add_u32_test() {
    graft_check_compile_prop(&arithmetic::add_u32_rast());
}

#[test]
fn add_u32_overwrite_rast_test() {
    graft_check_compile_prop(&arithmetic::add_u32_overwrite_rast());
}

#[test]
fn sub_u32_rast_1_test() {
    graft_check_compile_prop(&arithmetic::sub_u32_rast_1());
}

#[test]
fn sub_u32_rast_2_test() {
    graft_check_compile_prop(&arithmetic::sub_u32_rast_2());
}

#[test]
fn sub_u64_rast_1_test() {
    graft_check_compile_prop(&arithmetic::sub_u64_rast_1());
}

#[test]
fn sub_u64_rast_2_test() {
    graft_check_compile_prop(&arithmetic::sub_u64_rast_2());
}
#[test]
fn add_bfe_test() {
    graft_check_compile_prop(&arithmetic::add_bfe_rast());
}

#[test]
fn add_xfe_test() {
    graft_check_compile_prop(&arithmetic::add_xfe_rast());
}

#[test]
fn add_u64_test() {
    graft_check_compile_prop(&arithmetic::add_u64_rast());
}

#[test]
fn and_bool_test() {
    graft_check_compile_prop(&arithmetic::and_bool_rast());
}

#[test]
fn bitwise_and_u32_test() {
    graft_check_compile_prop(&arithmetic::bitwise_and_u32_rast());
}

#[test]
fn bitwise_and_u64_test() {
    graft_check_compile_prop(&arithmetic::bitwise_and_u64_rast());
}

#[test]
fn right_child_test() {
    graft_check_compile_prop(&mmr::right_child_rast());
}

#[test]
fn left_child_test() {
    graft_check_compile_prop(&mmr::left_child_rast());
}

#[test]
fn right_lineage_length_test() {
    graft_check_compile_prop(&mmr::right_lineage_length_stmt_rast());
    graft_check_compile_prop(&mmr::right_lineage_length_expr_rast());
}

#[test]
fn leftmost_ancestor_test() {
    graft_check_compile_prop(&mmr::leftmost_ancestor_rast());
}

#[test]
fn lt_u32_test() {
    graft_check_compile_prop(&arithmetic::lt_u32());
}

#[test]
fn simple_while_loop_test() {
    graft_check_compile_prop(&other::simple_while_loop());
}

#[test]
fn complicated_while_loop_test() {
    graft_check_compile_prop(&other::longer_while_loop());
}

#[test]
fn while_loop_with_declarations_test() {
    graft_check_compile_prop(&other::while_loop_with_declarations());
}

#[test]
fn code_block_test() {
    graft_check_compile_prop(&other::code_block());
}

#[test]
fn simple_list_support_test() {
    graft_check_compile_prop(&other::simple_list_support());
}

#[test]
fn tuple_support_test() {
    graft_check_compile_prop(&other::tuple_support());
}

#[test]
fn mut_list_argument_test() {
    graft_check_compile_prop(&other::mut_list_argument());
}

#[should_panic]
#[test]
fn missing_mut_keyword_test() {
    graft_check_compile_prop(&other::missing_mut_keyword());
}
