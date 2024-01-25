use itertools::Itertools;
use num::One;
use std::collections::HashSet;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;

use super::compile_block_stmt;
use super::compile_returning_block_expr;
use super::pop_n;
use super::CompilerState;
use super::ValueIdentifier;
use crate::ast;
use crate::ast::MatchCondition;
use crate::ast::MatchExpr;
use crate::ast::MatchStmt;
use crate::ast_types;
use crate::type_checker;
use crate::type_checker::GetType;
use crate::type_checker::Typing;

impl MatchExpr<Typing> {
    /// Return the code that evaluates to true iff the catch-all branch in a match statement
    /// should be taken.
    /// ```text
    /// BEFORE: _ discriminant
    /// AFTER: _ take_catch_all_branch
    /// ```
    pub(crate) fn compile_catch_all_predicate(&self) -> Vec<LabelledInstruction> {
        let match_conditions: Vec<_> = self
            .arms
            .iter()
            .map(|x| &x.match_condition)
            .cloned()
            .collect_vec();
        let enum_type = match self.match_expression.get_type() {
            ast_types::DataType::Boxed(inner) => inner.as_enum_type(),
            other => other.as_enum_type(),
        };
        compile_catch_all_predicate_inner(&match_conditions, enum_type)
    }
}

impl MatchStmt<Typing> {
    /// Return the code that evaluates to true iff the catch-all branch in a match statement
    /// should be taken.
    /// ```text
    /// BEFORE: _ discriminant
    /// AFTER: _ take_catch_all_branch
    /// ```
    pub(crate) fn compile_catch_all_predicate(&self) -> Vec<LabelledInstruction> {
        let match_conditions: Vec<_> = self
            .arms
            .iter()
            .map(|x| &x.match_condition)
            .cloned()
            .collect_vec();
        let enum_type = match self.match_expression.get_type() {
            ast_types::DataType::Boxed(inner) => inner.as_enum_type(),
            other => other.as_enum_type(),
        };
        compile_catch_all_predicate_inner(&match_conditions, enum_type)
    }
}

/// Generate the code for the predicate determining if a catch-all branch should be taken.
/// ```text
/// BEFORE: _ discriminant
/// AFTER:  _ take_catch_all_branch
/// ```
fn compile_catch_all_predicate_inner(
    match_conditions: &[MatchCondition],
    enum_type: ast_types::EnumType,
) -> Vec<LabelledInstruction> {
    assert!(match_conditions
        .iter()
        .any(|x| matches!(x, MatchCondition::CatchAll)));

    let all_discriminants = enum_type.all_discriminants();
    let discriminants_explicitly: HashSet<_> = match_conditions
        .iter()
        .filter_map(|x| {
            if let MatchCondition::EnumVariant(enum_variant) = x {
                Some(enum_variant)
            } else {
                None
            }
        })
        .map(|x| enum_type.variant_discriminant(&x.variant_name))
        .collect();

    // Special-case on the common pattern of *two* match arms, where
    // the latter is a catch-all
    if discriminants_explicitly.len().is_one() {
        let only_explicit_discriminant = *discriminants_explicitly.iter().next().unwrap();
        return triton_asm!(
            // _ discriminant

            push {only_explicit_discriminant}
            eq
            // _ (discriminant == discriminant_covered_explicitly)

            push 0
            eq
            // _ (discriminant != discriminant_covered_explicitly)
        );
    }

    let mut discriminants_covered_by_catch_all = all_discriminants
        .difference(&discriminants_explicitly)
        .collect_vec();

    // Sort discriminants list to ensure that generated code is deterministic
    discriminants_covered_by_catch_all.sort_unstable();

    let mut code = triton_asm!(push 0);
    for discriminant_covered in discriminants_covered_by_catch_all {
        code.extend(triton_asm!(
            // _ discriminant take_catch_all_branch

            dup 1
            push {discriminant_covered}
            eq
            // _ discriminant take_catch_all_branch (discriminant == discriminant_covered)

            add
            // _ discriminant take_catch_all_branch'
        ));
    }

    code.extend(triton_asm!(
        // _ discriminant take_catch_all_branch
        swap 1
        pop 1

        // _ take_catch_all_branch
    ));

    code
}

/// Compile a match-statement where the matched-against value lives on the stack.
/// ```text
/// BEFORE: _ [match_expression]
/// AFTER: _ [match_expression]
/// ```
pub(super) fn compile_match_stmt_stack_expr(
    match_stmt: &ast::MatchStmt<type_checker::Typing>,
    state: &mut CompilerState,
    match_expr_id: &ValueIdentifier,
) -> Vec<LabelledInstruction> {
    // Notice that
    // stack: _ [match_expression]
    // is equivalent to
    // stack: _ [payload] discriminant
    // and
    // stack: _ [variant_data] [variant_padding] discriminant
    let mut match_code = triton_asm!();

    let match_expression_enum_type = match_stmt.match_expression.get_type().as_enum_type();

    let outer_vstack = state.function_state.vstack.clone();
    let outer_bindings = state.function_state.var_addr.clone();
    let match_expr_discriminant = triton_asm!(dup 0);
    for (arm_counter, arm) in match_stmt.arms.iter().enumerate() {
        // At start of each loop-iteration, stack is:
        // stack: _ [payload] discriminant

        let arm_subroutine_label = format!("{match_expr_id}_body_{arm_counter}");

        match &arm.match_condition {
            ast::MatchCondition::EnumVariant(enum_variant_selector) => {
                // We know that variant discriminant is on top
                let arm_variant_discriminant = match_expression_enum_type
                    .variant_discriminant(&enum_variant_selector.variant_name);
                match_code.extend(triton_asm!(
                    {&match_expr_discriminant}
                    // _ [payload] discriminant

                    push {arm_variant_discriminant}
                    // _ [payload] discriminant needle_discriminant

                    eq
                    skiz
                    call {arm_subroutine_label}
                    // _ [payload] discriminant
                ));

                // Split compiler's view of evaluated expression from
                // _ [match_expr]
                // into
                // _ [variant_data] [padding] discriminant
                let new_ids = state.split_value(
                    match_expr_id,
                    match_expression_enum_type
                        .decompose_variant(&enum_variant_selector.variant_name),
                );

                // Insert bindings from pattern-match into stack view for arm-body
                enum_variant_selector
                    .data_bindings
                    .iter()
                    .zip(new_ids.iter())
                    .for_each(|(binding, new_id)| {
                        state
                            .function_state
                            .var_addr
                            .insert(binding.name.to_owned(), new_id.clone());
                    });

                let body_code = compile_block_stmt(&arm.body, state);

                // This arm-body changes the `arm_taken` bool but otherwise leaves the stack unchanged
                let subroutine_code = triton_asm!(
                    {arm_subroutine_label}:
                        // stack: _ [variant_data] [padding] discriminant

                        {&body_code}
                        // stack: _ [variant_data] [padding] discriminant

                        return
                );

                state
                    .function_state
                    .subroutines
                    .push(subroutine_code.try_into().unwrap());
            }
            ast::MatchCondition::CatchAll => {
                let predicate = match_stmt.compile_catch_all_predicate();
                match_code.append(&mut triton_asm!(
                    // _ [match_expr]

                    {&match_expr_discriminant}
                    // _ [match_expr] match_expr_discriminant

                    {&predicate}
                    // _ [match_expr] take_catch_all_branch

                    skiz
                    call {arm_subroutine_label}
                    // _ [match_expr]
                ));

                let body_code = compile_block_stmt(&arm.body, state);
                let subroutine_code = triton_asm!(
                    {arm_subroutine_label}:
                        {&body_code}
                        return
                );
                state
                    .function_state
                    .subroutines
                    .push(subroutine_code.try_into().unwrap());
            }
        }

        // Restore stack view and bindings view for next loop-iteration
        state
            .function_state
            .restore_stack_and_bindings(&outer_vstack, &outer_bindings);
    }

    match_code
}

/// Compile a match-statement where the matched-against value lives in memory
/// ```text
/// BEFORE: _ *match_expression
/// AFTER:  _ *match_expression
/// ```
pub(super) fn compile_match_stmt_boxed_expr(
    match_stmt: &ast::MatchStmt<type_checker::Typing>,
    state: &mut CompilerState,
    match_expr_id: &ValueIdentifier,
) -> Vec<LabelledInstruction> {
    // Notice that `*match_expression`` is equivalent to `*discriminant`
    let match_expr_discriminant = triton_asm!(
        // _ *match_expr
        read_mem 1 push 1 add
        // _ discriminant *match_expr

        swap 1
        // _ *match_expr discriminant
    );

    // Get enum_type
    let match_expression_enum_type = match_stmt
        .match_expression
        .get_type()
        .unbox()
        .as_enum_type();
    let outer_vstack = state.function_state.vstack.clone();
    let outer_bindings = state.function_state.var_addr.clone();
    let mut match_code = vec![];
    for (arm_counter, arm) in match_stmt.arms.iter().enumerate() {
        // At start of each loop-iteration, stack is:
        // stack: _ *match_expression

        let arm_subroutine_label = format!("{match_expr_id}_body_{arm_counter}");

        match &arm.match_condition {
            ast::MatchCondition::EnumVariant(enum_variant_selector) => {
                let arm_variant_discriminant = match_expression_enum_type
                    .variant_discriminant(&enum_variant_selector.variant_name);

                match_code.extend(triton_asm!(
                    // _ *match_expr

                    {&match_expr_discriminant}
                    // _ *match_expr discriminant

                    push {arm_variant_discriminant}
                    // _ *match_expr discriminant needle_discriminant

                    eq
                    // _ *match_expr (discriminant == needle_discriminant)

                    skiz
                    call {arm_subroutine_label}
                    // _ *match_expr
                ));
            }
            ast::MatchCondition::CatchAll => {
                let predicate = match_stmt.compile_catch_all_predicate();
                match_code.extend(triton_asm!(
                    // _ *match_expr

                    {&match_expr_discriminant}
                    // _ *match_expr discriminant

                    {&predicate}
                    // _ *match_expr take_catch_all_branch

                    skiz
                    call {arm_subroutine_label}
                    // _ *match_expr
                ));
            }
        }

        arm.match_condition
            .declarations_and_their_types(&match_expression_enum_type)
            .for_each(|decl| state.add_boxed_match_arm_binding_to_scope(decl));

        let label_for_bindings_subroutine = match_expression_enum_type
            .get_variant_data_fields_in_memory(&arm.match_condition, state);

        let body_code = compile_block_stmt(&arm.body, state);

        let pop_local_bindings = match &arm.match_condition {
            MatchCondition::CatchAll => vec![],
            MatchCondition::EnumVariant(selector) => pop_n(selector.data_bindings.len()),
        };

        let subroutine_code = triton_asm!(
            {arm_subroutine_label}:
                // _ *discriminant

                call {label_for_bindings_subroutine}
                // _ *match_expr [*variant-data-fields]

                {&body_code}
                // _ *match_expr [*variant-data-fields]

                // We can just pop local binding from top of stack, since a statement cannot return anything
                {&pop_local_bindings}
                // _ *match_expr

                return
        );

        state
            .function_state
            .subroutines
            .push(subroutine_code.try_into().unwrap());

        // Restore stack view and bindings view for next loop-iteration
        state
            .function_state
            .restore_stack_and_bindings(&outer_vstack, &outer_bindings);
    }

    match_code
}

/// Compile a match-statement where the matched-against value lives on the stack
/// ```text
/// BEFORE: _ [expression_payload] expression_discriminant
/// AFTER: _ [result]
/// ```
pub(super) fn compile_match_expr_stack_value(
    match_expr: &ast::MatchExpr<type_checker::Typing>,
    state: &mut CompilerState,
    match_expr_id: &ValueIdentifier,
) -> Vec<LabelledInstruction> {
    let match_expression_enum_type = match_expr.match_expression.get_type().as_enum_type();

    let outer_vstack = state.function_state.vstack.clone();
    let outer_bindings = state.function_state.var_addr.clone();
    let result_size = match_expr.get_type().stack_size();
    let dup_discriminant_to_top = triton_asm!(dup { result_size });

    let mut match_code = triton_asm!(dup 0);
    for (arm_counter, arm) in match_expr.arms.iter().enumerate() {
        // At start of each loop-iteration, stack is:
        // stack: _ [variant_payload] discriminant <[maybe_result]> discriminant

        let arm_subroutine_label = format!("{match_expr_id}_body_{arm_counter}");

        match &arm.match_condition {
            ast::MatchCondition::EnumVariant(enum_variant_selector) => {
                let arm_variant_discriminant = match_expression_enum_type
                    .variant_discriminant(&enum_variant_selector.variant_name);

                match_code.extend(triton_asm!(
                    // _ [variant_payload] discriminant <[maybe_result]> discriminant

                    dup 0
                    push {arm_variant_discriminant}
                    // _ [variant_payload] discriminant <[maybe_result]> discriminant discriminant needle_discriminant

                    eq
                    skiz
                    // _ [variant_payload] discriminant <[maybe_result]> discriminant (discriminant == needle_discriminant)

                    call {arm_subroutine_label}
                    // _ [variant_payload] discriminant <[maybe_result]> discriminant
                ));

                // Split compiler's view of evaluated expression from
                // _ [enum_value]
                // into
                // _ [enum_data] [padding] discriminant
                let new_ids = state.split_value(
                    match_expr_id,
                    match_expression_enum_type
                        .decompose_variant(&enum_variant_selector.variant_name),
                );

                // Insert bindings from pattern-match into stack view for arm-body
                enum_variant_selector
                    .data_bindings
                    .iter()
                    .zip(new_ids.iter())
                    .for_each(|(binding, new_id)| {
                        state
                            .function_state
                            .var_addr
                            .insert(binding.name.to_owned(), new_id.clone());
                    });
            }
            ast::MatchCondition::CatchAll => {
                let catch_all_predicate = match_expr.compile_catch_all_predicate();

                // Statically compile a function taking discriminant as input
                // and returns a bool indicating if the wild-card match arm
                // should execute.
                // This function can be compiled from a `MatchExpr` value.
                match_code.extend(triton_asm!(
                    // _ [variant_payload] discriminant <[maybe_result]> discriminant

                    dup 0
                    {&catch_all_predicate}
                    // _ [variant_payload] discriminant <[maybe_result]> discriminant take_catch_all_branch

                    skiz
                    call {arm_subroutine_label}
                ));
            }
        }

        let (_, body_code) = compile_returning_block_expr("arm-body", state, &arm.body);

        let subroutine_code = triton_asm!(
            {arm_subroutine_label}:
                // _ [variant_payload] discriminant discriminant

                pop 1
                // _ [variant_payload] discriminant
                // _ [enum_data] [padding] discriminant

                {&body_code}
                // _ [enum_data] [padding] discriminant [result]

                {&dup_discriminant_to_top}
                // _ [enum_data] [padding] discriminant [result] discriminant

                return
        );

        state
            .function_state
            .subroutines
            .push(subroutine_code.try_into().unwrap());

        // Restore stack view and bindings view for next loop-iteration
        state
            .function_state
            .restore_stack_and_bindings(&outer_vstack, &outer_bindings);
    }

    match_code.extend(triton_asm!(
        // _ [variant_payload] discriminant [result] discriminant

        pop 1
        // _ [variant_payload] discriminant [result]
    ));

    match_code
}

/// Compile a match-statement where the matched-against value lives, boxed, in memory
/// ```text
/// BEFORE: _ *match_expression
/// AFTER:  _ [result]
/// ```
pub(super) fn compile_match_expr_boxed_value(
    match_expr: &ast::MatchExpr<type_checker::Typing>,
    state: &mut CompilerState,
    match_expr_id: &ValueIdentifier,
) -> Vec<LabelledInstruction> {
    // Notice that `*match_expression`` is equivalent to `*discriminant`

    let match_expression_enum_type = match_expr
        .match_expression
        .get_type()
        .unbox()
        .as_enum_type();

    let outer_vstack = state.function_state.vstack.clone();
    state.show_vstack_values();
    let mut vstack_without_match_expression_pointer = outer_vstack.clone();
    vstack_without_match_expression_pointer.pop().unwrap();
    let height_of_stack_that_must_not_change =
        vstack_without_match_expression_pointer.get_stack_height();

    let outer_bindings = state.function_state.var_addr.clone();
    let return_type = match_expr.get_type();
    let match_expression_pointer_pointer = state.global_compiler_state.snippet_state.kmalloc(1);

    let get_discriminant = triton_asm!(
      // _
      push {match_expression_pointer_pointer}
      read_mem 1
      pop 1
      // _ *discriminant

      read_mem 1
      pop 1
      hint discriminant = stack[0]
      // _ discriminant
    );

    let mut match_code = triton_asm!(
        // _ *match_expression

        push {match_expression_pointer_pointer}
        // _ *match_expression **match_expression

        write_mem 1
        pop 1
        // _
    );

    for (arm_counter, arm) in match_expr.arms.iter().enumerate() {
        // INVARIANT: _ <[maybe_result]>
        let arm_subroutine_label = format!("{match_expr_id}_body_{arm_counter}");

        match &arm.match_condition {
            ast::MatchCondition::EnumVariant(selector) => {
                let arm_variant_discriminant =
                    match_expression_enum_type.variant_discriminant(&selector.variant_name);

                match_code.extend(triton_asm!(
                    // _ <[maybe_result]>

                    {&get_discriminant}
                    // _ <[maybe_result]> discriminant

                    push {arm_variant_discriminant}
                    // _ <[maybe_result]> discriminant arm_variant_discriminant

                    eq
                    skiz
                    // _ <[maybe_result]>

                    call {arm_subroutine_label}
                    // _ <[maybe_result']>
                ));
            }
            MatchCondition::CatchAll => {
                let catch_all_predicate = match_expr.compile_catch_all_predicate();
                match_code.extend(triton_asm!(
                    // _ <[maybe_result]>

                    {&get_discriminant}
                    // _ <[maybe_result]> discriminant

                    {&catch_all_predicate}
                    // _ <[maybe_result]> take_catch_all_branch

                    skiz
                    call {arm_subroutine_label}
                    // _ <[maybe_result']>
                ));
            }
        }

        arm.match_condition
            .declarations_and_their_types(&match_expression_enum_type)
            .for_each(|decl| state.add_boxed_match_arm_binding_to_scope(decl));

        let label_for_bindings_subroutine = match_expression_enum_type
            .get_variant_data_fields_in_memory(&arm.match_condition, state);

        let (_, body_code) = compile_returning_block_expr("arm-body", state, &arm.body);
        state.new_value_identifier("match_expr_body_res", &return_type);

        let leave_only_result =
            state.clear_all_but_top_stack_value_above_height(height_of_stack_that_must_not_change);
        let return_type_size = return_type.stack_size();
        let result_hint = format!(
            "hint match_arm_result: {} = stack[0..{return_type_size}]",
            return_type.label_friendly_name()
        );

        let subroutine_code = triton_asm!(
            {arm_subroutine_label}:
                // _

                push {match_expression_pointer_pointer}
                read_mem 1
                pop 1
                // _ *match_expression

                call {label_for_bindings_subroutine}
                // _ *match_expression [*variant-data-fields]

                {&body_code}
                // _ *match_expression [*variant-data-fields] [result]

                {&leave_only_result}
                {result_hint}
                // _ [result]

                return
        );

        state
            .function_state
            .subroutines
            .push(subroutine_code.try_into().unwrap());

        // Restore stack view and bindings view for next loop-iteration
        state
            .function_state
            .restore_stack_and_bindings(&outer_vstack, &outer_bindings);
    }

    // remove input argument `*match_expression` from compiler's view of stack
    state.function_state.vstack.pop();

    match_code
}
