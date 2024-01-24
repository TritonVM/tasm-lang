use std::collections::HashSet;

use itertools::Itertools;
use num::One;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;

use crate::ast::MatchCondition;
use crate::ast::MatchExpr;
use crate::ast::MatchStmt;
use crate::ast_types;
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
        compile_catch_all_predicate_inner(
            &match_conditions,
            self.match_expression.get_type().as_enum_type(),
        )
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
        compile_catch_all_predicate_inner(
            &match_conditions,
            self.match_expression.get_type().as_enum_type(),
        )
    }
}

/// Generate the code for the predicate determining if a catch-all branch should be taken.
/// ```text
/// BEFORE: _ discriminant
/// AFTER: _ take_catch_all_branch
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
        let only_explicit_discriminant = discriminants_explicitly.iter().next().unwrap();
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
            // _ actual_discriminant take_catch_all_branch

            dup 1
            push {discriminant_covered}
            eq
            // _ actual_discriminant take_catch_all_branch (actual_discriminant == discriminant_covered)

            add
            // _ actual_discriminant take_catch_all_branch'
        ));
    }

    code.extend(triton_asm!(
        // _ actual_discriminant take_catch_all_branch
        swap 1
        pop 1

        // _ take_catch_all_branch
    ));

    code
}
