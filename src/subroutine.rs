use std::fmt::Display;

use anyhow::bail;
use itertools::Itertools;
use tasm_lib::triton_vm::prelude::*;

use crate::libraries::LibraryFunction;

/// A `SubRoutine` is a list of instructions that always start with a label,
/// its name, and contains a `return` such that this code can be called
/// but is not itself executable, as the `return` would lead to a jump stack
/// underflow.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct SubRoutine(Vec<LabelledInstruction>);

impl Display for SubRoutine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().join("\n"))
    }
}

impl SubRoutine {
    /// Returns true iff subroutine starts with a label, ends with a return. Or
    /// if it ends with a recurse or a recurse_or_return, and contains a return.
    fn instructions_are_subroutine(instructions: &[LabelledInstruction]) -> bool {
        use LabelledInstruction::*;

        let is_instruction_or_label = |i: &&_| matches!(i, Instruction(_) | Label(_));
        let Some(Label(_)) = instructions.iter().find(is_instruction_or_label) else {
            return false;
        };

        let last_instruction = instructions.last().unwrap();

        let ends_with_return_or_recurse = *last_instruction == triton_instr!(return)
            || *last_instruction == triton_instr!(recurse)
            || *last_instruction == triton_instr!(recurse_or_return);
        let contains_return = instructions.iter().any(|x| *x == triton_instr!(return));
        ends_with_return_or_recurse && contains_return
    }

    /// Returns code, including label and any `return`s
    pub(crate) fn get_whole_function(&self) -> Vec<LabelledInstruction> {
        self.0.clone()
    }

    /// Returns true *iff* function starts with a label, ends with a return, contains only *one*
    /// return and no `recurse`. These conditions means that if can be inlined.
    pub(crate) fn can_be_inlined(&self) -> bool {
        let _label = self.get_label();

        let recurse_count = self
            .0
            .iter()
            .filter(|x| **x == triton_instr!(recurse))
            .count();
        let return_count = self
            .0
            .iter()
            .filter(|x| **x == triton_instr!(return))
            .count();

        let last_instruction = self.0.last().unwrap();

        *last_instruction == triton_instr!(return) && return_count == 1 && recurse_count == 0
    }

    /// Iff subroutine may be inlined, returns `Some(function body)`. Otherwise returns `None`
    pub(crate) fn get_function_body_for_inlining(&self) -> Option<Vec<LabelledInstruction>> {
        if !self.can_be_inlined() {
            None
        } else {
            Some(self.0[1..self.0.len() - 1].to_vec())
        }
    }

    pub(crate) fn get_label(&self) -> String {
        match self.0.first() {
            Some(LabelledInstruction::Label(label)) => label.to_owned(),
            _ => panic!(
                "Malformed subroutine constructed. First line must be a label. Instructions were:\n{}", self.0.iter().join("\n")
            ),
        }
    }
}

// Please do not add other constructors for SubRoutine that this, since we can then guarantee
// that all subroutines conform to the label/return/recurse rules.
impl TryFrom<Vec<LabelledInstruction>> for SubRoutine {
    type Error = anyhow::Error;

    fn try_from(instructions: Vec<LabelledInstruction>) -> Result<Self, Self::Error> {
        if SubRoutine::instructions_are_subroutine(&instructions) {
            Ok(SubRoutine(instructions))
        } else {
            bail!(
                "Cannot convert list of labelled instructions to subroutine as they do not conform. Instructions were:\n{}", instructions.iter().join("\n")
            );
        }
    }
}

impl TryFrom<LibraryFunction> for SubRoutine {
    type Error = anyhow::Error;

    fn try_from(compiled_function: LibraryFunction) -> Result<Self, Self::Error> {
        let instructions = triton_asm!(
            {compiled_function.signature.name}:
                {&compiled_function.body}
                return
        );

        instructions.try_into()
    }
}
