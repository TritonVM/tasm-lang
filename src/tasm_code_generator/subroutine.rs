use anyhow::bail;
use itertools::Itertools;
use triton_vm::{instruction::LabelledInstruction, triton_asm, triton_instr};

use crate::libraries::LibraryFunction;

/// A `SubRoutine` is a list of instructions that always start with a label,
/// its name, and contains a `return` such that this code can be called
/// but is not itself executable, as the `return` would lead to a jump stack
/// underflow.
#[derive(Clone, Debug)]
pub(crate) struct SubRoutine(Vec<LabelledInstruction>);

impl SubRoutine {
    /// Returns true iff subroutine starts with a label, ends with a return or a
    /// recurse, and contains a return.
    fn instructions_are_subroutine(instructions: &[LabelledInstruction]) -> bool {
        match instructions.first() {
            Some(LabelledInstruction::Label(_subroutine_label)) => (),
            _ => return false,
        };

        let last_instruction = instructions.last().unwrap();

        let ends_with_return_or_recurse = *last_instruction == triton_instr!(return)
            || *last_instruction == triton_instr!(recurse);
        let contains_return = instructions.iter().any(|x| *x == triton_instr!(return));
        ends_with_return_or_recurse && contains_return
    }

    pub(crate) fn get_code(&self) -> Vec<LabelledInstruction> {
        self.0.clone()
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

        if SubRoutine::instructions_are_subroutine(&instructions) {
            Ok(SubRoutine(instructions))
        } else {
            bail!(
                "Cannot convert list of labelled instructions to subroutine as they do not conform. Instructions were:\n{}", instructions.iter().join("\n")
            );
        }
    }
}
