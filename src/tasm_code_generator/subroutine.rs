use triton_vm::{instruction::LabelledInstruction, triton_instr};

#[derive(Clone, Debug)]
pub(crate) struct SubRoutine(Vec<LabelledInstruction>);

impl SubRoutine {
    /// Returns true iff subroutine starts with a label, ends with a return or a
    /// recurse, and contains a return.
    pub(crate) fn has_valid_structure(&self) -> bool {
        match self.0.first() {
            Some(LabelledInstruction::Label(_subroutine_label)) => (),
            _ => return false,
        };

        let last_instruction = self.0.last().unwrap();

        let ends_with_return_or_recurse = *last_instruction == triton_instr!(return)
            || *last_instruction == triton_instr!(recurse);
        let contains_return = self.0.iter().any(|x| *x == triton_instr!(return));
        ends_with_return_or_recurse && contains_return
    }

    pub(crate) fn get_code(&self) -> Vec<LabelledInstruction> {
        self.0.clone()
    }
}

impl From<Vec<LabelledInstruction>> for SubRoutine {
    fn from(value: Vec<LabelledInstruction>) -> Self {
        Self(value)
    }
}
