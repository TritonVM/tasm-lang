use triton_vm::instruction::LabelledInstruction;

pub(crate) struct CompiledTasm {
    code: Vec<LabelledInstruction>,
}

impl From<Vec<LabelledInstruction>> for CompiledTasm {
    fn from(value: Vec<LabelledInstruction>) -> Self {
        Self { code: value }
    }
}

impl CompiledTasm {
    pub fn remove_own_name(&self) -> Vec<LabelledInstruction> {
        assert!(
            matches!(self.code.first().unwrap(), LabelledInstruction::Label(_)),
            "1st line must be a label when removing it"
        );
        self.code[1..].to_vec()
    }

    pub fn get_code(&self) -> Vec<LabelledInstruction> {
        self.code.clone()
    }
}
