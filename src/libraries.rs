use triton_opcodes::instruction::LabelledInstruction;

use crate::ast::FnSignature;

pub mod bfe;
pub mod tasm;
pub mod unsigned_integers;
pub mod vector;
pub mod xfe;

pub struct CompiledFunction {
    signature: FnSignature,
    body: Vec<LabelledInstruction>,
}
