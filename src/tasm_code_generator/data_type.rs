use tasm_lib;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::op_stack::OpStackElement;
use triton_vm::triton_asm;

use crate::ast_types;
use crate::tasm_code_generator::CompilerState;

impl ast_types::DataType {
    /// Copy a value at a position on the stack to the top
    pub(super) fn dup_value_from_stack_code(
        &self,
        position: OpStackElement,
    ) -> Vec<LabelledInstruction> {
        let elem_size = self.stack_size();

        // the position of the deepest element of the value.
        let n: usize = Into::<usize>::into(position) + elem_size - 1;

        let instrs_as_str = format!("dup {}\n", n);
        let instrs_as_str = instrs_as_str.repeat(elem_size);

        triton_asm!({ instrs_as_str })
    }

    pub(super) fn compile_eq_code(&self, state: &mut CompilerState) -> Vec<LabelledInstruction> {
        use ast_types::DataType::*;
        match self {
            Bool | U32 | BFE | VoidPointer => triton_asm!(eq),
            U64 => triton_asm!(
                // _ a_hi a_lo b_hi b_lo
                swap 3
                eq
                swap 2
                eq
                mul
            ),
            U128 => triton_asm!(
                // _ a_3 a_2 a_1 a_0 b_3 b_2 b_1 b_0
                swap 5
                eq
                // _ a_3 a_2 b_0 a_0 b_3 b_2 (b_1 == a_1)
                swap 5
                eq
                // _ a_3 (b_1 == a_1) b_0 a_0 b_3 (b_2 == a_2)
                swap 5
                eq
                // _ (b_2 == a_2) (b_1 == a_1) b_0 a_0 (b_3 == a_3)
                swap 2
                eq
                // _ (b_2 == a_2) (b_1 == a_1) (b_3 == a_3) (b_0 == a_0)
                mul
                mul
                mul
                // _ (b_2 == a_2) * (b_1 == a_1) * (b_3 == a_3) * (b_0 == a_0)
            ),

            XFE => triton_asm!(
                 // _ a_2 a_1 a_0 b_2 b_1 b_0
                swap 4 // _ a_2 b_0 a_0 b_2 b_1 a_1
                eq     // _ a_2 b_0 a_0 b_2 (b_1 == a_1)
                swap 4 // _ (b_1 == a_1) b_0 a_0 b_2 a_2
                eq     // _ (b_1 == a_1) b_0 a_0 (b_2 == a_2)
                swap 2 // _ (b_1 == a_1) (b_2 == a_2) a_0 b_0
                eq     // _ (b_1 == a_1) (b_2 == a_2) (a_0 == b_0)
                mul    // _ (b_1 == a_1) ((b_2 == a_2)·(a_0 == b_0))
                mul    // _ ((b_1 == a_1)·(b_2 == a_2)·(a_0 == b_0))
            ),
            Digest => {
                let eq_digest =
                    state.import_snippet(Box::new(tasm_lib::hashing::eq_digest::EqDigest));
                triton_asm!(call { eq_digest })
            }
            List(_, _) => todo!(),
            Tuple(_) => todo!(),
            Array(_) => todo!("Equality for arrays not yet implemented"),
            Function(_) => todo!(),
            Struct(_) => todo!(),
            Boxed(_) => todo!("Comparison of MemPointer not supported yet"),
            Unresolved(name) => panic!("Cannot compare unresolved type {name}"),
            Reference(_) => panic!("Cannot compare references. Got {self}"),
            Enum(_) => todo!("Equality for enums not yet implemented"),
        }
    }
}
