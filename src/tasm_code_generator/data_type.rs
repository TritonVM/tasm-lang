use num::One;
use tasm_lib;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::op_stack::OpStackElement;
use triton_vm::triton_instr;
use triton_vm::{triton_asm, BFieldElement};

use crate::ast_types;
use crate::tasm_code_generator::CompilerState;

pub mod enum_type;
pub mod struct_type;

impl ast_types::DataType {
    fn copy_words_from_memory(
        static_memory_address: Option<BFieldElement>,
        value_size: usize,
    ) -> Vec<LabelledInstruction> {
        // A stack value of the form `_ val2 val1 val0`, with `val0` being on the top of the stack
        // is stored in memory as: `val0 val1 val2`, where `val0` is stored on the `memory_location`
        // address. So we read the value at the highest memory location first.
        // TODO: Consider making subroutines out of this in
        // order to get shorter programs.
        let mut ret = match static_memory_address {
            Some(mem_location) => {
                triton_asm!(push {mem_location.value() + value_size as u64 - 1})
            }
            None => {
                if value_size.is_one() {
                    triton_asm!()
                } else {
                    triton_asm!(push {value_size as u64 - 1} add)
                }
            }
        };

        // stack: _ memory_address_of_last_word

        for i in 0..value_size {
            // Stack: _ memory_address

            ret.push(triton_instr!(read_mem));
            // Stack: _ memory_address value

            ret.push(triton_instr!(swap 1));

            // Decrement memory address to prepare for next loop iteration
            if i != value_size - 1 {
                ret.append(&mut triton_asm!(push {BFieldElement::MAX} add))
                // Stack: _ (memory_address - 1)
            }
        }

        // Remove memory address from top of stack
        ret.push(triton_instr!(pop));

        // Stack: _ element_N element_{N - 1} ... element_0

        ret
    }

    /// Return the code to copy a value in memory to the stack.
    /// `position` refers to a memory address. If this is `None`, the
    /// memory address is assumed to be on top of the stack.
    /// BEFORE: _ <*value>
    /// AFTER: _ [value]
    pub(super) fn copy_from_memory(
        &self,
        static_address: Option<BFieldElement>,
    ) -> Vec<LabelledInstruction> {
        match self {
            ast_types::DataType::Bool
            | ast_types::DataType::U32
            | ast_types::DataType::U64
            | ast_types::DataType::U128
            | ast_types::DataType::BFE
            | ast_types::DataType::XFE
            | ast_types::DataType::Digest => {
                Self::copy_words_from_memory(static_address, self.stack_size())
            }
            ast_types::DataType::List(_, _) => Self::copy_words_from_memory(static_address, 1),
            ast_types::DataType::Tuple(_) => todo!(),
            ast_types::DataType::Array(_) => todo!(),
            ast_types::DataType::Struct(_) => todo!(),
            ast_types::DataType::Enum(_) => todo!(),
            ast_types::DataType::VoidPointer => todo!(),
            ast_types::DataType::Function(_) => todo!(),
            ast_types::DataType::Boxed(_) => todo!(),
            ast_types::DataType::Reference(_) => todo!(),
            ast_types::DataType::Unresolved(_) => todo!(),
        }
    }

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

    /// Return the code for evaluating equality expression
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
