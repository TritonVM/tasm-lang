use super::ValueIdentifier;
use crate::ast_types;
use crate::tasm_code_generator::InnerFunctionTasmCode;
use crate::tasm_code_generator::SubRoutine;

use itertools::Itertools;
use std::collections::HashMap;
use std::collections::HashSet;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;
use triton_vm::triton_instr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stack<T: Eq> {
    pub inner: Vec<T>,
}

impl<T: Eq> Default for Stack<T> {
    fn default() -> Self {
        let inner = Vec::default();
        Self { inner }
    }
}

impl<T: Eq> Stack<T> {
    pub fn push(&mut self, elem: T) {
        self.inner.push(elem);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.inner.pop()
    }

    pub fn peek(&self) -> Option<&T> {
        self.inner.last()
    }

    pub fn replace_value(&mut self, seek_value: &T, new_value: T) {
        let index_to_replace = self
            .inner
            .iter()
            .find_position(|found_value| seek_value == *found_value)
            .expect("Value must be present when removing from stack")
            .0;

        self.inner[index_to_replace] = new_value;
    }

    pub(crate) fn insert_at(&mut self, index: usize, value: T) {
        self.inner.insert(index, value);
    }

    // /// Remove an element from stack. Returns the index of the value that was removed.
    // pub fn remove_value(&mut self, seek_value: &T) -> usize {
    //     let index_to_remove = self
    //         .inner
    //         .iter()
    //         .find_position(|found_value| seek_value == *found_value)
    //         .expect("Value must be present when removing from stack")
    //         .0;

    //     self.inner.remove(index_to_remove);

    //     index_to_remove
    // }
}

// the compiler's view of the stack, including information about whether value has been spilled to memory
pub type VStack = Stack<(ValueIdentifier, (ast_types::DataType, Option<u32>))>;

impl VStack {
    /// Returns (stack_position, data_type, maybe_memory_location) where `stack_position` is the top of
    /// the value on the stack, i.e. the most shallow part of the value. Top of stack has index 0.
    /// May return a depth that exceeds the addressable space (16 elements) in which case spilling
    /// may be required.
    pub(crate) fn find_stack_value(
        &self,
        seek_addr: &ValueIdentifier,
    ) -> (usize, ast_types::DataType, Option<u32>) {
        let mut position: usize = 0;
        for (_i, (found_addr, (data_type, spilled))) in self.inner.iter().rev().enumerate() {
            if seek_addr == found_addr {
                return (position, data_type.to_owned(), spilled.to_owned());
            }

            position += data_type.stack_size();

            // By asserting after `+= data_type.size_of()`, we check that the deepest part
            // of the sought value is addressable, not just the top part of the value.
        }

        panic!("Cannot find {seek_addr} on vstack")
    }

    pub(crate) fn remove_by_id(&mut self, seek_addr: &ValueIdentifier) -> usize {
        let index_to_remove = self
            .inner
            .iter()
            .find_position(|&found_value| *seek_addr == found_value.0)
            .expect("Value must be present when removing from stack")
            .0;

        self.inner.remove(index_to_remove);

        index_to_remove
    }

    pub(crate) fn get_stack_height(&self) -> usize {
        self.inner
            .iter()
            .map(|(_, (data_type, _))| data_type.stack_size())
            .sum()
    }

    pub(crate) fn get_code_to_spill_to_memory(
        &self,
        values_to_spill: &[ValueIdentifier],
    ) -> Vec<LabelledInstruction> {
        let mut code = vec![];
        for value_to_spill in values_to_spill {
            let (stack_position, data_type, memory_spill_address) =
                self.find_stack_value(value_to_spill);
            let memory_spill_address = memory_spill_address.unwrap();
            let top_of_value = stack_position;
            let mut spill_code = Self::copy_value_to_memory(
                memory_spill_address,
                data_type.stack_size(),
                top_of_value,
            );
            code.append(&mut spill_code);
        }

        code
    }

    /// Return the code to store a stack-value in memory
    pub(crate) fn copy_value_to_memory(
        memory_location: u32,
        value_size: usize,
        stack_location_for_top_of_value: usize,
    ) -> Vec<LabelledInstruction> {
        // A stack value of the form `_ val2 val1 val0`, with `val0` being on the top of the stack
        // is stored in memory as: `val0 val1 val2`, where `val0` is stored on the `memory_location`
        // address.
        let mut ret = triton_asm!(push {memory_location as u64});

        for i in 0..value_size {
            // ret.push(dup(1 + i as u64 + stack_location_for_top_of_value as u64));
            ret.append(
                &mut triton_asm!(dup {1 + i as u64 + stack_location_for_top_of_value as u64}),
            );
            // _ [elements] mem_address element

            ret.push(triton_instr!(write_mem));
            // _ [elements] mem_address

            if i != value_size - 1 {
                ret.append(&mut triton_asm!(push 1 add));
                // _ (mem_address + 1)
            }
        }

        // remove memory address from top of stack
        ret.push(triton_instr!(pop));

        ret
    }
}
