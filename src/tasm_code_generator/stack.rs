use itertools::Itertools;
use tasm_lib::triton_vm::prelude::*;

use super::copy_value_to_memory;
use super::ValueIdentifier;
use crate::ast_types;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Stack<T: Eq> {
    pub(crate) inner: Vec<T>,
}

impl<T: Eq> Default for Stack<T> {
    fn default() -> Self {
        let inner = Vec::default();
        Self { inner }
    }
}

impl<T: Eq> Stack<T> {
    pub(crate) fn push(&mut self, elem: T) {
        self.inner.push(elem);
    }

    pub(crate) fn pop(&mut self) -> Option<T> {
        self.inner.pop()
    }

    pub(crate) fn peek(&self) -> Option<&T> {
        self.inner.last()
    }

    pub(crate) fn insert_at(&mut self, index: usize, value: T) {
        self.inner.insert(index, value);
    }
}

/// the compiler's view of the stack, including information about whether a value has been spilled
/// to memory
pub(crate) type VStack = Stack<(
    ValueIdentifier,
    (ast_types::DataType, Option<BFieldElement>),
)>;

impl VStack {
    /// Returns (stack_position, data_type, maybe_memory_location) where `stack_position` is the top of
    /// the value on the stack, i.e. the most shallow part of the value. Top of stack has index 0.
    /// May return a depth that exceeds the addressable space (16 elements) in which case spilling
    /// may be required.
    pub(crate) fn find_stack_value(
        &self,
        seek_addr: &ValueIdentifier,
    ) -> (usize, ast_types::DataType, Option<BFieldElement>) {
        let mut position: usize = 0;
        for (found_addr, (data_type, spilled)) in self.inner.iter().rev() {
            if seek_addr == found_addr {
                return (position, data_type.to_owned(), spilled.to_owned());
            }

            position += data_type.stack_size();
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
            let spill_code =
                copy_value_to_memory(memory_spill_address, data_type.stack_size(), top_of_value);
            code.extend(spill_code);
        }

        code
    }
}
