use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

use super::proof_item::*;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct VmProofIter {
    /// Uses highly specific knowledge about [`BFieldCodec`].
    pub current_item_size_pointer: usize,
}

impl VmProofIter {
    pub fn new() -> VmProofIter {
        return VmProofIter {
            current_item_size_pointer: 1,
        };
    }

    /// # Panics
    /// Panics if there is no next item.
    pub fn next_as_log_2_padded_height(&self) -> u32 {
        let current_item_size_pointer: BFieldElement =
            BFieldElement::new(self.current_item_size_pointer as u64);
        let proof_item: Box<ProofItem> =
            ProofItem::decode(&tasm::load_from_memory(current_item_size_pointer)).unwrap();

        return match proof_item.as_ref() {
            ProofItem::Log2PaddedHeight(height) => {
                //
                *height
            }
            _ => {
                panic!()
            }
        };
    }
}
