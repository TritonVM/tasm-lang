use num::One;
use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::bfield_codec;

use super::proof_item::*;

/// Uses highly specific knowledge about [`BFieldCodec`].
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct VmProofIter {
    pub current_item_pointer: usize,
}

impl VmProofIter {
    pub fn new() -> VmProofIter {
        return VmProofIter {
            current_item_pointer: 2,
        };
    }

    /// # Panics
    /// Panics if there is no next item.
    pub fn next_as_log_2_padded_height(&mut self) -> u32 {
        let current_item_size_pointer: BFieldElement =
            BFieldElement::new(self.current_item_pointer as u64);
        let current_item_pointer: BFieldElement = current_item_size_pointer + BFieldElement::one();

        // Super specific knowledge about the encoding of `ProofItem::Log2PaddedHeight`.
        self.current_item_pointer += 3;

        let log_2_padded_height_item: Box<ProofItem> =
            bfield_codec::decode_from_memory::<ProofItem>(current_item_pointer);

        return match log_2_padded_height_item.as_ref() {
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
