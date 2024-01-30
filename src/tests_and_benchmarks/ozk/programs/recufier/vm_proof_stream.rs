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
        let item_size_pointer: BFieldElement = BFieldElement::new(self.current_item_pointer as u64);
        let item_pointer: BFieldElement = item_size_pointer + BFieldElement::one();

        // super specific knowledge about the encoding of `ProofItem::Log2PaddedHeight`
        let item_size: usize = 2;
        self.current_item_pointer += item_size + 1;

        let log_2_padded_height_item: Box<ProofItem> =
            bfield_codec::decode_from_memory_using_size::<ProofItem>(item_pointer, item_size);

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
