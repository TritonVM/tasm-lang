use std::collections::HashMap;

use itertools::Itertools;

use crate::tests_and_benchmarks::ozk::rust_shadows::ND_MEMORY;
use crate::triton_vm::prelude::*;

/// # Panics
/// - if memory does not contain the item size
/// - if memory does not contain the item
/// - if decoding fails
pub fn decode_from_memory<T: BFieldCodec>(address: BFieldElement) -> Box<T> {
    ND_MEMORY.with(|mem| _decode_from_memory::<T>(&mem.borrow(), address))
}

/// # Panics
/// - if memory does not contain the item
/// - if decoding fails
pub fn decode_from_memory_using_size<T: BFieldCodec>(
    address: BFieldElement,
    item_size: usize,
) -> Box<T> {
    ND_MEMORY.with(|mem| _decode_from_memory_using_size::<T>(&mem.borrow(), address, item_size))
}

fn _decode_from_memory<T: BFieldCodec>(
    memory: &HashMap<BFieldElement, BFieldElement>,
    address: BFieldElement,
) -> Box<T> {
    let item_size = match T::static_length() {
        Some(length) => length,
        None => memory.get(&address).unwrap().value() as usize,
    };

    _decode_from_memory_using_size(memory, address, item_size)
}

fn _decode_from_memory_using_size<T: BFieldCodec>(
    memory: &HashMap<BFieldElement, BFieldElement>,
    address: BFieldElement,
    item_size: usize,
) -> Box<T> {
    let sequence = (0..item_size)
        .map(|i| address + BFieldElement::new(i as u64))
        .map(|b| memory.get(&b).copied().unwrap_or(BFieldElement::new(0)))
        .collect_vec();

    T::decode(&sequence).unwrap()
}
