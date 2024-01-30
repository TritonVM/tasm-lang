use std::collections::HashMap;

use itertools::Itertools;

use crate::tests_and_benchmarks::ozk::rust_shadows::ND_MEMORY;
use crate::triton_vm::prelude::*;

pub fn decode_from_memory<T: BFieldCodec>(address: BFieldElement) -> Box<T> {
    ND_MEMORY.with(|mem| _decode_from_memory::<T>(&mem.borrow(), address))
}

fn _decode_from_memory<T: BFieldCodec>(
    memory: &HashMap<BFieldElement, BFieldElement>,
    address: BFieldElement,
) -> Box<T> {
    let item_size = match T::static_length() {
        Some(length) => length,
        None => memory.get(&address).unwrap().value() as usize,
    };

    let sequence = (0..item_size)
        .map(|i| address + BFieldElement::new(i as u64))
        .map(|b| memory.get(&b).copied().unwrap_or(BFieldElement::new(0)))
        .collect_vec();

    T::decode(&sequence).unwrap()
}
