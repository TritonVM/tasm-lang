use std::collections::HashMap;

use itertools::Itertools;

use crate::tests_and_benchmarks::ozk::rust_shadows::ND_MEMORY;
use crate::triton_vm::prelude::*;

pub fn try_decode_from_memory_using_size<T: BFieldCodec>(
    address: BFieldElement,
    item_size: usize,
) -> Result<Box<T>, <T as BFieldCodec>::Error> {
    ND_MEMORY
        .with(|mem| inner_try_decode_from_memory_using_size::<T>(&mem.borrow(), address, item_size))
}

fn inner_try_decode_from_memory_using_size<T: BFieldCodec>(
    memory: &HashMap<BFieldElement, BFieldElement>,
    address: BFieldElement,
    item_size: usize,
) -> Result<Box<T>, <T as BFieldCodec>::Error> {
    let sequence = (0..item_size)
        .map(|i| address + BFieldElement::new(i as u64))
        .map(|b| memory.get(&b).copied().unwrap_or(BFieldElement::new(0)))
        .collect_vec();

    T::decode(&sequence)
}
