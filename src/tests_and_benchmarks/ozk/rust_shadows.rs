use std::cell::RefCell;
use std::collections::HashMap;
use std::thread_local;
use std::vec::Vec;

use anyhow::bail;
use num::Zero;
use rand::rngs::StdRng;
use rand::Rng;
use rand::RngCore;
use rand::SeedableRng;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::triton_vm::proof_item::FriResponse;
use tasm_lib::triton_vm::proof_item::ProofItem;
use tasm_lib::triton_vm::table::master_table::NUM_BASE_COLUMNS;
use tasm_lib::triton_vm::table::master_table::NUM_EXT_COLUMNS;
use tasm_lib::twenty_first::prelude::*;
use tasm_lib::twenty_first::shared_math::b_field_element::BFIELD_ONE;
use tasm_lib::twenty_first::shared_math::b_field_element::BFIELD_ZERO;
use tasm_lib::VmHasher;

use crate::tests_and_benchmarks::ozk::programs::recufier::vm_proof_stream::VmProofStream;

// This module contains functions for interacting with the input/output monad
// implicit in a VM execution. It contains functions for mutating and verifying
// the correct content of the input/output while executing a Rust function
// on the host machine's native architecture (i.e. your machine).
// It has been shamelessly copied from greenhat's omnizk compiler project:
// https://github.com/greenhat/omnizk

thread_local! {
    static PUB_INPUT: RefCell<Vec<BFieldElement>> = RefCell::new(vec![]);
    static PUB_OUTPUT: RefCell<Vec<BFieldElement>> = RefCell::new(vec![]);

    static ND_INDIVIDUAL_TOKEN: RefCell<Vec<BFieldElement>> = RefCell::new(vec![]);
    static ND_DIGESTS: RefCell<Vec<Digest>> = RefCell::new(vec![]);
    static ND_MEMORY: RefCell<HashMap<BFieldElement, BFieldElement>> = RefCell::new(HashMap::default());
}

pub(super) fn load_from_memory(start_address: BFieldElement) -> Vec<BFieldElement> {
    // Loads everything from address `start_address` an upwards
    let mut sorted_key_values = ND_MEMORY.with(|v| {
        let mut ret = vec![];
        for (k, v) in v.borrow().iter() {
            ret.push((*k, *v));
        }
        ret
    });
    sorted_key_values.sort_unstable_by_key(|x| x.0.value());
    let sorted_values = sorted_key_values
        .iter()
        .filter(|(k, _v)| k.value() >= start_address.value())
        .map(|x| x.1)
        .collect();
    sorted_values
}

pub(super) fn init_io(
    pub_input: Vec<BFieldElement>,
    non_determinism: NonDeterminism<BFieldElement>,
) {
    let mut pub_input_reversed = pub_input;
    pub_input_reversed.reverse();
    let mut inidividual_tokens_reversed = non_determinism.individual_tokens;
    inidividual_tokens_reversed.reverse();
    let mut digests_reversed = non_determinism.digests;
    digests_reversed.reverse();

    // TODO: Do we need to handle ND-memory as well?
    PUB_INPUT.with(|v| {
        *v.borrow_mut() = pub_input_reversed;
    });
    ND_INDIVIDUAL_TOKEN.with(|v| {
        *v.borrow_mut() = inidividual_tokens_reversed;
    });
    ND_DIGESTS.with(|v| {
        *v.borrow_mut() = digests_reversed;
    });
    ND_MEMORY.with(|v| {
        *v.borrow_mut() = non_determinism.ram;
    });
    PUB_OUTPUT.with(|v| {
        *v.borrow_mut() = vec![];
    });
}

pub(super) fn get_pub_output() -> Vec<BFieldElement> {
    PUB_OUTPUT.with(|v| v.borrow().clone())
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_read_stdin___bfe() -> BFieldElement {
    #[allow(clippy::unwrap_used)]
    PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap())
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_read_stdin___xfe() -> XFieldElement {
    #[allow(clippy::unwrap_used)]
    let x2 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    let x1 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    let x0 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    XFieldElement::new([x0, x1, x2])
}

#[allow(dead_code)]
#[allow(non_snake_case)]
pub(super) fn tasm_arithmetic_u64_log_2_floor(val: u64) -> u32 {
    assert!(!val.is_zero());
    u64::BITS - val.leading_zeros() - 1
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_read_stdin___u32() -> u32 {
    #[allow(clippy::unwrap_used)]
    let val: u32 = PUB_INPUT
        .with(|v| v.borrow_mut().pop().unwrap())
        .try_into()
        .unwrap();
    val
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_read_stdin___u64() -> u64 {
    #[allow(clippy::unwrap_used)]
    let hi: u32 = PUB_INPUT
        .with(|v| v.borrow_mut().pop().unwrap())
        .try_into()
        .unwrap();
    let lo: u32 = PUB_INPUT
        .with(|v| v.borrow_mut().pop().unwrap())
        .try_into()
        .unwrap();
    ((hi as u64) << 32) + lo as u64
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_read_stdin___u128() -> u128 {
    #[allow(clippy::unwrap_used)]
    let e3: u32 = PUB_INPUT
        .with(|v| v.borrow_mut().pop().unwrap())
        .try_into()
        .unwrap();
    let e2: u32 = PUB_INPUT
        .with(|v| v.borrow_mut().pop().unwrap())
        .try_into()
        .unwrap();
    let e1: u32 = PUB_INPUT
        .with(|v| v.borrow_mut().pop().unwrap())
        .try_into()
        .unwrap();
    let e0: u32 = PUB_INPUT
        .with(|v| v.borrow_mut().pop().unwrap())
        .try_into()
        .unwrap();
    ((e3 as u128) << 96) + ((e2 as u128) << 64) + ((e1 as u128) << 32) + e0 as u128
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_read_stdin___digest() -> Digest {
    #[allow(clippy::unwrap_used)]
    let e4 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    let e3 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    let e2 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    let e1 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    let e0 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    Digest::new([e0, e1, e2, e3, e4])
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_write_to_stdout___bfe(x: BFieldElement) {
    PUB_OUTPUT.with(|v| v.borrow_mut().push(x));
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_write_to_stdout___xfe(x: XFieldElement) {
    PUB_OUTPUT.with(|v| v.borrow_mut().append(&mut x.coefficients.to_vec()));
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_write_to_stdout___digest(x: Digest) {
    PUB_OUTPUT.with(|v| v.borrow_mut().append(&mut x.values().to_vec()));
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_write_to_stdout___bool(x: bool) {
    PUB_OUTPUT.with(|v| v.borrow_mut().push(BFieldElement::new(x as u64)));
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_write_to_stdout___u32(x: u32) {
    PUB_OUTPUT.with(|v| v.borrow_mut().push(BFieldElement::new(x as u64)));
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_write_to_stdout___u64(x: u64) {
    PUB_OUTPUT.with(|v| v.borrow_mut().append(&mut x.encode()));
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_write_to_stdout___u128(x: u128) {
    PUB_OUTPUT.with(|v| v.borrow_mut().append(&mut x.encode()));
}

pub(super) fn tasm_arithmetic_u64_mul_two_u64s_to_u128_u64(lhs: u64, rhs: u64) -> u128 {
    lhs as u128 * rhs as u128
}

#[allow(clippy::type_complexity)]
pub(super) fn wrap_main_with_io(
    main_func: &'static dyn Fn(),
) -> Box<dyn Fn(Vec<BFieldElement>, NonDeterminism<BFieldElement>) -> Vec<BFieldElement>> {
    Box::new(
        |input: Vec<BFieldElement>, non_determinism: NonDeterminism<BFieldElement>| {
            init_io(input, non_determinism);
            main_func();
            get_pub_output()
        },
    )
}

// Shadowing of `VmProofStream` functionality

pub(super) fn _tasm_recufier_proof_stream_dequeue(
    proof_stream: &mut VmProofStream,
) -> Box<ProofItem> {
    proof_stream._dequeue_internal().unwrap()
}

type _VmHasherState = twenty_first::shared_math::tip5::Tip5State;

impl VmProofStream {
    fn _new_internal(items: &[ProofItem]) -> Self {
        Self {
            word_index: 1,
            data: items.to_vec().encode(),
            _sponge_state: _VmHasherState::new(
                twenty_first::util_types::algebraic_hasher::Domain::VariableLength,
            ),
        }
    }
    fn _dequeue_internal(&mut self) -> anyhow::Result<Box<ProofItem>> {
        if self.word_index as usize >= self.data.len() {
            bail!("No more words left in stream.")
        }
        let size = self.data[self.word_index as usize].value() as usize;
        println!("size: {size}");
        let sequence =
            &self.data[(self.word_index as usize + 1)..(self.word_index as usize + 1 + size)];
        self.word_index += size as u32 + 1;
        let item = *ProofItem::decode(sequence)?;

        if item.include_in_fiat_shamir_heuristic() {
            self._fiat_shamir_internal(&item);
        }

        Ok(Box::new(item))
    }

    fn _fiat_shamir_internal<T: BFieldCodec>(&mut self, item: &T) {
        VmHasher::absorb_repeatedly(
            &mut self._sponge_state,
            Self::_encode_and_pad_item_internal(item).iter(),
        );
    }

    fn _encode_and_pad_item_internal<T: BFieldCodec>(item: &T) -> Vec<BFieldElement> {
        let encoding = item.encode();
        let last_chunk_len = (encoding.len() + 1) % VmHasher::RATE;
        let num_padding_zeros = match last_chunk_len {
            0 => 0,
            _ => VmHasher::RATE - last_chunk_len,
        };
        [
            encoding,
            vec![BFIELD_ONE],
            vec![BFIELD_ZERO; num_padding_zeros],
        ]
        .concat()
    }

    fn _sample_scalars_internal(&mut self, number: usize) -> Vec<XFieldElement> {
        VmHasher::sample_scalars(&mut self._sponge_state, number)
    }

    fn _sample_indices_internal(&mut self, upper_bound: u32, number: u32) -> Vec<u32> {
        VmHasher::sample_indices(&mut self._sponge_state, upper_bound, number as usize)
    }

    fn _pseudorandom_items_list_internal(seed: [u8; 32]) -> Vec<ProofItem> {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let num_iterations = rng.next_u32() % 5;
        let mut proof_items = vec![];
        for _ in 0..num_iterations {
            if rng.next_u32() % 2 == 1 {
                let authentication_structure: Vec<Digest> = (0..20).map(|_| rng.gen()).collect();
                proof_items.push(ProofItem::AuthenticationStructure(authentication_structure));
            }
            if rng.next_u32() % 2 == 1 {
                let fri_codeword: Vec<XFieldElement> = (0..20).map(|_| rng.gen()).collect();
                proof_items.push(ProofItem::FriCodeword(fri_codeword));
            }
            if rng.next_u32() % 2 == 1 {
                let auth_structure: Vec<Digest> = (0..20).map(|_| rng.gen()).collect();
                let revealed_leaves: Vec<XFieldElement> = (0..20).map(|_| rng.gen()).collect();
                let fri_response = FriResponse {
                    auth_structure,
                    revealed_leaves,
                };
                proof_items.push(ProofItem::FriResponse(fri_response));
            }
            if rng.next_u32() % 2 == 1 {
                proof_items.push(ProofItem::Log2PaddedHeight(rng.next_u32()));
            }
            if rng.next_u32() % 2 == 1 {
                let master_table_base_rows: Vec<Vec<BFieldElement>> = (0..20)
                    .map(|_| {
                        (0..20)
                            .map(|_| rng.gen::<BFieldElement>())
                            .collect::<Vec<BFieldElement>>()
                    })
                    .collect::<Vec<Vec<BFieldElement>>>();
                proof_items.push(ProofItem::MasterBaseTableRows(master_table_base_rows));
            }
            if rng.next_u32() % 2 == 1 {
                let master_table_ext_rows: Vec<Vec<XFieldElement>> = (0..20)
                    .map(|_| {
                        (0..20)
                            .map(|_| rng.gen::<XFieldElement>())
                            .collect::<Vec<XFieldElement>>()
                    })
                    .collect::<Vec<Vec<XFieldElement>>>();
                proof_items.push(ProofItem::MasterExtTableRows(master_table_ext_rows));
            }
            if rng.next_u32() % 2 == 1 {
                proof_items.push(ProofItem::MerkleRoot(rng.gen()));
            }
            if rng.next_u32() % 2 == 1 {
                let ood_base_row = (0..NUM_BASE_COLUMNS).map(|_| rng.gen()).collect();
                proof_items.push(ProofItem::OutOfDomainBaseRow(ood_base_row));
            }
            if rng.next_u32() % 2 == 1 {
                let ood_ext_row = (0..NUM_EXT_COLUMNS).map(|_| rng.gen()).collect();
                proof_items.push(ProofItem::OutOfDomainExtRow(ood_ext_row));
            }
            if rng.next_u32() % 2 == 1 {
                let ood_quotient_segments = rng.gen();
                proof_items.push(ProofItem::OutOfDomainQuotientSegments(
                    ood_quotient_segments,
                ));
            }
            if rng.next_u32() % 2 == 1 {
                let quotient_segment_elements = (0..20).map(|_| rng.gen()).collect();
                proof_items.push(ProofItem::QuotientSegmentsElements(
                    quotient_segment_elements,
                ));
            }
        }
        proof_items
    }
}
