use std::cell::RefCell;
use std::collections::HashMap;
use std::thread_local;
use std::vec::Vec;

use num::Zero;
use tasm_lib::triton_vm::prelude::*;

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
    pub(super) static ND_MEMORY: RefCell<HashMap<BFieldElement, BFieldElement>> =
        RefCell::new(HashMap::default());
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
    PUB_OUTPUT.with(|v| v.borrow_mut().extend(x.coefficients.to_vec()));
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_write_to_stdout___digest(x: Digest) {
    PUB_OUTPUT.with(|v| v.borrow_mut().extend(x.values().to_vec()));
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
    PUB_OUTPUT.with(|v| v.borrow_mut().extend(x.encode()));
}

#[allow(non_snake_case)]
pub(super) fn tasm_io_write_to_stdout___u128(x: u128) {
    PUB_OUTPUT.with(|v| v.borrow_mut().extend(x.encode()));
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

/// Note: the rust shadowing does not actually assert digest equivalence – it has no way of knowing
/// the digest of the own program. That property is inherent to Triton VM.
pub(super) fn tasm_recufier_read_and_verify_own_program_digest_from_std_in() -> Digest {
    tasm_io_read_stdin___digest()
}

use crate::tests_and_benchmarks::ozk::programs::recufier::verify::FriVerify;
pub(super) fn tasm_recufier_fri_verify(
    proof_iter: &mut VmProofIter,
    fri_parameters: Box<FriVerify>,
) -> Vec<(u32, XFieldElement)> {
    vec![]
}

use num::One;
use tasm_lib::triton_vm::proof_item::ProofItem;
use tasm_lib::triton_vm::proof_item::ProofItemVariant;
use tasm_lib::twenty_first::shared_math::tip5::Tip5State;
use tasm_lib::twenty_first::util_types::algebraic_hasher::SpongeHasher;

use crate::tests_and_benchmarks::ozk::bfield_codec::decode_from_memory_using_size;
use crate::triton_vm::fri::AuthenticationStructure;
use crate::triton_vm::proof_item::FriResponse;

// This struct should only be seen be `rustc`, not by `tasm-lang`
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) struct VmProofIter {
    pub current_item_pointer: BFieldElement,
}

impl VmProofIter {
    pub(super) fn new() -> Self {
        Self {
            current_item_pointer: BFieldElement::new(2),
        }
    }
}

macro_rules! vm_proof_iter_impl {
    ($($variant:ident($payload:ty) defines $next_as_fn:ident uses $try_into_fn:ident,)+) => {
        impl VmProofIter {
            $(
            pub(super) fn $next_as_fn(&mut self, sponge_state: &mut Tip5State) -> Box<$payload> {
                let read_size_from_ram = || {
                    let bfe = decode_from_memory_using_size::<BFieldElement>(
                        self.current_item_pointer,
                        1,
                    );
                    bfe.value() as usize
                };
                let item_size = ProofItemVariant::$variant
                    .payload_static_length()
                    .map(|x| x + 1)
                    .unwrap_or_else(read_size_from_ram);

                let discriminant_pointer = self.current_item_pointer + BFieldElement::one();
                let proof_item =
                    decode_from_memory_using_size::<ProofItem>(discriminant_pointer, item_size);

                self.current_item_pointer += BFieldElement::new(item_size as u64 + 1);

                if ProofItemVariant::$variant.include_in_fiat_shamir_heuristic() {
                    Tip5::pad_and_absorb_all(sponge_state, &proof_item.encode());
                }

                let payload = proof_item.$try_into_fn().unwrap();
                Box::new(payload)
            }
        )+
        }
    };
}

vm_proof_iter_impl!(
    MerkleRoot(Digest) defines next_as_merkleroot
        uses try_into_merkle_root,
    OutOfDomainBaseRow(Vec<XFieldElement>) defines next_as_outofdomainbaserow
        uses try_into_out_of_domain_base_row,
    OutOfDomainExtRow(Vec<XFieldElement>) defines next_as_outofdomainextrow
        uses try_into_out_of_domain_ext_row,
    OutOfDomainQuotientSegments([XFieldElement; 4]) defines next_as_outofdomainquotientsegments
        uses try_into_out_of_domain_quot_segments,
    AuthenticationStructure(AuthenticationStructure) defines next_as_authenticationstructure
        uses try_into_authentication_structure,
    MasterBaseTableRows(Vec<Vec<BFieldElement>>) defines next_as_masterbasetablerows
        uses try_into_master_base_table_rows,
    MasterExtTableRows(Vec<Vec<XFieldElement>>) defines next_as_masterexttablerows
        uses try_into_master_ext_table_rows,
    Log2PaddedHeight(u32) defines next_as_log2paddedheight
        uses try_into_log2_padded_height,
    QuotientSegmentsElements(Vec<[XFieldElement; 4]>) defines next_as_quotientsegmentselements
        uses try_into_quot_segments_elements,
    FriCodeword(Vec<XFieldElement>) defines next_as_fricodeword
        uses try_into_fri_codeword,
    FriResponse(FriResponse) defines next_as_friresponse
        uses try_into_fri_response,
);
