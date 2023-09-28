use std::cell::RefCell;
use std::collections::HashMap;
use std::thread_local;
use std::vec::Vec;
use tasm_lib::Digest;
use triton_vm::{BFieldElement, NonDeterminism};
use twenty_first::shared_math::{bfield_codec::BFieldCodec, x_field_element::XFieldElement};

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

pub(super) fn tasm_io_read_stdin___bfe() -> BFieldElement {
    #[allow(clippy::unwrap_used)]
    PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap())
}

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

pub(super) fn tasm_io_read_stdin___digest() -> Digest {
    #[allow(clippy::unwrap_used)]
    let e4 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    let e3 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    let e2 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    let e1 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    let e0 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    Digest::new([e0, e1, e2, e3, e4])
}

pub(super) fn tasm_io_write_to_stdout___bfe(x: BFieldElement) {
    PUB_OUTPUT.with(|v| v.borrow_mut().push(x));
}

pub(super) fn tasm_io_write_to_stdout___xfe(x: XFieldElement) {
    PUB_OUTPUT.with(|v| v.borrow_mut().append(&mut x.coefficients.to_vec()));
}

pub(super) fn tasm_io_write_to_stdout___digest(x: Digest) {
    PUB_OUTPUT.with(|v| v.borrow_mut().append(&mut x.values().to_vec()));
}

pub(super) fn tasm_io_write_to_stdout___bool(x: bool) {
    PUB_OUTPUT.with(|v| v.borrow_mut().push(BFieldElement::new(x as u64)));
}

pub(super) fn tasm_io_write_to_stdout___u32(x: u32) {
    PUB_OUTPUT.with(|v| v.borrow_mut().push(BFieldElement::new(x as u64)));
}

pub(super) fn tasm_io_write_to_stdout___u64(x: u64) {
    PUB_OUTPUT.with(|v| v.borrow_mut().append(&mut x.encode()));
}

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
    // TODO: It would be cool if `main_func` could return something, but I'm not sure that's possible.
    // If it was possible, then that return value could be compared to the stack from Triton VM after
    // execution.
    Box::new(
        |input: Vec<BFieldElement>, non_determinism: NonDeterminism<BFieldElement>| {
            init_io(input, non_determinism);
            main_func();
            get_pub_output()
        },
    )
}
