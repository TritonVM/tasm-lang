use std::cell::RefCell;
use std::collections::HashMap;
use std::thread_local;
use std::vec::Vec;
use tasm_lib::Digest;
use triton_vm::{BFieldElement, NonDeterminism};

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

pub fn load_from_memory(start_address: BFieldElement) -> Vec<BFieldElement> {
    // Loads everything from address 1 an upwards
    // TODO: We probably want to be able to set the
    // starting address of the memory we want to load
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

pub fn init_io(pub_input: Vec<BFieldElement>, non_determinism: NonDeterminism<BFieldElement>) {
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

pub fn get_pub_output() -> Vec<BFieldElement> {
    PUB_OUTPUT.with(|v| v.borrow().clone())
}

pub(crate) fn tasm_io_read_stdin_bfe() -> BFieldElement {
    #[allow(clippy::unwrap_used)]
    PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap())
}

pub(crate) fn tasm_io_write_to_stdout_bfe(x: BFieldElement) {
    PUB_OUTPUT.with(|v| v.borrow_mut().push(x));
}

pub(crate) fn tasm_io_read_stdin_digest() -> Digest {
    #[allow(clippy::unwrap_used)]
    // ND_DIGESTS.with(|v| v.borrow_mut().pop().unwrap())
    let e4 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    let e3 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    let e2 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    let e1 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    let e0 = PUB_INPUT.with(|v| v.borrow_mut().pop().unwrap());
    Digest::new([e0, e1, e2, e3, e4])
}

#[allow(dead_code)]
pub(crate) fn divine() -> BFieldElement {
    #[allow(clippy::unwrap_used)]
    ND_INDIVIDUAL_TOKEN.with(|v| v.borrow_mut().pop().unwrap())
}

#[allow(clippy::type_complexity)]
pub fn wrap_main_with_io(
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
