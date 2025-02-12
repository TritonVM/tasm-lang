//! This module contains functions for interacting with the input/output monad
//! implicit in a VM execution. It contains functions for mutating and verifying
//! the correct content of the input/output while executing a Rust function
//! on the host machine's native architecture (i.e. your machine).
//! It has been shamelessly copied from greenhat's omnizk compiler project:
//! https://github.com/greenhat/omnizk

use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::thread_local;
use std::vec::Vec;

use itertools::Itertools;
use num::One;
use num::Zero;
use tasm_lib::memory::encode_to_memory;
use tasm_lib::structure::tasm_object::decode_from_memory_with_size;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::triton_vm::proof_item::ProofItem;
use tasm_lib::triton_vm::proof_item::ProofItemVariant;
use tasm_lib::triton_vm::table::master_table::MasterAuxTable;
use tasm_lib::twenty_first::math::tip5::Tip5;
use tasm_lib::twenty_first::math::tip5::RATE;
use tasm_lib::twenty_first::math::traits::ModPowU32;
use tasm_lib::twenty_first::math::x_field_element::EXTENSION_DEGREE;
use tasm_lib::twenty_first::util_types::merkle_tree::MerkleTreeInclusionProof;
use tasm_lib::twenty_first::util_types::sponge::Sponge;
use tasm_lib::verifier::master_table::air_constraint_evaluation;
use tasm_lib::verifier::master_table::air_constraint_evaluation::AirConstraintEvaluation;
use tasm_lib::verifier::master_table::air_constraint_evaluation::AirConstraintSnippetInputs;

use crate::tests_and_benchmarks::ozk::programs::recufier::challenges::Challenges as TasmLangChallenges;
use crate::tests_and_benchmarks::ozk::programs::recufier::stark_parameters::FriVerify;
use crate::triton_vm::arithmetic_domain::ArithmeticDomain;
use crate::triton_vm::challenges::Challenges;
use crate::triton_vm::fri::AuthenticationStructure;
use crate::triton_vm::fri::Fri;
use crate::triton_vm::proof_item::FriResponse;
use crate::triton_vm::proof_stream::ProofStream;
use crate::triton_vm::table::AuxiliaryRow;
use crate::triton_vm::table::MainRow;
use crate::triton_vm::table::QuotientSegments;
use crate::twenty_first::prelude::*;

thread_local! {
    static PUB_INPUT: RefCell<VecDeque<BFieldElement>> = const { RefCell::new(VecDeque::new()) };
    static PUB_OUTPUT: RefCell<Vec<BFieldElement>> = const { RefCell::new(vec![]) };
    static ND_INDIVIDUAL_TOKEN: RefCell<VecDeque<BFieldElement>> = const { RefCell::new(VecDeque::new()) };
    static ND_DIGESTS: RefCell<VecDeque<Digest>> = const { RefCell::new(VecDeque::new()) };
    static ND_MEMORY: RefCell<HashMap<BFieldElement, BFieldElement>> =
        RefCell::new(HashMap::default());
    static SPONGE_STATE: RefCell<Option<Tip5>> = const {  RefCell::new(None) };
    static PROGRAM_DIGEST: RefCell<Option<Digest>> = const {  RefCell::new(None) };
}

pub(super) struct Tip5WithState;

impl Tip5WithState {
    pub(super) fn init() {
        SPONGE_STATE.with_borrow_mut(|v| {
            *v = Some(Tip5::init());
        });
    }

    pub(super) fn absorb(input: [BFieldElement; RATE]) {
        SPONGE_STATE.with_borrow_mut(|v| {
            Tip5::absorb(v.as_mut().unwrap(), input);
        });
    }

    pub(super) fn squeeze() -> [BFieldElement; RATE] {
        SPONGE_STATE.with_borrow_mut(|v| Tip5::squeeze(v.as_mut().unwrap()))
    }

    pub(super) fn pad_and_absorb_all(input: &[BFieldElement]) {
        SPONGE_STATE.with_borrow_mut(|v| {
            Tip5::pad_and_absorb_all(v.as_mut().unwrap(), input);
        });
    }

    pub(super) fn sample_scalars(num_elements: usize) -> Vec<XFieldElement> {
        SPONGE_STATE.with_borrow_mut(|v| Tip5::sample_scalars(v.as_mut().unwrap(), num_elements))
    }
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

pub(super) fn init_vm_state(
    pub_input: Vec<BFieldElement>,
    non_determinism: NonDeterminism,
    program_digest: Option<Digest>,
) {
    PUB_INPUT.with(|v| {
        *v.borrow_mut() = pub_input.into();
    });
    ND_INDIVIDUAL_TOKEN.with(|v| {
        *v.borrow_mut() = non_determinism.individual_tokens.into();
    });
    ND_DIGESTS.with(|v| {
        *v.borrow_mut() = non_determinism.digests.into();
    });
    ND_MEMORY.with(|v| {
        *v.borrow_mut() = non_determinism.ram;
    });
    PUB_OUTPUT.with(|v| {
        *v.borrow_mut() = vec![];
    });
    SPONGE_STATE.with_borrow_mut(|v| {
        *v = None;
    });
    PROGRAM_DIGEST.with_borrow_mut(|v| {
        *v = program_digest;
    })
}

pub(super) fn get_pub_output() -> Vec<BFieldElement> {
    PUB_OUTPUT.with(|v| v.borrow().clone())
}

#[allow(non_snake_case)]
pub(super) fn tasmlib_io_read_stdin___bfe() -> BFieldElement {
    #[allow(clippy::unwrap_used)]
    PUB_INPUT.with(|v| v.borrow_mut().pop_front().unwrap())
}

#[allow(non_snake_case)]
pub(super) fn tasmlib_io_read_secin___bfe() -> BFieldElement {
    #[allow(clippy::unwrap_used)]
    ND_INDIVIDUAL_TOKEN.with(|v| v.borrow_mut().pop_front().unwrap())
}

#[allow(non_snake_case)]
pub(super) fn tasmlib_io_read_stdin___xfe() -> XFieldElement {
    let x2 = PUB_INPUT.with(|v| v.borrow_mut().pop_front().unwrap());
    let x1 = PUB_INPUT.with(|v| v.borrow_mut().pop_front().unwrap());
    let x0 = PUB_INPUT.with(|v| v.borrow_mut().pop_front().unwrap());
    XFieldElement::new([x0, x1, x2])
}

#[allow(dead_code)]
#[allow(non_snake_case)]
pub(super) fn tasmlib_arithmetic_u64_log_2_floor(val: u64) -> u32 {
    assert!(!val.is_zero());
    u64::BITS - val.leading_zeros() - 1
}

#[allow(non_snake_case)]
pub(super) fn tasmlib_io_read_stdin___u32() -> u32 {
    #[allow(clippy::unwrap_used)]
    let val: u32 = PUB_INPUT
        .with(|v| v.borrow_mut().pop_front().unwrap())
        .try_into()
        .unwrap();
    val
}

#[allow(non_snake_case)]
pub(super) fn tasmlib_io_read_stdin___u64() -> u64 {
    #[allow(clippy::unwrap_used)]
    let hi: u32 = PUB_INPUT
        .with(|v| v.borrow_mut().pop_front().unwrap())
        .try_into()
        .unwrap();
    let lo: u32 = PUB_INPUT
        .with(|v| v.borrow_mut().pop_front().unwrap())
        .try_into()
        .unwrap();
    ((hi as u64) << 32) + lo as u64
}

#[allow(non_snake_case)]
pub(super) fn tasmlib_io_read_stdin___u128() -> u128 {
    #[allow(clippy::unwrap_used)]
    let e3: u32 = PUB_INPUT
        .with(|v| v.borrow_mut().pop_front().unwrap())
        .try_into()
        .unwrap();
    let e2: u32 = PUB_INPUT
        .with(|v| v.borrow_mut().pop_front().unwrap())
        .try_into()
        .unwrap();
    let e1: u32 = PUB_INPUT
        .with(|v| v.borrow_mut().pop_front().unwrap())
        .try_into()
        .unwrap();
    let e0: u32 = PUB_INPUT
        .with(|v| v.borrow_mut().pop_front().unwrap())
        .try_into()
        .unwrap();
    ((e3 as u128) << 96) + ((e2 as u128) << 64) + ((e1 as u128) << 32) + e0 as u128
}

#[allow(non_snake_case)]
pub(super) fn tasmlib_io_read_stdin___digest() -> Digest {
    let e4 = PUB_INPUT.with(|v| v.borrow_mut().pop_front().unwrap());
    let e3 = PUB_INPUT.with(|v| v.borrow_mut().pop_front().unwrap());
    let e2 = PUB_INPUT.with(|v| v.borrow_mut().pop_front().unwrap());
    let e1 = PUB_INPUT.with(|v| v.borrow_mut().pop_front().unwrap());
    let e0 = PUB_INPUT.with(|v| v.borrow_mut().pop_front().unwrap());
    Digest::new([e0, e1, e2, e3, e4])
}

#[allow(non_snake_case)]
pub(super) fn tasmlib_io_write_to_stdout___bfe(x: BFieldElement) {
    PUB_OUTPUT.with(|v| v.borrow_mut().push(x));
}

#[allow(non_snake_case)]
pub(super) fn tasmlib_io_write_to_stdout___xfe(x: XFieldElement) {
    PUB_OUTPUT.with(|v| v.borrow_mut().extend(x.coefficients.to_vec()));
}

#[allow(non_snake_case)]
pub(super) fn tasmlib_io_write_to_stdout___digest(x: Digest) {
    PUB_OUTPUT.with(|v| v.borrow_mut().extend(x.values().to_vec()));
}

#[allow(non_snake_case)]
pub(super) fn tasmlib_io_write_to_stdout___bool(x: bool) {
    PUB_OUTPUT.with(|v| v.borrow_mut().push(BFieldElement::new(x as u64)));
}

#[allow(non_snake_case)]
pub(super) fn tasmlib_io_write_to_stdout___u32(x: u32) {
    PUB_OUTPUT.with(|v| v.borrow_mut().push(BFieldElement::new(x as u64)));
}

#[allow(non_snake_case)]
pub(super) fn tasmlib_io_write_to_stdout___u64(x: u64) {
    PUB_OUTPUT.with(|v| v.borrow_mut().extend(x.encode()));
}

#[allow(non_snake_case)]
pub(super) fn tasmlib_io_write_to_stdout___u128(x: u128) {
    PUB_OUTPUT.with(|v| v.borrow_mut().extend(x.encode()));
}

pub(super) fn tasmlib_arithmetic_u64_mul_two_u64s_to_u128_u64(lhs: u64, rhs: u64) -> u128 {
    lhs as u128 * rhs as u128
}

#[allow(clippy::type_complexity)]
pub(super) fn wrap_main_with_io(
    main_func: &'static dyn Fn(),
) -> Box<dyn Fn(Vec<BFieldElement>, NonDeterminism) -> Vec<BFieldElement>> {
    Box::new(
        |input: Vec<BFieldElement>, non_determinism: NonDeterminism| {
            init_vm_state(input, non_determinism, None);
            main_func();
            get_pub_output()
        },
    )
}

#[allow(clippy::type_complexity)]
pub(super) fn wrap_main_with_io_and_program_digest(
    main_func: &'static dyn Fn(),
) -> Box<dyn Fn(Vec<BFieldElement>, NonDeterminism, Program) -> Vec<BFieldElement>> {
    Box::new(
        |input: Vec<BFieldElement>, non_determinism: NonDeterminism, program: Program| {
            init_vm_state(input, non_determinism, Some(program.hash()));
            main_func();
            get_pub_output()
        },
    )
}

// Hashing-related shadows
pub(super) fn tasmlib_hashing_merkle_verify(
    root: Digest,
    tree_height: u32,
    leaf_index: u32,
    leaf: Digest,
) {
    let mut path: Vec<Digest> = vec![];

    ND_DIGESTS.with_borrow_mut(|nd_digests| {
        for _ in 0..tree_height {
            path.push(nd_digests.pop_front().unwrap());
        }
    });

    let mt_inclusion_proof = MerkleTreeInclusionProof {
        tree_height: tree_height as usize,
        indexed_leafs: vec![(leaf_index as usize, leaf)],
        authentication_structure: path,
    };

    assert!(mt_inclusion_proof.verify(root));
}

pub(super) fn tasmlib_verifier_own_program_digest() -> Digest {
    PROGRAM_DIGEST
        .with_borrow(|digest| digest.expect("Program digest must be set for this function to work"))
}

pub(super) fn tasmlib_verifier_claim_instantiate_fiat_shamir_with_claim(claim: &Claim) {
    SPONGE_STATE
        .with_borrow_mut(|sponge| sponge.as_mut().unwrap().pad_and_absorb_all(&claim.encode()));
}

pub(super) fn tasmlib_verifier_challenges_new_generic_dyn_claim_59_4(
    claim: &Claim,
) -> Box<TasmLangChallenges> {
    let sampled_challenges = SPONGE_STATE.with_borrow_mut(|maybe_sponge| {
        let sponge = maybe_sponge.as_mut().unwrap();
        sponge.sample_scalars(Challenges::SAMPLE_COUNT)
    });
    let claim = Claim {
        program_digest: claim.program_digest,
        version: claim.version,
        input: claim.input.clone(),
        output: claim.output.clone(),
    };
    let Challenges { challenges } = Challenges::new(sampled_challenges, &claim);

    // Store Challenges at their expected value in memory
    let memory_layout = air_constraint_evaluation::MemoryLayout::conventional_dynamic();
    ND_MEMORY.with_borrow_mut(|memory| {
        encode_to_memory(memory, memory_layout.challenges_pointer(), &challenges);
    });

    let challenges = TasmLangChallenges { challenges };
    Box::new(challenges)
}

fn inner_product(a: &[XFieldElement], b: &[XFieldElement]) -> XFieldElement {
    a.iter()
        .zip_eq(b.iter())
        .fold(XFieldElement::zero(), |acc, (a, b)| acc + *a * *b)
}

pub(super) fn tasmlib_array_inner_product_of_4_xfes(
    a: [XFieldElement; 4],
    b: [XFieldElement; 4],
) -> XFieldElement {
    inner_product(&a, &b)
}

pub(super) fn tasmlib_array_inner_product_of_596_xfes(
    a: [XFieldElement; 596],
    b: [XFieldElement; 596],
) -> XFieldElement {
    inner_product(&a, &b)
}

pub(super) fn tasmlib_array_horner_evaluation_with_4_coefficients(
    coefficients: [XFieldElement; 4],
    indeterminate: XFieldElement,
) -> XFieldElement {
    // evaluate polynomial using Horner's method
    let mut running_evaluation = XFieldElement::zero();
    for c in coefficients.into_iter().rev() {
        running_evaluation *= indeterminate;
        running_evaluation += c;
    }

    running_evaluation
}

pub(super) fn tasmlib_verifier_master_table_air_constraint_evaluation(
    curr_main: &MainRow<XFieldElement>,
    curr_aux: &AuxiliaryRow,
    next_main: &MainRow<XFieldElement>,
    next_aux: &AuxiliaryRow,
) -> [XFieldElement; MasterAuxTable::NUM_CONSTRAINTS] {
    const CHALLENGES_LENGTH: usize = Challenges::COUNT;
    let mem_layout = air_constraint_evaluation::MemoryLayout::conventional_static();
    let challenges: Box<[XFieldElement; CHALLENGES_LENGTH]> = ND_MEMORY.with_borrow(|memory| {
        decode_from_memory_with_size(
            memory,
            mem_layout.challenges_pointer(),
            EXTENSION_DEGREE * CHALLENGES_LENGTH,
        )
        .unwrap()
    });

    let input_values = AirConstraintSnippetInputs {
        current_main_row: curr_main.to_vec(),
        current_aux_row: curr_aux.to_vec(),
        next_main_row: next_main.to_vec(),
        next_aux_row: next_aux.to_vec(),
        challenges: Challenges {
            challenges: *challenges,
        },
    };

    AirConstraintEvaluation::host_machine_air_constraint_evaluation(input_values)
        .try_into()
        .unwrap()
}

pub(super) fn tasmlib_verifier_master_table_divide_out_zerofiers(
    mut air_evaluation_result: [XFieldElement; MasterAuxTable::NUM_CONSTRAINTS],
    out_of_domain_point_curr_row: XFieldElement,
    padded_height: u32,
    trace_domain_generator: BFieldElement,
) -> [XFieldElement; MasterAuxTable::NUM_CONSTRAINTS] {
    let initial_zerofier_inv: XFieldElement =
        (out_of_domain_point_curr_row - BFieldElement::one()).inverse();
    let consistency_zerofier_inv: XFieldElement =
        (out_of_domain_point_curr_row.mod_pow_u32(padded_height) - BFieldElement::one()).inverse();
    let except_last_row: XFieldElement =
        out_of_domain_point_curr_row - trace_domain_generator.inverse();
    let transition_zerofier_inv: XFieldElement = except_last_row * consistency_zerofier_inv;
    let terminal_zerofier_inv: XFieldElement = except_last_row.inverse();

    let mut i: usize = 0;
    while i < MasterAuxTable::NUM_INITIAL_CONSTRAINTS {
        air_evaluation_result[i] *= initial_zerofier_inv;
        i += 1;
    }
    while i < MasterAuxTable::NUM_INITIAL_CONSTRAINTS + MasterAuxTable::NUM_CONSISTENCY_CONSTRAINTS
    {
        air_evaluation_result[i] *= consistency_zerofier_inv;
        i += 1;
    }
    while i < MasterAuxTable::NUM_INITIAL_CONSTRAINTS
        + MasterAuxTable::NUM_CONSISTENCY_CONSTRAINTS
        + MasterAuxTable::NUM_TRANSITION_CONSTRAINTS
    {
        air_evaluation_result[i] *= transition_zerofier_inv;
        i += 1;
    }
    while i < MasterAuxTable::NUM_INITIAL_CONSTRAINTS
        + MasterAuxTable::NUM_CONSISTENCY_CONSTRAINTS
        + MasterAuxTable::NUM_TRANSITION_CONSTRAINTS
        + MasterAuxTable::NUM_TERMINAL_CONSTRAINTS
    {
        air_evaluation_result[i] *= terminal_zerofier_inv;
        i += 1;
    }

    air_evaluation_result
}

#[allow(non_snake_case)] // Name must agree with `tasm-lib`
pub(super) fn tasmlib_verifier_master_table_verify_Main_table_rows(
    num_combination_codeword_checks: usize,
    merkle_tree_height: u32,
    merkle_tree_root: &Digest,
    revealed_fri_indices_and_elements: &[(u32, XFieldElement)],
    main_rows: &[MainRow<BFieldElement>],
) {
    assert_eq!(main_rows.len(), num_combination_codeword_checks);
    let leaf_digests_main: Vec<_> = main_rows
        .iter()
        .map(|revealed_main_elem| Tip5::hash_varlen(revealed_main_elem))
        .collect();
    for (i, leaf) in leaf_digests_main.into_iter().enumerate() {
        tasmlib_hashing_merkle_verify(
            *merkle_tree_root,
            merkle_tree_height,
            revealed_fri_indices_and_elements[i].0,
            leaf,
        );
    }
}

#[allow(non_snake_case)] // Name must agree with `tasm-lib`
pub(super) fn tasmlib_verifier_master_table_verify_Aux_table_rows(
    num_combination_codeword_checks: usize,
    merkle_tree_height: u32,
    merkle_tree_root: &Digest,
    revealed_fri_indices_and_elements: &[(u32, XFieldElement)],
    aux_rows: &[AuxiliaryRow],
) {
    assert_eq!(aux_rows.len(), num_combination_codeword_checks);
    let leaf_digests_aux = aux_rows
        .iter()
        .map(|xvalues| {
            let b_values = xvalues.iter().flat_map(|xfe| xfe.coefficients.to_vec());
            Tip5::hash_varlen(&b_values.collect_vec())
        })
        .collect::<Vec<_>>();
    for (i, leaf) in leaf_digests_aux.into_iter().enumerate() {
        tasmlib_hashing_merkle_verify(
            *merkle_tree_root,
            merkle_tree_height,
            revealed_fri_indices_and_elements[i].0,
            leaf,
        );
    }
}

#[allow(non_snake_case)] // Name must agree with `tasm-lib`
pub(super) fn tasmlib_verifier_master_table_verify_Quotient_table_rows(
    num_combination_codeword_checks: usize,
    merkle_tree_height: u32,
    merkle_tree_root: &Digest,
    revealed_fri_indices_and_elements: &[(u32, XFieldElement)],
    quotient_segment_rows: &[QuotientSegments],
) {
    assert_eq!(quotient_segment_rows.len(), num_combination_codeword_checks);
    let interpret_xfe_as_bfes = |xfe: XFieldElement| xfe.coefficients.to_vec();
    let collect_row_as_bfes = |row: &QuotientSegments| row.map(interpret_xfe_as_bfes).concat();
    let leaf_digests_quot: Vec<_> = quotient_segment_rows
        .iter()
        .map(collect_row_as_bfes)
        .map(|row| Tip5::hash_varlen(&row))
        .collect();
    for (i, leaf) in leaf_digests_quot.into_iter().enumerate() {
        tasmlib_hashing_merkle_verify(
            *merkle_tree_root,
            merkle_tree_height,
            revealed_fri_indices_and_elements[i].0,
            leaf,
        );
    }
}

pub(super) fn tasmlib_verifier_fri_verify(
    proof_iter: &mut VmProofIter,
    fri_parameters: &FriVerify,
) -> Vec<(u32, XFieldElement)> {
    let fri = fri_parameters._to_fri();
    let tasm_lib_fri: tasm_lib::verifier::fri::verify::FriVerify = fri.into();
    let (advance_nd_digests_by, ret) = SPONGE_STATE.with_borrow_mut(|maybe_sponge_state| {
        let sponge_state = maybe_sponge_state.as_mut().unwrap();
        let proof_stream_before_fri = proof_iter
            .to_owned()
            ._into_proof_stream(sponge_state.to_owned());
        let advance_nd_digests_by =
            tasm_lib_fri.extract_digests_required_for_proving(&proof_stream_before_fri);
        let mut proof_stream = proof_stream_before_fri.clone();
        let indexed_leaves = fri.verify(&mut proof_stream).unwrap();
        let num_items_used_by_fri = proof_stream.items_index - proof_stream_before_fri.items_index;
        proof_iter._advance_by(num_items_used_by_fri).unwrap();
        *sponge_state = proof_stream.sponge;
        (
            advance_nd_digests_by.len(),
            indexed_leaves
                .into_iter()
                .map(|(idx, leaf)| (idx as u32, leaf))
                .collect(),
        )
    });

    ND_DIGESTS.with_borrow_mut(|nd_digests| {
        for _ in 0..advance_nd_digests_by {
            nd_digests.pop_front().unwrap();
        }
    });

    ret
}

impl FriVerify {
    fn _to_fri(&self) -> Fri {
        let fri_domain = ArithmeticDomain::of_length(self.domain_length as usize)
            .unwrap()
            .with_offset(self.domain_offset);
        let maybe_fri = Fri::new(
            fri_domain,
            self.expansion_factor as usize,
            self.num_collinearity_checks as usize,
        );

        maybe_fri.unwrap()
    }
}

/// This struct should only be seen be `rustc`, not by `tasm-lang`
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

    /// Advances by the given number of proof items or until the end of the iterator is reached.
    /// Returns an `Err` in the latter case.
    fn _advance_by(&mut self, num_items: usize) -> Result<(), ()> {
        for _ in 0..num_items {
            self._decode_current_item().ok_or(())?;
        }
        Ok(())
    }

    fn _into_proof_stream(mut self, sponge: Tip5) -> ProofStream {
        let mut items = vec![];
        while let Some(item) = self._decode_current_item() {
            items.push(item);
        }

        ProofStream {
            items,
            items_index: 0,
            sponge,
        }
    }

    fn _decode_current_item(&mut self) -> Option<ProofItem> {
        let item_size_pointer = self.current_item_pointer;
        let item_size =
            try_decode_from_memory_using_size::<BFieldElement>(item_size_pointer, 1).ok()?;
        let item_size = item_size.value() as usize;

        let discriminant_pointer = self.current_item_pointer + BFieldElement::one();
        let proof_item =
            try_decode_from_memory_using_size::<ProofItem>(discriminant_pointer, item_size).ok()?;
        self.current_item_pointer += BFieldElement::new(item_size as u64 + 1);

        Some(*proof_item)
    }
}

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

macro_rules! vm_proof_iter_impl {
    ($($variant:ident($payload:ty) defines $next_as_fn:ident uses $try_into_fn:ident,)+) => {
        impl VmProofIter {
            $(
            pub(super) fn $next_as_fn(&mut self) -> Box<$payload> {
                let read_size_from_ram = || {
                    let item_pointer = self.current_item_pointer;
                    let bfe = try_decode_from_memory_using_size::<BFieldElement>(item_pointer, 1);
                    bfe.unwrap().value() as usize
                };
                let item_size = ProofItemVariant::$variant
                .payload_static_length()
                .map(|x| x + 1)
                .unwrap_or_else(read_size_from_ram);

            let discriminant_pointer = self.current_item_pointer + BFieldElement::one();
            let proof_item =
            try_decode_from_memory_using_size::<ProofItem>(discriminant_pointer, item_size)
            .unwrap();

        self.current_item_pointer += BFieldElement::new(item_size as u64 + 1);

        if ProofItemVariant::$variant.include_in_fiat_shamir_heuristic() {
                SPONGE_STATE.with_borrow_mut(|maybe_sponge_state| {
                    let sponge_state = maybe_sponge_state.as_mut().unwrap();
                    Tip5::pad_and_absorb_all(sponge_state, &proof_item.encode());
                })
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
    OutOfDomainMainRow(Box<MainRow<XFieldElement>>) defines next_as_outofdomainmainrow
        uses try_into_out_of_domain_main_row,
    OutOfDomainAuxRow(Box<AuxiliaryRow>) defines next_as_outofdomainauxrow
        uses try_into_out_of_domain_aux_row,
    OutOfDomainQuotientSegments(QuotientSegments) defines next_as_outofdomainquotientsegments
        uses try_into_out_of_domain_quot_segments,
    AuthenticationStructure(AuthenticationStructure) defines next_as_authenticationstructure
        uses try_into_authentication_structure,
    MasterMainTableRows(Vec<MainRow<BFieldElement>>) defines next_as_mastermaintablerows
        uses try_into_master_main_table_rows,
    MasterAuxTableRows(Vec<AuxiliaryRow>) defines next_as_masterauxtablerows
        uses try_into_master_aux_table_rows,
    Log2PaddedHeight(u32) defines next_as_log2paddedheight
        uses try_into_log2_padded_height,
    QuotientSegmentsElements(Vec<QuotientSegments>) defines next_as_quotientsegmentselements
        uses try_into_quot_segments_elements,
    FriCodeword(Vec<XFieldElement>) defines next_as_fricodeword
        uses try_into_fri_codeword,
    FriPolynomial(Polynomial<XFieldElement>) defines next_as_fripolynomial uses try_into_fri_polynomial,
    FriResponse(FriResponse) defines next_as_friresponse
        uses try_into_fri_response,
);

#[allow(non_snake_case)] // Name must agree with `tasm-lib`
pub fn tasmlib_array_inner_product_of_three_rows_with_weights_Bfe_mainrowelem<const N: usize>(
    ext_row: AuxiliaryRow,
    base_row: MainRow<BFieldElement>,
    weights: [XFieldElement; N],
) -> XFieldElement {
    let mut acc: XFieldElement = XFieldElement::zero();
    for i in 0..base_row.len() {
        acc += weights[i] * base_row[i];
    }

    let number_of_randomizers: usize = 0;
    for i in 0..(ext_row.len() - number_of_randomizers) {
        acc += ext_row[i] * weights[i + base_row.len()];
    }

    acc
}
