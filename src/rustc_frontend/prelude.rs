//! The "prelude" that programs are type-checked against.
//!
//! Programs compiled by this compiler are written in Rust and make use of a
//! handful of types and functions that are native to Triton VM, or that are
//! provided by `tasm-lib`: `BFieldElement`, `XFieldElement`, `Digest`,
//! `Tip5`, the `tasm::tasmlib_*` snippets, etc. When such a program is
//! executed natively (as is done in this repository's tests), these come from
//! `twenty-first` and from Rust shadows of `tasm-lib` snippets. When such a
//! program is compiled to Triton assembly, `rustc` only needs to *type-check*
//! against them. So the compiler provides *stub* declarations of all those
//! types and functions -- declarations with the right signatures but without
//! meaningful bodies -- and lets `rustc` do all name resolution and type
//! checking. The code generator then maps the stub types back to their
//! Triton VM representation by name.
//!
//! In particular, the native types `BFieldElement`, `XFieldElement`, and
//! `Digest` are opaque here: their representation is never exposed to the
//! program, and the code generator keeps them as 1, 3, and 5 words on Triton
//! VM's stack, respectively.

use std::collections::BTreeSet;

use itertools::Itertools;
use regex::Regex;
use strum::IntoEnumIterator;
use tasm_lib::triton_vm::proof_item::ProofItemVariant;
use tasm_lib::triton_vm::table::master_table::MasterAuxTable;
use tasm_lib::triton_vm::table::master_table::MasterMainTable;
use tasm_lib::triton_vm::table::NUM_QUOTIENT_SEGMENTS;
use tasm_lib::triton_vm::table::NUM_RANDOMIZED_QUOTIENT_SEGMENTS;
use tasm_lib::twenty_first::tip5::RATE;

/// Name of the module that contains the prelude in the crate that is handed
/// to `rustc`.
pub(crate) const PRELUDE_MODULE_NAME: &str = "__tasm_lang_prelude";

/// Name of the module containing the `tasm-lib` snippet stubs.
pub(crate) const TASM_MODULE_NAME: &str = "tasm";

/// Name of the module containing the `BFieldCodec`-related memory functions.
pub(crate) const BFIELD_CODEC_MODULE_NAME: &str = "bfield_codec";

const STATIC_PRELUDE: &str = r#"
#![allow(unused, dead_code, non_snake_case, non_camel_case_types, clippy::all)]

use core::ops::{Add, AddAssign, BitAnd, BitOr, BitXor, Div, Mul, MulAssign, Neg, Not, Rem, Shl, Shr, Sub, SubAssign};

/// Marker for values that could not be produced by a stub. Stubs never run.
fn __stub<T>() -> T {
    panic!("tasm-lang prelude stubs must never be executed")
}

// ---------------------------------------------------------------------------
// `BFieldCodec`
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BFieldCodecError;

/// Stub of `twenty_first::math::bfield_codec::BFieldCodec`. All methods have
/// default implementations so that `impl BFieldCodec for T {}` suffices.
pub trait BFieldCodec: Sized {
    fn encode(&self) -> Vec<BFieldElement> { __stub() }
    fn decode(sequence: &[BFieldElement]) -> Result<Box<Self>, BFieldCodecError> { __stub() }
    fn static_length() -> Option<usize> { __stub() }
}

/// Stub of `tasm_lib::structure::tasm_object::TasmObject`.
pub trait TasmObject {}

impl BFieldCodec for bool {}
impl BFieldCodec for u32 {}
impl BFieldCodec for u64 {}
impl BFieldCodec for u128 {}
impl BFieldCodec for usize {}
impl BFieldCodec for BFieldElement {}
impl BFieldCodec for XFieldElement {}
impl BFieldCodec for Digest {}
impl BFieldCodec for () {}
impl<T: BFieldCodec> BFieldCodec for Vec<T> {}
impl<T: BFieldCodec> BFieldCodec for Box<T> {}
impl<T: BFieldCodec, const N: usize> BFieldCodec for [T; N] {}
impl<T: BFieldCodec> BFieldCodec for Option<T> {}
impl<T: BFieldCodec, E: BFieldCodec> BFieldCodec for Result<T, E> {}
impl<A: BFieldCodec, B: BFieldCodec> BFieldCodec for (A, B) {}
impl<A: BFieldCodec, B: BFieldCodec, C: BFieldCodec> BFieldCodec for (A, B, C) {}
impl<A: BFieldCodec, B: BFieldCodec, C: BFieldCodec, D: BFieldCodec> BFieldCodec for (A, B, C, D) {}

// ---------------------------------------------------------------------------
// Field elements
// ---------------------------------------------------------------------------

/// An element of the base field of Triton VM. Opaque: one word on the stack.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct BFieldElement(u64);

impl BFieldElement {
    pub const BYTES: usize = 8;
    pub const P: u64 = 0xffff_ffff_0000_0001;
    pub const MAX: u64 = Self::P - 1;

    pub const fn new(value: u64) -> Self { Self(value) }
    pub const fn zero() -> Self { Self(0) }
    pub const fn one() -> Self { Self(1) }
    pub fn is_zero(&self) -> bool { __stub() }
    pub fn is_one(&self) -> bool { __stub() }
    pub fn value(&self) -> u64 { __stub() }
    pub fn lift(&self) -> XFieldElement { __stub() }
    pub fn mod_pow_u32(&self, exponent: u32) -> Self { __stub() }
    pub fn mod_pow(&self, exponent: u64) -> Self { __stub() }
    pub fn inverse(&self) -> Self { __stub() }
    pub fn primitive_root_of_unity(n: u64) -> Option<Self> { __stub() }
    pub fn generator() -> Self { __stub() }
}

impl Add for BFieldElement { type Output = Self; fn add(self, rhs: Self) -> Self { __stub() } }
impl Sub for BFieldElement { type Output = Self; fn sub(self, rhs: Self) -> Self { __stub() } }
impl Mul for BFieldElement { type Output = Self; fn mul(self, rhs: Self) -> Self { __stub() } }
impl Div for BFieldElement { type Output = Self; fn div(self, rhs: Self) -> Self { __stub() } }
impl Neg for BFieldElement { type Output = Self; fn neg(self) -> Self { __stub() } }
impl AddAssign for BFieldElement { fn add_assign(&mut self, rhs: Self) { __stub() } }
impl SubAssign for BFieldElement { fn sub_assign(&mut self, rhs: Self) { __stub() } }
impl MulAssign for BFieldElement { fn mul_assign(&mut self, rhs: Self) { __stub() } }
impl From<u32> for BFieldElement { fn from(value: u32) -> Self { __stub() } }
impl From<u64> for BFieldElement { fn from(value: u64) -> Self { __stub() } }

/// An element of the degree-three extension field. Opaque: three words on
/// the stack.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct XFieldElement([BFieldElement; 3]);

pub const EXTENSION_DEGREE: usize = 3;

impl XFieldElement {
    pub const fn new(coefficients: [BFieldElement; 3]) -> Self { Self(coefficients) }
    pub const fn new_const(element: BFieldElement) -> Self { Self([element, BFieldElement::zero(), BFieldElement::zero()]) }
    pub const fn zero() -> Self { Self::new_const(BFieldElement::zero()) }
    pub const fn one() -> Self { Self::new_const(BFieldElement::one()) }
    pub fn is_zero(&self) -> bool { __stub() }
    pub fn is_one(&self) -> bool { __stub() }
    pub fn unlift(&self) -> Option<BFieldElement> { __stub() }
    pub fn mod_pow_u32(&self, exponent: u32) -> Self { __stub() }
    pub fn inverse(&self) -> Self { __stub() }
}

impl Add for XFieldElement { type Output = Self; fn add(self, rhs: Self) -> Self { __stub() } }
impl Sub for XFieldElement { type Output = Self; fn sub(self, rhs: Self) -> Self { __stub() } }
impl Mul for XFieldElement { type Output = Self; fn mul(self, rhs: Self) -> Self { __stub() } }
impl Div for XFieldElement { type Output = Self; fn div(self, rhs: Self) -> Self { __stub() } }
impl Neg for XFieldElement { type Output = Self; fn neg(self) -> Self { __stub() } }
impl Add<BFieldElement> for XFieldElement { type Output = Self; fn add(self, rhs: BFieldElement) -> Self { __stub() } }
impl Sub<BFieldElement> for XFieldElement { type Output = Self; fn sub(self, rhs: BFieldElement) -> Self { __stub() } }
impl Mul<BFieldElement> for XFieldElement { type Output = Self; fn mul(self, rhs: BFieldElement) -> Self { __stub() } }
impl AddAssign for XFieldElement { fn add_assign(&mut self, rhs: Self) { __stub() } }
impl SubAssign for XFieldElement { fn sub_assign(&mut self, rhs: Self) { __stub() } }
impl MulAssign for XFieldElement { fn mul_assign(&mut self, rhs: Self) { __stub() } }
impl MulAssign<BFieldElement> for XFieldElement { fn mul_assign(&mut self, rhs: BFieldElement) { __stub() } }
impl AddAssign<BFieldElement> for XFieldElement { fn add_assign(&mut self, rhs: BFieldElement) { __stub() } }

/// A Tip5 digest. Opaque: five words on the stack.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Digest(pub [BFieldElement; 5]);

impl Digest {
    pub const LEN: usize = 5;
    pub const fn new(digest: [BFieldElement; 5]) -> Self { Self(digest) }
    pub fn values(self) -> [BFieldElement; 5] { self.0 }
    pub fn reversed(self) -> Self { __stub() }
}

// ---------------------------------------------------------------------------
// Hashing
// ---------------------------------------------------------------------------

pub const RATE: usize = 10;

/// Stub of `twenty_first::math::tip5::Tip5`.
#[derive(Debug, Clone, Copy)]
pub struct Tip5;

impl Tip5 {
    pub fn hash_pair(left: Digest, right: Digest) -> Digest { __stub() }
    pub fn hash_varlen(input: &Vec<BFieldElement>) -> Digest { __stub() }
    pub fn hash<T: BFieldCodec>(value: &T) -> Digest { __stub() }
}

/// A Tip5 sponge whose state is held by Triton VM.
#[derive(Debug, Clone, Copy)]
pub struct Tip5WithState;

impl Tip5WithState {
    pub fn init() { __stub() }
    pub fn absorb(input: [BFieldElement; RATE]) { __stub() }
    pub fn squeeze() -> [BFieldElement; RATE] { __stub() }
    pub fn pad_and_absorb_all(input: &Vec<BFieldElement>) { __stub() }
    pub fn sample_scalars(num_elements: usize) -> Vec<XFieldElement> { __stub() }
}

// ---------------------------------------------------------------------------
// Recursive verification (`recufy`)
// ---------------------------------------------------------------------------

pub type MainRow<T> = [T; NUM_MAIN_COLUMNS];
pub type AuxiliaryRow = [XFieldElement; NUM_AUX_COLUMNS];
pub type OodQuotientSegments = [XFieldElement; NUM_QUOTIENT_SEGMENTS];
pub type QuotientSegments = OodQuotientSegments;
pub type RandQuotientSegments = [XFieldElement; NUM_RANDOMIZED_QUOTIENT_SEGMENTS];
pub type AuthenticationStructure = Vec<Digest>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FriResponse {
    pub auth_structure: Vec<Digest>,
    pub revealed_leaves: Vec<XFieldElement>,
}
impl BFieldCodec for FriResponse {}

/// A polynomial. The lifetime mirrors `twenty_first`'s `Polynomial`, which
/// can borrow its coefficients; here it is meaningless.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Polynomial<'a, T> {
    coefficients: Vec<T>,
    _lifetime: core::marker::PhantomData<&'a ()>,
}
impl<'a, T> Polynomial<'a, T> {
    pub fn new(coefficients: Vec<T>) -> Self { __stub() }
    pub fn into_coefficients(self) -> Vec<T> { self.coefficients }
}
impl<'a, T: BFieldCodec> BFieldCodec for Polynomial<'a, T> {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Claim {
    pub program_digest: Digest,
    pub version: u32,
    pub input: Vec<BFieldElement>,
    pub output: Vec<BFieldElement>,
}
impl BFieldCodec for Claim {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Proof(pub Vec<BFieldElement>);
impl BFieldCodec for Proof {}

// ---------------------------------------------------------------------------
// Iterator helpers
// ---------------------------------------------------------------------------

/// Stub of the parts of `itertools::Itertools` that programs may use.
pub trait Itertools: Iterator {
    fn collect_vec(self) -> Vec<Self::Item> where Self: Sized { __stub() }
}
impl<I: Iterator> Itertools for I {}
"#;

/// Everything that programs use from `tasm::` that does not correspond to a
/// snippet exported from `tasm-lib`, in the form of Rust function stubs.
const EXTRA_TASM_STUBS: &str = r#"
    /// Reads the memory region starting at `start_address` and returns it as a
    /// list. Only meaningful in the `T::decode(&tasm::load_from_memory(x))`
    /// idiom, which the compiler resolves at compile time.
    pub fn load_from_memory(start_address: BFieldElement) -> Vec<BFieldElement> { super::__stub() }
"#;

const BFIELD_CODEC_MODULE: &str = r#"
    pub fn decode_from_memory<T: BFieldCodec>(address: BFieldElement) -> Box<T> { super::__stub() }
    pub fn decode_from_memory_using_size<T: BFieldCodec>(address: BFieldElement, size: usize) -> Box<T> { super::__stub() }
"#;

/// Build the prelude for a program. `program_source` is scanned for the
/// `tasm::` functions the program uses, so that only those get a stub.
pub(crate) fn prelude_source(program_source: &str) -> String {
    let constants = format!(
        "pub const NUM_MAIN_COLUMNS: usize = {};\n\
         pub const NUM_AUX_COLUMNS: usize = {};\n\
         pub const NUM_QUOTIENT_SEGMENTS: usize = {};\n\
         pub const NUM_RANDOMIZED_QUOTIENT_SEGMENTS: usize = {};\n",
        MasterMainTable::NUM_COLUMNS,
        MasterAuxTable::NUM_COLUMNS,
        NUM_QUOTIENT_SEGMENTS,
        NUM_RANDOMIZED_QUOTIENT_SEGMENTS,
    );
    assert_eq!(RATE, 10, "prelude hard-codes the sponge's rate");

    // Snippets may refer to types declared by the program, e.g. `Challenges`.
    let tasm_module = format!(
        "pub mod {TASM_MODULE_NAME} {{\n    use super::*;\n    use crate::*;\n{}\n{EXTRA_TASM_STUBS}\n}}\n",
        tasm_lib_snippet_stubs(program_source)
    );
    let bfield_codec_module = format!(
        "pub mod {BFIELD_CODEC_MODULE_NAME} {{\n    use super::*;\n{BFIELD_CODEC_MODULE}\n}}\n"
    );

    format!(
        "{STATIC_PRELUDE}\n{constants}\n{}\n{tasm_module}\n{bfield_codec_module}\n",
        vm_proof_iter_source()
    )
}

/// The `VmProofIter` type and its `next_as_*` methods, matching what the
/// `recufy` library of this compiler expects.
fn vm_proof_iter_source() -> String {
    let struct_type = tasm_lib::verifier::vm_proof_iter::shared::vm_proof_iter_type();
    let fields = struct_type
        .fields
        .iter()
        .map(|(name, dtype)| {
            let rust_type = rust_type_of_tasm_lib_type(dtype)
                .unwrap_or_else(|| panic!("VmProofIter field {name} must have a Rust type"));
            format!("    pub {name}: {rust_type},")
        })
        .join("\n");

    let next_as_methods = ProofItemVariant::iter()
        .map(|variant| {
            let method_name = format!("next_as_{}", variant.to_string().to_lowercase());
            let payload_type = variant.payload_type();
            format!("    pub fn {method_name}(&mut self) -> Box<{payload_type}> {{ __stub() }}")
        })
        .join("\n");

    format!(
        "/// Iterator over the items of a proof that lives in Triton VM's memory.\n\
         #[derive(Debug, Clone, Copy, PartialEq, Eq)]\n\
         pub struct VmProofIter {{\n{fields}\n}}\n\
         impl VmProofIter {{\n    pub fn new() -> Self {{ __stub() }}\n{next_as_methods}\n}}\n"
    )
}

/// Generate stubs for all `tasm::tasmlib_*` snippets that the program uses.
fn tasm_lib_snippet_stubs(program_source: &str) -> String {
    // The program source is the output of a token stream, which puts spaces
    // around `::`.
    let tasm_fn_regex = Regex::new(&format!(
        r"\b{TASM_MODULE_NAME}\s*::\s*(tasmlib_[A-Za-z0-9_]+)"
    ))
    .unwrap();
    let names: BTreeSet<&str> = tasm_fn_regex
        .captures_iter(program_source)
        .map(|caps| caps.get(1).unwrap().as_str())
        .collect();

    names
        .into_iter()
        .map(|name| {
            let snippet = tasm_lib::exported_snippets::name_to_snippet(name).unwrap_or_else(|| {
                panic!("Program uses `tasm::{name}`, but no such snippet is exported by tasm-lib")
            });
            snippet_stub(name, snippet)
        })
        .join("\n")
}

/// The Rust signature of a `tasm-lib` snippet. Pointer-typed arguments become
/// generic, since the snippets are agnostic about what they point to, and
/// programs refer to the pointed-to values through references or boxes.
fn snippet_stub(
    name: &str,
    snippet: Box<dyn tasm_lib::traits::basic_snippet::BasicSnippet>,
) -> String {
    let mut generics = vec![];
    let mut params = vec![];
    for (i, (dtype, param_name)) in snippet.parameters().into_iter().enumerate() {
        let param_name = format!(
            "arg_{i}_{}",
            param_name.replace(['*', '[', ']'], "").to_lowercase()
        );
        let rust_type = match rust_type_of_tasm_lib_type(&dtype) {
            Some(rust_type) => rust_type,
            None => {
                let generic = format!("P{i}");
                generics.push(generic.clone());
                generic
            }
        };
        params.push(format!("{param_name}: {rust_type}"));
    }

    let return_types = snippet
        .return_values()
        .into_iter()
        .map(|(dtype, _)| match dtype {
            tasm_lib::data_type::DataType::StructRef(struct_type) => {
                format!("Box<{}>", struct_type.name)
            }
            other => rust_type_of_tasm_lib_type(&other).unwrap_or_else(|| {
                panic!("Cannot express return type {other:?} of snippet {name} in Rust")
            }),
        })
        .collect_vec();
    let return_type = match return_types.len() {
        0 => String::new(),
        1 => format!(" -> {}", return_types[0]),
        _ => format!(" -> ({})", return_types.join(", ")),
    };

    let generics = if generics.is_empty() {
        String::new()
    } else {
        format!("<{}>", generics.join(", "))
    };

    format!(
        "    #[allow(non_snake_case)]\n    pub fn {name}{generics}({}){return_type} {{ super::__stub() }}",
        params.join(", ")
    )
}

/// The Rust type corresponding to a `tasm-lib` type, if it can be expressed.
fn rust_type_of_tasm_lib_type(dtype: &tasm_lib::data_type::DataType) -> Option<String> {
    use tasm_lib::data_type::DataType::*;
    let rust_type = match dtype {
        Bool => "bool".to_owned(),
        U32 => "u32".to_owned(),
        U64 => "u64".to_owned(),
        U128 => "u128".to_owned(),
        Bfe => "BFieldElement".to_owned(),
        Xfe => "XFieldElement".to_owned(),
        Digest => "Digest".to_owned(),
        List(elem) => format!("Vec<{}>", rust_type_of_tasm_lib_type(elem)?),
        Array(array) => format!(
            "[{}; {}]",
            rust_type_of_tasm_lib_type(&array.element_type)?,
            array.length
        ),
        Tuple(elems) => format!(
            "({})",
            elems
                .iter()
                .map(rust_type_of_tasm_lib_type)
                .collect::<Option<Vec<_>>>()?
                .join(", ")
        ),
        U160 | U192 | I128 | VoidPointer | StructRef(_) => return None,
    };

    Some(rust_type)
}
