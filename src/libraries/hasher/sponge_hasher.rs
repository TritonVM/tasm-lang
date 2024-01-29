use itertools::Itertools;
use num::One;
use tasm_lib::traits::basic_snippet::BasicSnippet;

use crate::ast;
use crate::ast::FnCall;
use crate::ast::UnaryOp;
use crate::ast_types::DataType;
use crate::graft::Graft;
use crate::libraries::Annotation;

pub(super) const SPONGE_HASHER_INDICATOR: &str = "Tip5::";
const SPONGE_HASHER_INIT_NAME: &str = "Tip5::init";
const SPONGE_HASHER_ABSORB_NAME: &str = "Tip5::absorb";
const SPONGE_HASHER_SQUEEZE_NAME: &str = "Tip5::squeeze";
const SPONGE_HASHER_PAD_AND_ABSORB_ALL_NAME: &str = "Tip5::pad_and_absorb_all";

pub(super) fn graft_sponge_hasher_functions(
    grafter: &mut Graft,
    full_name: &str,
    args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
) -> ast::Expr<Annotation> {
    match full_name {
        SPONGE_HASHER_INIT_NAME => graft_init_function_call(args),
        SPONGE_HASHER_ABSORB_NAME => graft_absorb_once_function_call(grafter, args),
        SPONGE_HASHER_SQUEEZE_NAME => graft_squeeze_once_function_call(args),
        SPONGE_HASHER_PAD_AND_ABSORB_ALL_NAME => {
            graft_pad_and_absorb_all_function_call(grafter, args)
        }
        _ => panic!("Cannot graft function with name {full_name}"),
    }
}

fn graft_init_function_call(
    args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
) -> ast::Expr<Annotation> {
    assert!(
        args.is_empty(),
        "{SPONGE_HASHER_INIT_NAME} does not take any arguments. Got: args:\n{args:?}",
    );

    let tasm_lib_snippet = tasm_lib::hashing::sponge_hasher::init::Init;
    ast::Expr::FnCall(FnCall {
        name: format!("tasm::{}", tasm_lib_snippet.entrypoint()),
        args: vec![],
        type_parameter: None,
        arg_evaluation_order: Default::default(),
        annot: crate::type_checker::Typing::KnownType(DataType::unit()),
    })
}

fn graft_absorb_once_function_call(
    grafter: &mut Graft,
    args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
) -> ast::Expr<Annotation> {
    // From the perspective of the VM, the Sponge state is handled through its own co-processor.
    let [_state, arg] = args.iter().collect_vec()[..] else {
        panic!("{SPONGE_HASHER_ABSORB_NAME} expects exactly two arguments");
    };
    let arg = grafter.graft_expr(arg);

    let tasm_lib_snippet = tasm_lib::hashing::sponge_hasher::absorb::Absorb;
    let entrypoint = tasm_lib_snippet.entrypoint();

    ast::Expr::FnCall(FnCall {
        name: format!("tasm::{entrypoint}"),
        args: vec![arg],
        type_parameter: None,
        arg_evaluation_order: Default::default(),
        annot: Default::default(),
    })
}

fn graft_squeeze_once_function_call(
    args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
) -> ast::Expr<Annotation> {
    // Ignore 1st argument, as it's the `SpongeState` in the source code, but from
    // the perspective of the VM, this value is handled through its own co-processor.
    assert!(
        args.len().is_one(),
        "{SPONGE_HASHER_SQUEEZE_NAME} expects exactly one argument"
    );

    let tasm_lib_snippet = tasm_lib::hashing::sponge_hasher::squeeze::Squeeze;
    ast::Expr::FnCall(FnCall {
        name: format!("tasm::{}", tasm_lib_snippet.entrypoint()),
        args: vec![],
        type_parameter: None,
        arg_evaluation_order: Default::default(),
        annot: Default::default(),
    })
}

fn graft_pad_and_absorb_all_function_call(
    grafter: &mut Graft,
    args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
) -> ast::Expr<Annotation> {
    let [_state, arg] = args.iter().collect_vec()[..] else {
        panic!("{SPONGE_HASHER_PAD_AND_ABSORB_ALL_NAME} expects exactly two arguments");
    };

    let tasm_lib_snippet = tasm_lib::hashing::sponge_hasher::pad_and_absorb_all::PadAndAbsorbAll {
        list_type: grafter.list_type.into(),
    };

    // Ignore 1st argument, as it's the `SpongeState` in the source code, but from
    // the perspective of the VM, this value is handled through its own co-processor.
    let ast::Expr::Unary(UnaryOp::Ref(_mutable), arg, _) = grafter.graft_expr(arg) else {
        panic!("Incorrect argument to {SPONGE_HASHER_PAD_AND_ABSORB_ALL_NAME}. Got:\n{args:?}");
    };

    ast::Expr::FnCall(FnCall {
        name: format!("tasm::{}", tasm_lib_snippet.entrypoint()),
        args: vec![*arg.to_owned()],
        type_parameter: None,
        arg_evaluation_order: Default::default(),
        annot: Default::default(),
    })
}
