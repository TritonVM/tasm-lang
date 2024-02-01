use itertools::Itertools;
use tasm_lib::traits::basic_snippet::BasicSnippet;

use super::HasherLib;
use crate::ast;
use crate::ast::FnCall;
use crate::ast_types;
use crate::graft::Annotation;
use crate::graft::Graft;
use crate::libraries::LibraryFunction;
use crate::tasm_code_generator::CompilerState;
use crate::triton_vm::prelude::*;
use crate::LabelledInstruction;

pub(super) const HASH_PAIR_FUNCTION_NAME: &str = "Tip5::hash_pair";
pub(super) const HASH_VARLEN_FUNCTION_NAME: &str = "Tip5::hash_varlen";
pub(super) const SAMPLE_SCALARS_FUNCTION_NAME: &str = "Tip5::sample_scalars";

pub(super) fn hash_pair_function() -> LibraryFunction {
    let fn_signature = ast::FnSignature {
        name: "hash_pair".to_owned(),
        args: vec![
            ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
                name: "left".to_owned(),
                data_type: ast_types::DataType::Digest,
                mutable: false,
            }),
            ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
                name: "right".to_owned(),
                data_type: ast_types::DataType::Digest,
                mutable: false,
            }),
        ],
        output: ast_types::DataType::Digest,
        // If the definition of Tip5's `hash_pair` was changed, this could
        // be left-to-right instead
        arg_evaluation_order: ast::ArgEvaluationOrder::RightToLeft,
    };

    LibraryFunction {
        signature: fn_signature,
        body: triton_asm!(hash),
    }
}

pub(super) fn graft_sample_scalars_function_call(
    grafter: &mut Graft,
    args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
) -> ast::Expr<Annotation> {
    // From the perspective of the VM, the Sponge state is handled through its own co-processor,
    // so it lives neither in memory, nor on the stack.
    let [_state, arg] = args.iter().collect_vec()[..] else {
        panic!("{SAMPLE_SCALARS_FUNCTION_NAME} expects exactly two arguments");
    };
    let arg = grafter.graft_expr(arg);

    let tasm_lib_snippet = tasm_lib::hashing::algebraic_hasher::sample_scalars::SampleScalars;
    let entrypoint = tasm_lib_snippet.entrypoint();
    ast::Expr::FnCall(FnCall {
        name: format!("tasm::{entrypoint}"),
        args: vec![arg],
        type_parameter: None,
        arg_evaluation_order: Default::default(),
        annot: Default::default(),
    })
}

impl HasherLib {
    pub(super) fn hash_varlen_signature(&self) -> ast::FnSignature {
        ast::FnSignature::value_function_immutable_args(
            "hash_varlen",
            vec![(
                "input",
                ast_types::DataType::Boxed(Box::new(ast_types::DataType::List(
                    Box::new(ast_types::DataType::Bfe),
                    self.list_type,
                ))),
            )],
            ast_types::DataType::Digest,
        )
    }

    pub(super) fn hash_varlen_code(&self, state: &mut CompilerState) -> Vec<LabelledInstruction> {
        // This is just a thin wrapper around `tasm-lib`'s `hash_varlen`, such that
        // you can call `Tip5::hash_varlen(&bfes)`, where `bfes` has to be a list of
        // `BFieldElement`s, no other element type works.
        let tasm_libs_hash_varlen_label = state.import_snippet(Box::new(
            tasm_lib::hashing::algebraic_hasher::hash_varlen::HashVarlen,
        ));
        let tasm_langs_hash_varlen_label = "tasm_langs_hash_varlen".to_owned();

        let tasm_langs_hash_varlen = triton_asm!(
            {tasm_langs_hash_varlen_label}:
            // _ *list

            read_mem 1
            // _ len (*list - 1)

            push {self.list_type.metadata_size() + 1}
            add
            swap 1
            // _ *elem_0 len


            call { tasm_libs_hash_varlen_label }
            // _ digest

            return
        );
        state.add_library_function(tasm_langs_hash_varlen.try_into().unwrap());

        triton_asm!(call {
            tasm_langs_hash_varlen_label
        })
    }
}
