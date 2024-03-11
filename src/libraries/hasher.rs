use tasm_lib::list::LIST_METADATA_SIZE;
use tasm_lib::traits::basic_snippet::BasicSnippet;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::prelude::tip5::DIGEST_LENGTH;

use crate::ast;
use crate::ast::FnSignature;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::composite_types::CompositeTypes;
use crate::graft::Graft;
use crate::libraries::Library;
use crate::subroutine::SubRoutine;
use crate::tasm_code_generator::CompilerState;
use crate::type_checker::CheckState;

use super::bfe::BfeLibrary;
use super::LibraryFunction;

const HASHER_LIB_INDICATOR: &str = "Tip5::";
const STATEFUL_HASHER_LIB_INDICATOR: &str = "Tip5WithState::";
const DEFAULT_DIGEST_FUNCTION: &str = "Digest::default";
const NEW_DIGEST_FUNCTION: &str = "Digest::new";
const SPONGE_HASHER_INIT_NAME: &str = "Tip5WithState::init";
const SPONGE_HASHER_ABSORB_NAME: &str = "Tip5WithState::absorb";
const SPONGE_HASHER_SQUEEZE_NAME: &str = "Tip5WithState::squeeze";
const SPONGE_HASHER_PAD_AND_ABSORB_ALL_NAME: &str = "Tip5WithState::pad_and_absorb_all";
const SAMPLE_SCALARS_FUNCTION_NAME: &str = "Tip5WithState::sample_scalars";
const HASH_PAIR_FUNCTION_NAME: &str = "Tip5::hash_pair";
const HASH_VARLEN_FUNCTION_NAME: &str = "Tip5::hash_varlen";

#[derive(Clone, Debug)]
pub(crate) struct HasherLib;

impl Library for HasherLib {
    fn graft_type(
        &self,
        _graft: &mut Graft,
        _rust_type_as_string: &str,
        _path_args: &syn::PathArguments,
    ) -> Option<DataType> {
        None
    }

    fn handle_function_call(
        &self,
        full_name: &str,
        _qualified_self_type: &Option<DataType>,
    ) -> bool {
        full_name.starts_with(HASHER_LIB_INDICATOR)
            || full_name.starts_with(STATEFUL_HASHER_LIB_INDICATOR)
    }

    fn handle_method_call(&self, _method_name: &str, _receiver_type: &DataType) -> bool {
        false
    }

    fn method_name_to_signature(
        &self,
        _fn_name: &str,
        _receiver_type: &DataType,
        _args: &[ast::Expr<super::Annotation>],
        _type_checker_state: &CheckState,
    ) -> FnSignature {
        panic!("HasherLib does not contain any methods")
    }

    fn function_name_to_signature(
        &self,
        fn_name: &str,
        _type_parameter: Option<DataType>,
        _args: &[ast::Expr<super::Annotation>],
        _qualified_self_type: &Option<DataType>,
        _composite_types: &mut CompositeTypes,
    ) -> ast::FnSignature {
        match fn_name {
            HASH_PAIR_FUNCTION_NAME => hash_pair_function().signature,
            HASH_VARLEN_FUNCTION_NAME => self.hash_varlen_signature(),
            SPONGE_HASHER_INIT_NAME => ast::FnSignature::from_basic_snippet(Box::new(
                tasm_lib::hashing::sponge_hasher::init::Init,
            )),
            SPONGE_HASHER_ABSORB_NAME => ast::FnSignature::from_basic_snippet(Box::new(
                tasm_lib::hashing::sponge_hasher::absorb::Absorb,
            )),
            SPONGE_HASHER_SQUEEZE_NAME => ast::FnSignature::from_basic_snippet(Box::new(
                tasm_lib::hashing::sponge_hasher::squeeze::Squeeze,
            )),
            SPONGE_HASHER_PAD_AND_ABSORB_ALL_NAME => {
                ast::FnSignature::value_function_immutable_args(
                    SPONGE_HASHER_PAD_AND_ABSORB_ALL_NAME,
                    vec![(
                        "input",
                        ast_types::DataType::Boxed(Box::new(ast_types::DataType::List(Box::new(
                            DataType::Bfe,
                        )))),
                    )],
                    DataType::unit(),
                )
            }
            SAMPLE_SCALARS_FUNCTION_NAME => ast::FnSignature::from_basic_snippet(Box::new(
                tasm_lib::hashing::algebraic_hasher::sample_scalars::SampleScalars,
            )),
            _ => panic!("Unknown function {fn_name}"),
        }
    }

    fn call_method(
        &self,
        _method_name: &str,
        _receiver_type: &DataType,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        panic!("HasherLib does not contain any methods")
    }

    fn call_function(
        &self,
        fn_name: &str,
        _type_parameter: Option<DataType>,
        _args: &[ast::Expr<super::Annotation>],
        state: &mut CompilerState,
        _qualified_self_type: &Option<DataType>,
    ) -> Vec<LabelledInstruction> {
        let snippet = name_to_tasm_lib_snippet(fn_name);
        match snippet {
            Some(snippet) => {
                let snippet_label = state.import_snippet(snippet);
                triton_asm!(call { snippet_label })
            }
            None => match fn_name {
                HASH_PAIR_FUNCTION_NAME => {
                    let hash_pair: SubRoutine = hash_pair_function().try_into().unwrap();
                    let hash_pair_label = hash_pair.get_label();
                    state.add_subroutine(hash_pair);

                    triton_asm!(call { hash_pair_label })
                }
                HASH_VARLEN_FUNCTION_NAME => self.hash_varlen_code(state),
                _ => panic!("Unknown function {fn_name}"),
            },
        }
    }

    fn get_graft_function_name(&self, full_name: &str) -> Option<String> {
        const GRAFTED_FUNCTIONS: [&str; 2] = [DEFAULT_DIGEST_FUNCTION, NEW_DIGEST_FUNCTION];
        if GRAFTED_FUNCTIONS.contains(&full_name) {
            return Some(full_name.to_owned());
        }

        None
    }

    fn graft_function(
        &self,
        graft_config: &mut Graft,
        full_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        _function_type_parameter: Option<DataType>,
    ) -> Option<ast::Expr<super::Annotation>> {
        match full_name {
            DEFAULT_DIGEST_FUNCTION => {
                assert!(
                    args.is_empty(),
                    "Digest::default() should not have any arguments"
                );

                Some(ast::Expr::Lit(ast::ExprLit::Digest(Digest::default())))
            }
            NEW_DIGEST_FUNCTION => Some(graft_digest_new(&args[0], graft_config)),
            _ => panic!("HasherLib cannot graft function {full_name}"),
        }
    }

    fn graft_method_call(
        &self,
        _graft_config: &mut Graft,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        None
    }
}

impl HasherLib {
    pub(super) fn hash_varlen_signature(&self) -> ast::FnSignature {
        ast::FnSignature::value_function_immutable_args(
            "hash_varlen",
            vec![(
                "input",
                ast_types::DataType::Boxed(Box::new(ast_types::DataType::List(Box::new(
                    ast_types::DataType::Bfe,
                )))),
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

            push {LIST_METADATA_SIZE + 1}
            add
            swap 1
            // _ *elem_0 len


            call { tasm_libs_hash_varlen_label }
            // _ digest

            return
        );
        state.add_subroutine(tasm_langs_hash_varlen.try_into().unwrap());

        triton_asm!(call {
            tasm_langs_hash_varlen_label
        })
    }
}

/// Map hasher functions to the TASM lib snippet type
fn name_to_tasm_lib_snippet(public_name: &str) -> Option<Box<dyn BasicSnippet>> {
    match public_name {
        SPONGE_HASHER_INIT_NAME => Some(Box::new(tasm_lib::hashing::sponge_hasher::init::Init)),
        SPONGE_HASHER_ABSORB_NAME => {
            Some(Box::new(tasm_lib::hashing::sponge_hasher::absorb::Absorb))
        }
        SPONGE_HASHER_SQUEEZE_NAME => {
            Some(Box::new(tasm_lib::hashing::sponge_hasher::squeeze::Squeeze))
        }
        SPONGE_HASHER_PAD_AND_ABSORB_ALL_NAME => Some(Box::new(
            tasm_lib::hashing::sponge_hasher::pad_and_absorb_all::PadAndAbsorbAll,
        )),
        SAMPLE_SCALARS_FUNCTION_NAME => Some(Box::new(
            tasm_lib::hashing::algebraic_hasher::sample_scalars::SampleScalars,
        )),
        _ => None,
    }
}

/// Handle initialization of digests through `Digest::new([BFieldElement::new(4), ...])`
fn graft_digest_new(arg_0: &syn::Expr, graft_config: &mut Graft) -> ast::Expr<super::Annotation> {
    match arg_0 {
        syn::Expr::Array(syn::ExprArray { elems, .. }) => {
            let mut initializer_exprs = vec![];
            for elem in elems {
                match elem {
                    syn::Expr::Call(syn::ExprCall { func, args, .. }) => {
                        let (name, _type_parameter) = match func.as_ref() {
                            syn::Expr::Path(path) => (
                                Graft::path_to_ident(&path.path),
                                graft_config.path_to_type_parameter(&path.path),
                            ),
                            other => panic!("unsupported: {other:?}"),
                        };

                        let bfe_library = BfeLibrary;
                        if let Some(bfe_fn_name) = bfe_library.get_graft_function_name(&name) {
                            initializer_exprs.push(
                                bfe_library
                                    .graft_function(graft_config, &bfe_fn_name, args, None)
                                    .unwrap(),
                            );
                        } else {
                            panic!();
                        }
                    }
                    _ => panic!("unsupported: {elem:?}"),
                }
            }

            let mut bfe_literals = vec![];
            for expr in initializer_exprs {
                match expr {
                    ast::Expr::Lit(ast::ExprLit::Bfe(bfe)) => {
                        bfe_literals.push(bfe);
                    }
                    _ => {
                        unreachable!("BFE grafting must return BFE literals. Got: {:#?}", expr)
                    }
                }
            }

            let bfe_literals: [BFieldElement; DIGEST_LENGTH] =
                bfe_literals.clone().try_into().unwrap_or_else(|_| {
                    panic!(
                        "Digest initialization must happen with {DIGEST_LENGTH} BFEs. Got {}",
                        bfe_literals.len(),
                    )
                });

            ast::Expr::Lit(ast::ExprLit::Digest(Digest::new(bfe_literals)))
        }
        _ => panic!("Digest instantiation must happen with an array"),
    }
}

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
