use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::prelude::tip5::DIGEST_LENGTH;

use crate::ast;
use crate::ast::FnSignature;
use crate::ast_types::DataType;
use crate::ast_types::ListType;
use crate::graft::Graft;
use crate::libraries::hasher::algebraic_hasher::hash_pair_function;
use crate::libraries::hasher::algebraic_hasher::HASH_PAIR_FUNCTION_NAME;
use crate::libraries::hasher::algebraic_hasher::HASH_VARLEN_FUNCTION_NAME;
use crate::libraries::hasher::sponge_hasher::graft_sponge_hasher_functions;
use crate::libraries::hasher::sponge_hasher::SPONGE_HASHER_INDICATOR;
use crate::libraries::Library;
use crate::subroutine::SubRoutine;
use crate::tasm_code_generator::CompilerState;
use crate::type_checker::CheckState;

use super::bfe::BfeLibrary;

pub(crate) mod algebraic_hasher;
pub(crate) mod sponge_hasher;

const HASHER_LIB_INDICATOR: &str = "H::";
const DEFAULT_DIGEST_FUNCTION: &str = "Digest::default";
const NEW_DIGEST_FUNCTION: &str = "Digest::new";

#[derive(Clone, Debug)]
pub(crate) struct HasherLib {
    pub(crate) list_type: ListType,
}

impl Library for HasherLib {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        if full_name.starts_with(HASHER_LIB_INDICATOR)
            || full_name.starts_with(SPONGE_HASHER_INDICATOR)
        {
            return Some(full_name.to_owned());
        }

        None
    }

    fn get_method_name(&self, _method_name: &str, _receiver_type: &DataType) -> Option<String> {
        None
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
    ) -> ast::FnSignature {
        if fn_name == HASH_PAIR_FUNCTION_NAME {
            return hash_pair_function().signature;
        }

        if fn_name == HASH_VARLEN_FUNCTION_NAME {
            return self.hash_varlen_signature();
        }

        panic!("Unknown function {fn_name}");
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
    ) -> Vec<LabelledInstruction> {
        if fn_name == HASH_PAIR_FUNCTION_NAME {
            let hash_pair: SubRoutine = hash_pair_function().try_into().unwrap();
            let hash_pair_label = hash_pair.get_label();
            state.add_library_function(hash_pair);

            return triton_asm!(call { hash_pair_label });
        }

        if fn_name == HASH_VARLEN_FUNCTION_NAME {
            return self.hash_varlen_code(state);
        }

        panic!("Unknown function {fn_name}");
    }

    fn get_graft_function_name(&self, full_name: &str) -> Option<String> {
        if full_name == DEFAULT_DIGEST_FUNCTION || full_name == NEW_DIGEST_FUNCTION {
            return Some(full_name.to_owned());
        }

        if full_name.starts_with(SPONGE_HASHER_INDICATOR) {
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
        if full_name == DEFAULT_DIGEST_FUNCTION {
            assert!(
                args.is_empty(),
                "Digest::default() should not have any arguments"
            );

            return Some(ast::Expr::Lit(ast::ExprLit::Digest(Digest::default())));
        }

        if full_name == NEW_DIGEST_FUNCTION {
            return Some(graft_digest_new(&args[0], graft_config));
        }

        if full_name.starts_with(SPONGE_HASHER_INDICATOR) {
            return Some(graft_sponge_hasher_functions(graft_config, full_name, args));
        }

        panic!("HasherLib cannot graft function {full_name}")
    }

    fn graft_method(
        &self,
        _graft_config: &mut Graft,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        None
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

                        let bfe_library = BfeLibrary {
                            list_type: graft_config.list_type,
                        };
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
