use tasm_lib::Digest;
use triton_vm::{instruction::LabelledInstruction, triton_asm, BFieldElement};
use twenty_first::shared_math::tip5::DIGEST_LENGTH;

use crate::{
    ast, ast_types,
    graft::Graft,
    libraries::bfe::BfeLibrary,
    tasm_code_generator::{subroutine::SubRoutine, CompilerState},
};

use super::{Library, LibraryFunction};

const HASHER_LIB_INDICATOR: &str = "H::";
const HASH_PAIR_FUNCTION_NAME: &str = "H::hash_pair";
const HASH_VARLEN_FUNCTION_NAME: &str = "H::hash_varlen";
const DEFAULT_DIGEST_FUNCTION: &str = "Digest::default";
const NEW_DIGEST_FUNCTION: &str = "Digest::new";

#[derive(Clone, Debug)]
pub struct HasherLib {
    pub list_type: ast_types::ListType,
}

impl Library for HasherLib {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        // Any function call that starts with `H::` is assumed to exist in this library
        if full_name.starts_with(HASHER_LIB_INDICATOR) {
            return Some(full_name.to_owned());
        }

        None
    }

    fn get_method_name(
        &self,
        _method_name: &str,
        _receiver_type: &ast_types::DataType,
    ) -> Option<String> {
        None
    }

    fn method_name_to_signature(
        &self,
        _fn_name: &str,
        _receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature {
        panic!("HasherLib does not contain any methods")
    }

    fn function_name_to_signature(
        &self,
        fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        if fn_name == HASH_PAIR_FUNCTION_NAME {
            return get_hash_pair_function().signature;
        }

        if fn_name == HASH_VARLEN_FUNCTION_NAME {
            return self.hash_varlen_signature();
        }

        panic!("Unknown function {fn_name}");
    }

    fn call_method(
        &self,
        _method_name: &str,
        _receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        panic!("HasherLib does not contain any methods")
    }

    fn call_function(
        &self,
        fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        state: &mut CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        if fn_name == HASH_PAIR_FUNCTION_NAME {
            let hash_pair: SubRoutine = get_hash_pair_function().try_into().unwrap();
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

        None
    }

    fn graft_function(
        &self,
        graft_config: &Graft,
        full_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        _function_type_parameter: Option<ast_types::DataType>,
    ) -> Option<ast::Expr<super::Annotation>> {
        fn graft_digest_new(
            arg_0: &syn::Expr,
            graft_config: &Graft,
        ) -> ast::Expr<super::Annotation> {
            match arg_0 {
                syn::Expr::Array(syn::ExprArray {
                    attrs: _,
                    bracket_token: _,
                    elems,
                }) => {
                    let mut initializer_exprs = vec![];
                    for elem in elems {
                        match elem {
                            syn::Expr::Call(syn::ExprCall {
                                attrs: _,
                                func,
                                paren_token: _,
                                args,
                            }) => {
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
                                if let Some(bfe_fn_name) =
                                    bfe_library.get_graft_function_name(&name)
                                {
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
                            ast::Expr::Lit(ast::ExprLit::BFE(bfe)) => {
                                bfe_literals.push(bfe);
                            }
                            _ => {
                                unreachable!(
                                    "BFE grafting must return BFE literals. Got: {:#?}",
                                    expr
                                )
                            }
                        }
                    }

                    let bfe_literals: [BFieldElement; DIGEST_LENGTH] =
                        bfe_literals.clone().try_into().unwrap_or_else(|_| {
                            panic!(
                            "Digest initialization must happen with {DIGEST_LENGTH} BFEs. Got {}",
                            bfe_literals.len(),)
                        });

                    ast::Expr::Lit(ast::ExprLit::Digest(Digest::new(bfe_literals)))
                }
                _ => panic!("Digest instantiation must happen with an array"),
            }
        }

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

        panic!("HasherLib cannot graft function {full_name}")
    }

    fn graft_method(
        &self,
        _graft_config: &Graft,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        None
    }
}

impl HasherLib {
    fn hash_varlen_signature(&self) -> ast::FnSignature {
        ast::FnSignature {
            name: "hash_varlen".to_owned(),
            args: vec![ast_types::AbstractArgument::ValueArgument(
                ast_types::AbstractValueArg {
                    name: "list".to_owned(),
                    data_type: ast_types::DataType::Reference(Box::new(ast_types::DataType::List(
                        Box::new(ast_types::DataType::BFE),
                        self.list_type,
                    ))),
                    mutable: false,
                },
            )],
            output: ast_types::DataType::Digest,
            arg_evaluation_order: Default::default(),
        }
    }

    fn hash_varlen_code(&self, state: &mut CompilerState) -> Vec<LabelledInstruction> {
        // This is just a thin wrapper around `tasm-lib`'s `hash_varlen`
        let tasm_libs_hash_varlen_label =
            state.import_snippet(Box::new(tasm_lib::hashing::hash_varlen::HashVarlen));
        let tasm_langs_hash_varlen_label = "tasm_langs_hash_varlen".to_owned();

        let tasm_langs_hash_varlen = triton_asm!(
            {tasm_langs_hash_varlen_label}:
            // _ *list

            read_mem
            // _ *list len

            swap 1
            push {self.list_type.metadata_size()}
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

fn get_hash_pair_function() -> LibraryFunction {
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
        body: triton_asm!(hash pop pop pop pop pop),
    }
}
