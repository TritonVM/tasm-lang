use itertools::Itertools;
use num::One;
use num::Zero;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::prelude::x_field_element::EXTENSION_DEGREE;

use crate::ast;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::graft::Graft;
use crate::libraries::Library;
use crate::tasm_code_generator::CompilerState;

use super::bfe::BfeLibrary;
use super::LibraryFunction;

const XFIELDELEMENT_LIB_INDICATOR: &str = "XFieldElement::";
const ZERO_CONST_NAME: &str = "XFieldElement::zero";
const ONE_CONST_NAME: &str = "XFieldElement::one";
const FUNCTION_NAME_NEW: &str = "XFieldElement::new";
const UNLIFT_NAME: &str = "unlift";
const METHOD_NAME_MOD_POW_U32: &str = "mod_pow_u32";
const INVERSE_METHOD_NAME: &str = "inverse";

#[derive(Clone, Debug)]
pub(crate) struct XfeLibrary;

fn xfe_lib_has_method(method_name: &str) -> bool {
    method_name == UNLIFT_NAME
        || method_name == METHOD_NAME_MOD_POW_U32
        || method_name == INVERSE_METHOD_NAME
}

fn method_name_to_signature_inner(method_name: &str) -> ast::FnSignature {
    match method_name {
        UNLIFT_NAME => xfe_unlift_method().signature,
        METHOD_NAME_MOD_POW_U32 => ast::FnSignature::value_function_immutable_args(
            METHOD_NAME_MOD_POW_U32,
            vec![("base", DataType::Xfe), ("exponent", DataType::U32)],
            DataType::Xfe,
        ),
        INVERSE_METHOD_NAME => ast::FnSignature::value_function_immutable_args(
            "xfe_inverse",
            vec![("self", ast_types::DataType::Xfe)],
            ast_types::DataType::Xfe,
        ),
        _ => panic!("XFE library does not know method {method_name}"),
    }
}

fn call_method_inner(method_name: &str, state: &mut CompilerState) -> Vec<LabelledInstruction> {
    match method_name {
        UNLIFT_NAME => xfe_unlift_method().body,
        METHOD_NAME_MOD_POW_U32 => {
            let xfe_mod_pow_u32_generic_label = state.import_snippet(Box::new(
                tasm_lib::arithmetic::xfe::mod_pow_u32_generic::XfeModPowU32Generic,
            ));

            triton_asm!(
                // _ [base] exponent

                swap 3
                swap 2
                swap 1
                // _ exponent [base]

                call {xfe_mod_pow_u32_generic_label}

                // _ [result]
            )
        }
        INVERSE_METHOD_NAME => {
            triton_asm!(xinvert)
        }
        _ => panic!("XFE library does not know method {method_name}"),
    }
}

impl Library for XfeLibrary {
    fn get_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn get_method_name(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
    ) -> Option<String> {
        if *receiver_type == ast_types::DataType::Xfe && xfe_lib_has_method(method_name) {
            return Some(method_name.to_owned());
        }

        None
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        _receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature {
        method_name_to_signature_inner(method_name)
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        panic!("No functions implemented for XFE library");
    }

    fn call_method(
        &self,
        method_name: &str,
        _receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<LabelledInstruction> {
        call_method_inner(method_name, state)
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<LabelledInstruction> {
        panic!("No functions implemented for XFE library");
    }

    fn get_graft_function_name(&self, full_name: &str) -> Option<String> {
        if !full_name.starts_with(XFIELDELEMENT_LIB_INDICATOR) {
            return None;
        }

        if full_name == FUNCTION_NAME_NEW
            || full_name == ZERO_CONST_NAME
            || full_name == ONE_CONST_NAME
        {
            return Some(full_name.to_owned());
        }

        None
    }

    fn graft_function(
        &self,
        graft_config: &mut Graft,
        fn_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        _function_type_parameter: Option<ast_types::DataType>,
    ) -> Option<ast::Expr<super::Annotation>> {
        if fn_name == ZERO_CONST_NAME {
            assert!(args.len().is_zero(), "{ZERO_CONST_NAME} takes no arguments");
            return Some(ast::Expr::Lit(ast::ExprLit::Xfe(XFieldElement::zero())));
        }

        if fn_name == ONE_CONST_NAME {
            assert!(args.len().is_zero(), "{ONE_CONST_NAME} takes no arguments");
            return Some(ast::Expr::Lit(ast::ExprLit::Xfe(XFieldElement::one())));
        }

        if fn_name != FUNCTION_NAME_NEW {
            return None;
        }

        graft_xfe_new_function_call(args, graft_config)
    }

    fn graft_method_call(
        &self,
        graft_config: &mut Graft,
        rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        // Handle `unlift().unwrap()`. Ignore everything else.
        const UNWRAP_NAME: &str = "unwrap";
        const UNLIFT_NAME: &str = "unlift";

        let last_method_name = rust_method_call.method.to_string();

        if last_method_name != UNWRAP_NAME {
            return None;
        }

        match rust_method_call.receiver.as_ref() {
            syn::Expr::MethodCall(rust_inner_method_call) => {
                let inner_method_call = graft_config.graft_method_call(rust_inner_method_call);
                let inner_method_call = match inner_method_call {
                    ast::Expr::MethodCall(mc) => mc,
                    _ => return None,
                };
                if inner_method_call.method_name != UNLIFT_NAME {
                    return None;
                }

                let identifier = match &inner_method_call.args[0] {
                    ast::Expr::Var(ident) => ident.to_owned(),
                    // Maybe cover more cases here?
                    _ => todo!(),
                };

                let mut args = vec![ast::Expr::Var(identifier)];
                args.append(
                    &mut rust_inner_method_call
                        .args
                        .iter()
                        .map(|x| graft_config.graft_expr(x))
                        .collect_vec(),
                );

                Some(ast::Expr::MethodCall(ast::MethodCall {
                    method_name: UNLIFT_NAME.to_owned(),
                    args,
                    annot: Default::default(),
                    associated_type: None,
                }))
            }
            _ => None,
        }
    }
}

fn graft_xfe_new_function_call(
    args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
    graft_config: &mut Graft<'_>,
) -> Option<ast::Expr<crate::type_checker::Typing>> {
    if args.len() != 1 {
        panic!("XFE instantiation only takes one argument which must be an array");
    }

    match &args[0] {
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

            let bfe_literals: [BFieldElement; EXTENSION_DEGREE] = bfe_literals.try_into().unwrap();
            Some(ast::Expr::Lit(ast::ExprLit::Xfe(XFieldElement::new(
                bfe_literals,
            ))))
        }
        _ => panic!("XFE instantiation must happen with an array"),
    }
}

fn xfe_unlift_method() -> LibraryFunction {
    let signature = ast::FnSignature::value_function_immutable_args(
        "unlift",
        vec![("value", ast_types::DataType::Xfe)],
        ast_types::DataType::Bfe,
    );

    LibraryFunction {
        signature,
        body: triton_asm!(swap 2 push 0 eq assert push 0 eq assert),
    }
}
