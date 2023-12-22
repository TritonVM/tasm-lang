use itertools::Itertools;
use num::{One, Zero};
use triton_vm::triton_asm;
use twenty_first::shared_math::{
    b_field_element::BFieldElement,
    x_field_element::{XFieldElement, EXTENSION_DEGREE},
};

use crate::{ast, ast_types, graft::Graft, libraries::Library};

use super::{bfe::BfeLibrary, LibraryFunction};
const XFIELDELEMENT_LIB_INDICATOR: &str = "XFieldElement::";
const ZERO_CONST_NAME: &str = "XFieldElement::zero";
const ONE_CONST_NAME: &str = "XFieldElement::one";
const FUNCTION_NAME_NEW: &str = "XFieldElement::new";
const UNLIFT_NAME: &str = "unlift";

#[derive(Clone, Debug)]
pub struct XfeLibrary;

impl Library for XfeLibrary {
    fn get_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn get_method_name(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
    ) -> Option<String> {
        if *receiver_type == ast_types::DataType::Xfe && method_name == UNLIFT_NAME {
            return Some(method_name.to_owned());
        }

        None
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature {
        if *receiver_type == ast_types::DataType::Xfe && method_name == UNLIFT_NAME {
            get_xfe_unlift_method().signature
        } else {
            panic!("Unknown method in XFE library. Got: {method_name}");
        }
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
        receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        if *receiver_type == ast_types::DataType::Xfe && method_name == UNLIFT_NAME {
            get_xfe_unlift_method().body
        } else {
            panic!("Unknown method in XFE library. Got: {method_name}");
        }
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
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

                let bfe_literals: [BFieldElement; EXTENSION_DEGREE] =
                    bfe_literals.try_into().unwrap();
                Some(ast::Expr::Lit(ast::ExprLit::Xfe(XFieldElement::new(
                    bfe_literals,
                ))))
            }
            _ => panic!("XFE instantiation must happen with an array"),
        }
    }

    fn graft_method(
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

fn get_xfe_unlift_method() -> LibraryFunction {
    let fn_signature = ast::FnSignature {
        name: "unlift".to_owned(),
        args: vec![ast_types::AbstractArgument::ValueArgument(
            ast_types::AbstractValueArg {
                name: "value".to_owned(),
                data_type: ast_types::DataType::Xfe,
                mutable: false,
            },
        )],
        output: ast_types::DataType::Bfe,
        arg_evaluation_order: Default::default(),
    };

    LibraryFunction {
        signature: fn_signature,
        body: triton_asm!(swap 2 push 0 eq assert push 0 eq assert),
    }
}
