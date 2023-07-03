use itertools::Itertools;
use triton_vm::triton_asm;
use twenty_first::shared_math::{
    b_field_element::BFieldElement,
    x_field_element::{XFieldElement, EXTENSION_DEGREE},
};

use crate::{
    ast,
    graft::{self, graft_expr},
    libraries::Library,
};

use super::{bfe::BfeLibrary, CompiledFunction};
const XFIELDELEMENT_LIB_INDICATOR: &str = "XFieldElement::";

#[derive(Clone, Debug)]
pub struct XfeLibrary;

impl Library for XfeLibrary {
    fn get_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn get_method_name(&self, method_name: &str, _receiver_type: &ast::DataType) -> Option<String> {
        if method_name == "unlift" {
            Some(method_name.to_owned())
        } else {
            None
        }
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        _receiver_type: &ast::DataType,
        args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        if method_name == "unlift" {
            get_xfe_unlift_method().signature
        } else {
            panic!("Unknown method in XFE library. Got: {method_name}");
        }
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast::DataType>,
    ) -> ast::FnSignature {
        panic!("No functions implemented for XFE library");
    }

    fn call_method(
        &self,
        method_name: &str,
        _receiver_type: &ast::DataType,
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        if method_name == "unlift" {
            get_xfe_unlift_method().body
        } else {
            panic!("Unknown method in XFE library. Got: {method_name}");
        }
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast::DataType>,
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        panic!("No functions implemented for XFE library");
    }

    fn get_graft_function_name(&self, full_name: &str) -> Option<String> {
        if !full_name.starts_with(XFIELDELEMENT_LIB_INDICATOR) {
            return None;
        }

        let stripped_name = &full_name[XFIELDELEMENT_LIB_INDICATOR.len()..full_name.len()];

        if stripped_name != "new" {
            return None;
        }

        Some(stripped_name.to_owned())
    }

    fn graft_function(
        &self,
        fn_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
    ) -> Option<ast::Expr<super::Annotation>> {
        if fn_name != "new" {
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
                                    graft::path_to_ident(&path.path),
                                    graft::path_to_type_parameter(&path.path),
                                ),
                                other => panic!("unsupported: {other:?}"),
                            };

                            if let Some(bfe_fn_name) = BfeLibrary.get_graft_function_name(&name) {
                                initializer_exprs
                                    .push(BfeLibrary.graft_function(&bfe_fn_name, args).unwrap());
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
                            unreachable!("BFE grafting must return BFE literals. Got: {:#?}", expr)
                        }
                    }
                }

                let bfe_literals: [BFieldElement; EXTENSION_DEGREE] =
                    bfe_literals.try_into().unwrap();
                Some(ast::Expr::Lit(ast::ExprLit::XFE(XFieldElement::new(
                    bfe_literals,
                ))))
            }
            _ => panic!("XFE instantiation must happen with an array"),
        }
    }

    fn graft_method(
        &self,
        rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::MethodCall<super::Annotation>> {
        // Handle `unlift().unwrap()`. Ignore everything else.
        const UNWRAP_NAME: &str = "unwrap";
        const UNLIFT_NAME: &str = "unlift";

        let last_method_name = rust_method_call.method.to_string();

        if last_method_name != UNWRAP_NAME {
            return None;
        }

        match rust_method_call.receiver.as_ref() {
            syn::Expr::MethodCall(rust_inner_method_call) => {
                let inner_method_call = graft::graft_method_call(rust_inner_method_call);
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
                        .map(graft_expr)
                        .collect_vec(),
                );
                let annot = Default::default();

                Some(ast::MethodCall {
                    method_name: UNLIFT_NAME.to_owned(),
                    args,
                    annot,
                })
            }
            _ => todo!(),
        }
    }
}

fn get_xfe_unlift_method() -> CompiledFunction {
    let fn_signature = ast::FnSignature {
        name: "unlift".to_owned(),
        args: vec![ast::AbstractArgument::ValueArgument(
            ast::AbstractValueArg {
                name: "value".to_owned(),
                data_type: ast::DataType::XFE,
                mutable: false,
            },
        )],
        output: ast::DataType::BFE,
        arg_evaluation_order: Default::default(),
    };

    CompiledFunction {
        signature: fn_signature,
        body: triton_asm!(swap 2 push 0 eq assert push 0 eq assert),
    }
}
