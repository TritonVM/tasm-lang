use itertools::Itertools;
use triton_vm::triton_asm;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{ast, graft::graft_expr};

use super::{CompiledFunction, Library};

const BFIELDELEMENT_LIB_INDICATOR: &str = "BFieldElement::";

#[derive(Clone, Debug)]
pub struct BfeLibrary;

impl Library for BfeLibrary {
    fn get_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn get_method_name(&self, method_name: &str, _receiver_type: &ast::DataType) -> Option<String> {
        if matches!(method_name, "lift") {
            Some(method_name.to_owned())
        } else {
            None
        }
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        _receiver_type: &ast::DataType,
    ) -> ast::FnSignature {
        if method_name != "lift" {
            panic!("Unknown method {method_name} for BFE");
        }

        get_bfe_lift_method().signature
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast::DataType>,
    ) -> ast::FnSignature {
        panic!("No functions implemented for BFE library");
    }

    fn call_method(
        &self,
        method_name: &str,
        _receiver_type: &ast::DataType,
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        if method_name != "lift" {
            panic!("Unknown method {method_name} for BFE");
        }

        get_bfe_lift_method().body
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast::DataType>,
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        panic!("No functions implemented for BFE library");
    }

    fn get_graft_function_name(&self, full_name: &str) -> Option<String> {
        if !full_name.starts_with(BFIELDELEMENT_LIB_INDICATOR) {
            return None;
        }

        let stripped_name = &full_name[BFIELDELEMENT_LIB_INDICATOR.len()..full_name.len()];

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

        let args = args.iter().map(graft_expr).collect_vec();

        if args.len() == 1 && matches!(args[0], ast::Expr::Lit(_)) {
            if let ast::Expr::Lit(ast::ExprLit::U64(value)) = args[0] {
                Some(ast::Expr::Lit(ast::ExprLit::BFE(BFieldElement::new(value))))
            } else if let ast::Expr::Lit(ast::ExprLit::GenericNum(value, _)) = args[0] {
                Some(ast::Expr::Lit(ast::ExprLit::BFE(BFieldElement::new(value))))
            } else {
                panic!("Can only instantiate BFE with u64-literal. Please use casting for conversion to `BFieldElement`. Got: {:#?}", args[0]);
            }
        } else {
            panic!("Unknown initialization expression for BFE");
        }
    }

    fn graft_method(
        &self,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::MethodCall<super::Annotation>> {
        None
    }
}

fn get_bfe_lift_method() -> CompiledFunction {
    let fn_signature = ast::FnSignature {
        name: "lift".to_owned(),
        args: vec![ast::FnArg {
            name: "value".to_owned(),
            data_type: ast::DataType::BFE,
            mutable: false,
        }],
        output: ast::DataType::XFE,
        arg_evaluation_order: Default::default(),
    };

    CompiledFunction {
        signature: fn_signature,
        body: triton_asm!(push 0 push 0 swap 2),
    }
}
