use std::fmt::Display;

use anyhow::bail;

use crate::ast::Expr;
use crate::ast::FnSignature;
use crate::ast_types::DataType;
use crate::composite_types;
use crate::graft;
use crate::tasm_code_generator;
use crate::type_checker;

use super::Annotation;
use super::Library;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum PolynomialCoefficientType {
    Bfe,
    Xfe,
}

impl TryFrom<DataType> for PolynomialCoefficientType {
    type Error = anyhow::Error;

    fn try_from(value: DataType) -> Result<Self, Self::Error> {
        match value {
            DataType::Bfe => Ok(Self::Bfe),
            DataType::Xfe => Ok(Self::Xfe),
            _ => bail!("Can only handle polynomials over BFEs or XFEs"),
        }
    }
}

impl Display for PolynomialCoefficientType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PolynomialCoefficientType::Bfe => write!(f, "BFieldElement"),
            PolynomialCoefficientType::Xfe => write!(f, "XFieldElement"),
        }
    }
}

impl PolynomialCoefficientType {
    pub(crate) fn label_friendly_name(&self) -> String {
        match self {
            PolynomialCoefficientType::Bfe => "bfe".to_owned(),
            PolynomialCoefficientType::Xfe => "xfe".to_owned(),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PolynomialLib;

impl Library for PolynomialLib {
    fn graft_type(
        &self,
        graft: &mut crate::graft::Graft,
        rust_type_as_string: &str,
        path_args: &syn::PathArguments,
    ) -> Option<DataType> {
        None
    }

    fn handle_function_call(
        &self,
        full_name: &str,
        qualified_self_type: &Option<DataType>,
    ) -> bool {
        false
    }

    fn handle_method_call(&self, method_name: &str, receiver_type: &DataType) -> bool {
        false
    }

    fn method_name_to_signature(
        &self,
        fn_name: &str,
        receiver_type: &DataType,
        args: &[Expr<Annotation>],
        type_checker_state: &type_checker::CheckState,
    ) -> FnSignature {
        todo!()
    }

    fn function_name_to_signature(
        &self,
        fn_name: &str,
        type_parameter: Option<DataType>,
        args: &[Expr<Annotation>],
        qualified_self_type: &Option<DataType>,
        composite_types: &mut composite_types::CompositeTypes,
    ) -> FnSignature {
        todo!()
    }

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &DataType,
        args: &[Expr<Annotation>],
        state: &mut tasm_code_generator::CompilerState,
    ) -> Vec<tasm_lib::prelude::triton_vm::prelude::LabelledInstruction> {
        todo!()
    }

    fn call_function(
        &self,
        fn_name: &str,
        type_parameter: Option<DataType>,
        args: &[Expr<Annotation>],
        state: &mut tasm_code_generator::CompilerState,
        qualified_self_type: &Option<DataType>,
    ) -> Vec<tasm_lib::prelude::triton_vm::prelude::LabelledInstruction> {
        todo!()
    }

    fn get_graft_function_name(&self, full_name: &str) -> Option<String> {
        None
    }

    fn graft_function(
        &self,
        graft_config: &mut graft::Graft,
        fn_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        type_parameter: Option<DataType>,
    ) -> Option<Expr<Annotation>> {
        None
    }

    fn graft_method_call(
        &self,
        graft_config: &mut graft::Graft,
        rust_method_call: &syn::ExprMethodCall,
    ) -> Option<Expr<Annotation>> {
        None
    }
}
