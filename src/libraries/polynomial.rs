use std::fmt::Display;

use anyhow::bail;
use regex::Regex;

use crate::ast::Expr;
use crate::ast::FnSignature;
use crate::ast_types::CustomTypeOil;
use crate::ast_types::DataType;
use crate::ast_types::StructType;
use crate::ast_types::StructVariant;
use crate::composite_types;
use crate::composite_types::TypeContext;
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

impl From<PolynomialCoefficientType> for DataType {
    fn from(value: PolynomialCoefficientType) -> Self {
        match value {
            PolynomialCoefficientType::Bfe => DataType::Bfe,
            PolynomialCoefficientType::Xfe => DataType::Xfe,
        }
    }
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

pub(super) fn polynomial_type(coefficient_type: PolynomialCoefficientType) -> TypeContext {
    let composite_type = CustomTypeOil::Struct(StructType {
        name: "Polynomial".to_owned(),
        is_copy: false,
        variant: StructVariant::named_fields(vec![(
            "coefficients".to_owned(),
            DataType::List(Box::new(coefficient_type.into())),
        )]),
    });
    let methods = vec![];
    let associated_functions = vec![];

    TypeContext {
        composite_type,
        methods,
        associated_functions,
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PolynomialLib;

impl PolynomialLib {
    pub(crate) fn try_from_string(value: &str) -> Result<TypeContext, anyhow::Error> {
        let poly_regex = Regex::new(r"Polynomial<(?<inner>.+)>").unwrap();
        let Some(caps) = poly_regex.captures(value) else {
            bail!("String does not match polynomial type");
        };

        // Inner type may only be Xfe or Bfe
        let inner_parsed = DataType::try_from_string(&caps["inner"]).unwrap();
        let coefficient_type: PolynomialCoefficientType = inner_parsed.try_into().unwrap();

        Ok(polynomial_type(coefficient_type))
    }
}

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
