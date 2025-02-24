use std::fmt::Display;

use anyhow::bail;
use regex::Regex;

use crate::ast;
use crate::ast::ArgEvaluationOrder;
use crate::ast::AsmDefinedBody;
use crate::ast::FnSignature;
use crate::ast::RoutineBody;
use crate::ast_types::AbstractArgument;
use crate::ast_types::AbstractValueArg;
use crate::ast_types::CustomTypeOil;
use crate::ast_types::DataType;
use crate::ast_types::StructType;
use crate::ast_types::StructVariant;
use crate::composite_types::TypeContext;
use crate::triton_vm::isa::triton_asm;

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

pub(super) fn polynomial_type(coefficient_type: PolynomialCoefficientType) -> TypeContext {
    let coefficient_list_type = DataType::List(Box::new(coefficient_type.into()));
    let self_type = StructType {
        name: "Polynomial".to_owned(),
        is_copy: false,
        variant: StructVariant::named_fields(vec![(
            "coefficients".to_owned(),
            coefficient_list_type.clone(),
        )]),
    };

    let composite_type = CustomTypeOil::Struct(self_type.clone());
    let methods = vec![ast::Method {
        signature: FnSignature {
            name: "coefficients".to_string(),
            args: vec![AbstractArgument::ValueArgument(AbstractValueArg {
                name: "self".to_string(),
                data_type: DataType::Struct(self_type),
                mutable: false,
            })],
            output: coefficient_list_type,
            arg_evaluation_order: ArgEvaluationOrder::default(),
        },
        body: RoutineBody::Instructions(AsmDefinedBody {
            dependencies: vec![],
            instructions: triton_asm!(addi 1), // hacky as hell… but how to do it properly?
        }),
    }];
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
