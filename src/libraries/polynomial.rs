use std::fmt::Display;

use anyhow::bail;
use regex::Regex;

use crate::ast_types::CustomTypeOil;
use crate::ast_types::DataType;
use crate::ast_types::StructType;
use crate::ast_types::StructVariant;
use crate::composite_types::TypeContext;

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
