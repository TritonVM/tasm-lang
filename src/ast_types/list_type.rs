use std::fmt::Display;
use tasm_lib::traits::basic_snippet::BasicSnippet;

use super::DataType;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) enum ListType {
    // The `Safe` list type is currently only used in tests. So we
    // must allow dead code here, as the compiler will otherwise
    // warn about that variant never being constructed.
    #[allow(dead_code)]
    Safe,
    Unsafe,
}

impl ListType {
    pub(crate) fn with_capacity_snippet(&self, type_parameter: DataType) -> Box<dyn BasicSnippet> {
        let tasm_type: tasm_lib::data_type::DataType = type_parameter.try_into().unwrap();
        match self {
            Self::Safe => Box::new(tasm_lib::list::safeimplu32::new::SafeNew {
                data_type: tasm_type,
            }),
            Self::Unsafe => Box::new(tasm_lib::list::unsafeimplu32::new::UnsafeNew {
                data_type: tasm_type,
            }),
        }
    }

    pub(crate) fn push_snippet(&self, type_parameter: DataType) -> Box<dyn BasicSnippet> {
        let tasm_type: tasm_lib::data_type::DataType = type_parameter.try_into().unwrap();
        match self {
            Self::Safe => Box::new(tasm_lib::list::safeimplu32::push::SafePush {
                data_type: tasm_type,
            }),
            Self::Unsafe => Box::new(tasm_lib::list::unsafeimplu32::push::UnsafePush {
                data_type: tasm_type,
            }),
        }
    }

    pub(crate) fn pop_snippet(&self, type_parameter: DataType) -> Box<dyn BasicSnippet> {
        let tasm_type: tasm_lib::data_type::DataType = type_parameter.try_into().unwrap();
        match self {
            Self::Safe => Box::new(tasm_lib::list::safeimplu32::pop::SafePop {
                data_type: tasm_type,
            }),
            Self::Unsafe => Box::new(tasm_lib::list::unsafeimplu32::pop::UnsafePop {
                data_type: tasm_type,
            }),
        }
    }
}

impl From<ListType> for tasm_lib::list::ListType {
    fn from(value: ListType) -> Self {
        match value {
            ListType::Safe => tasm_lib::list::ListType::Safe,
            ListType::Unsafe => tasm_lib::list::ListType::Unsafe,
        }
    }
}

impl ListType {
    pub(crate) fn metadata_size(&self) -> usize {
        match self {
            ListType::Safe => 2,
            ListType::Unsafe => 1,
        }
    }
}

impl Display for ListType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Safe => "safe",
                Self::Unsafe => "unsafe",
            }
        )
    }
}
