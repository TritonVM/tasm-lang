use anyhow::bail;
use itertools::Itertools;
use std::{collections::HashMap, fmt::Display, str::FromStr};

use crate::ast::FnSignature;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum AbstractArgument {
    FunctionArgument(AbstractFunctionArg),
    ValueArgument(AbstractValueArg),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AbstractFunctionArg {
    pub abstract_name: String,
    pub function_type: FunctionType,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AbstractValueArg {
    pub name: String,
    pub data_type: DataType,
    pub mutable: bool,
}

impl Display for AbstractValueArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.data_type)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, DataType)>,
}

impl From<StructType> for DataType {
    fn from(value: StructType) -> Self {
        DataType::Struct(value)
    }
}

impl From<&StructType> for DataType {
    fn from(value: &StructType) -> Self {
        value.to_owned().into()
    }
}

impl StructType {
    pub fn get_field_type(&self, field_name: &str) -> DataType {
        match self.fields.iter().find(|&field| field.0 == *field_name) {
            // Type of the field is either another struct, or a pointer to a primitive
            // type living in memory. In case of a pointer to a list, that is the same
            // as a list. An unsafe one, though, so we keep the list wrapped in a
            // MemPointer for now.
            // TODO: Once #38 is implemented, we can remove the MemPointer here and
            // use unsafe list type here.
            // Some((_field_name, field_type)) => match field_type {
            //     DataType::Struct(_) => field_type.to_owned(),
            //     // ast_types::DataType::List(_) => item.to_owned(),
            //     // _ => DataType::MemPointer(Box::new(field_type.clone())),
            Some((_field_name, field_type)) => field_type.to_owned(),

            // },
            None => panic!("Struct {} has no field of name {field_name}", self.name),
        }
    }
}

impl Display for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {}",
            self.name,
            self.fields
                .iter()
                .map(|(k, v)| format!("{k} => {v}"))
                .join(",")
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FunctionType {
    pub input_argument: DataType,
    pub output: DataType,
}

impl From<&FnSignature> for DataType {
    fn from(value: &FnSignature) -> Self {
        let mut input_args = vec![];

        for inp in value.args.iter() {
            let input = match inp {
                AbstractArgument::FunctionArgument(_) => todo!(),
                AbstractArgument::ValueArgument(abs_val) => abs_val.data_type.to_owned(),
            };
            input_args.push(input);
        }

        DataType::Function(Box::new(FunctionType {
            input_argument: match input_args.len() {
                1 => input_args[0].to_owned(),
                _ => DataType::Tuple(input_args),
            },
            output: value.output.to_owned(),
        }))
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum ListType {
    Safe,
    Unsafe,
}

impl ListType {
    pub fn metadata_size(&self) -> usize {
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum DataType {
    Bool,
    U32,
    U64,
    U128,
    BFE,
    XFE,
    Digest,
    List(Box<DataType>, ListType),
    Tuple(Vec<DataType>),
    VoidPointer,
    Function(Box<FunctionType>),
    Struct(StructType),
    MemPointer(Box<DataType>),
    Unresolved(String),
}

// impl PartialEq for DataType {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Self::List(et0, _), Self::List(et1, _)) => et0 == et1,
//             (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
//             (Self::Function(l0), Self::Function(r0)) => l0 == r0,
//             (Self::Struct(l0), Self::Struct(r0)) => l0 == r0,
//             (Self::MemPointer(l0), Self::MemPointer(r0)) => l0 == r0,
//             (Self::Unresolved(l0), Self::Unresolved(r0)) => l0 == r0,
//             _ => core::mem::discriminant(self) == core::mem::discriminant(other),
//         }
//     }
// }

// impl Eq for DataType {}

impl DataType {
    /// What type is returned when type is accessed with a field of name `field_name`?
    pub fn field_access_returned_type(&self, field_name: &str) -> Self {
        match &self {
            DataType::MemPointer(inner_type) => {
                if let DataType::Struct(inner_struct_type) = *inner_type.to_owned() {
                    // Case: `let foo: Foo = ...; foo.a`. `inner_struct_type = Foo`
                    let resulting_type = inner_struct_type.get_field_type(field_name);

                    // `resulting_type` is type of `a` field i `Foo`

                    // We dereference all resulting type unless we have nested structs, in that case, new pointers to structs
                    // are returned. I think that's more or less how Rust does it.
                    match &resulting_type {
                        DataType::Struct(_) => DataType::MemPointer(Box::new(resulting_type)),
                        _ => resulting_type,
                    }
                } else {
                    panic!("Field getter can only operate on type of struct or pointer to struct. Attempted to access field `{field_name}` on type `{self}`")
                }
            },
            DataType::Struct(struct_type) => struct_type.get_field_type(field_name),
            _ => panic!("Field getter can only operate on type of struct or pointer to struct. Attempted to access field `{field_name}` on type `{self}`")
        }
    }

    // TODO: Consider getting rid of this method
    /// Return the element type for lists
    pub fn type_parameter(&self) -> Option<DataType> {
        match self {
            DataType::List(element_type, _) => Some(*element_type.to_owned()),
            // TODO: Is this the right solution, or do we perhaps need to resolve
            // other types for our field access operators? I'm leaning towards the
            // latter ... but what if the are more field operators following each
            // other? Then the 2nd layer is a `struct` which should then probably
            // be `MemPointer(Struct)`. I think we need to resolve our problems
            // as best we can in the type resolver for expressions!
            // Can we do automatic, or selective dereferencing when meating a `.`
            // operator?
            // DataType::MemPointer(inner_type) => inner_type.type_parameter(),
            _ => None,
        }
    }

    // Notice that this implementation must match that derived by `BFieldCodec`
    pub fn bfield_codec_length(&self) -> Option<usize> {
        match self {
            DataType::Bool => Some(1),
            DataType::U32 => Some(1),
            DataType::U64 => Some(2),
            DataType::U128 => Some(4),
            DataType::BFE => Some(1),
            DataType::XFE => Some(3),
            DataType::Digest => Some(5),
            DataType::Tuple(inner_types) => inner_types
                .iter()
                .map(|x| x.bfield_codec_length())
                .fold(Some(0), |acc, x| acc.and_then(|a| x.map(|v| a + v))),
            DataType::List(_, _) => None,
            DataType::Struct(struct_type) => {
                struct_type
                    .fields
                    .iter()
                    .fold(Some(0), |acc, (_, field_type)| {
                        acc.and_then(|a| field_type.bfield_codec_length().map(|v| a + v))
                    })
            }
            DataType::MemPointer(inner_type) => inner_type.bfield_codec_length(),
            DataType::VoidPointer => todo!(),
            DataType::Function(_) => todo!(),
            DataType::Unresolved(_) => todo!(),
        }
    }

    pub fn unit() -> Self {
        Self::Tuple(vec![])
    }

    pub fn stack_size(&self) -> usize {
        match self {
            Self::Bool => 1,
            Self::U32 => 1,
            Self::U64 => 2,
            Self::U128 => 4,
            Self::BFE => 1,
            Self::XFE => 3,
            Self::Digest => 5,
            Self::List(_list_type, _) => 1,
            Self::Tuple(tuple_type) => tuple_type.iter().map(Self::stack_size).sum(),
            Self::VoidPointer => 1,
            Self::Function(_) => todo!(),
            Self::Struct(_) => 1, // a pointer to a struct in memory
            Self::Unresolved(name) => panic!("cannot get size of unresolved type {name}"),
            Self::MemPointer(_) => 1,
        }
    }

    /// Returns true iff any of the contained types have to be resolved through types associated with the program
    pub fn is_unresolved(&self) -> bool {
        match self {
            DataType::Unresolved(_) => true,
            DataType::MemPointer(inner) => inner.is_unresolved(),
            DataType::Tuple(inners) => inners.iter().any(|inner| inner.is_unresolved()),
            DataType::List(element, _) => element.is_unresolved(),
            DataType::Struct(StructType { name: _, fields }) => fields
                .iter()
                .any(|(_field_name, field_type)| field_type.is_unresolved()),
            DataType::Function(function_type) => {
                function_type.input_argument.is_unresolved() || function_type.output.is_unresolved()
            }
            _ => false,
        }
    }

    pub fn resolve_types(&self, declared_structs: &HashMap<String, StructType>) -> Self {
        match self {
            DataType::Unresolved(unresolved_type) => {
                let outer_resolved = declared_structs.get(unresolved_type).unwrap_or_else(|| {
                    panic!("Failed to resolve type {unresolved_type}. Does not know this type.")
                });
                let resolved_fields = outer_resolved
                    .fields
                    .iter()
                    .map(|(field_name, field_type)| {
                        (
                            field_name.to_owned(),
                            field_type.resolve_types(declared_structs),
                        )
                    })
                    .collect_vec();
                DataType::Struct(StructType {
                    name: outer_resolved.name.clone(),
                    fields: resolved_fields,
                })
            }
            DataType::List(inner, list_type) => {
                DataType::List(Box::new(inner.resolve_types(declared_structs)), *list_type)
            }
            DataType::Tuple(inners) => DataType::Tuple(
                inners
                    .iter()
                    .map(|inner_type| inner_type.resolve_types(declared_structs))
                    .collect_vec(),
            ),
            DataType::Function(function_type) => DataType::Function(Box::new(FunctionType {
                input_argument: function_type.input_argument.resolve_types(declared_structs),
                output: function_type.output.resolve_types(declared_structs),
            })),
            DataType::MemPointer(inner) => {
                DataType::MemPointer(Box::new(inner.resolve_types(declared_structs)))
            }
            DataType::Struct(struct_type) => {
                let resolved_fields = struct_type
                    .fields
                    .iter()
                    .map(|(field_name, field_type)| {
                        (
                            field_name.to_owned(),
                            field_type.resolve_types(declared_structs),
                        )
                    })
                    .collect_vec();
                DataType::Struct(StructType {
                    name: struct_type.name.clone(),
                    fields: resolved_fields,
                })
            }
            _ => self.clone(),
        }
    }
}

impl TryFrom<DataType> for tasm_lib::snippet::DataType {
    type Error = String;

    fn try_from(value: DataType) -> Result<Self, Self::Error> {
        match value {
            DataType::Bool => Ok(tasm_lib::snippet::DataType::Bool),
            DataType::U32 => Ok(tasm_lib::snippet::DataType::U32),
            DataType::U64 => Ok(tasm_lib::snippet::DataType::U64),
            DataType::U128 => Ok(tasm_lib::snippet::DataType::U128),
            DataType::BFE => Ok(tasm_lib::snippet::DataType::BFE),
            DataType::XFE => Ok(tasm_lib::snippet::DataType::XFE),
            DataType::Digest => Ok(tasm_lib::snippet::DataType::Digest),
            DataType::List(elem_type, _) => {
                let element_type = (*elem_type).try_into();
                let element_type = match element_type {
                    Ok(e) => e,
                    Err(err) => return Err(format!("Failed to convert element type of list: {err}")),
                };
                Ok(tasm_lib::snippet::DataType::List(Box::new(element_type)))
            },
            DataType::Tuple(_) => Err("Tuple cannot be converted to a tasm_lib type. Try converting its individual elements".to_string()),
            DataType::VoidPointer => Ok(tasm_lib::snippet::DataType::VoidPointer),
            DataType::Function(_) => todo!(),
            DataType::Struct(_) => todo!(),
            DataType::Unresolved(name) => panic!("cannot convert unresolved type {name}"),
            DataType::MemPointer(value) => match *value {
                // A MemPointer to a list is just a list
                // TODO: Default to `Unsafe` list here??
                DataType::List(_, ListType::Unsafe) => (*value).try_into(),
                DataType::Unresolved(_) => todo!(),
                _ => Ok(tasm_lib::snippet::DataType::VoidPointer),
            }
        }
    }
}

impl From<tasm_lib::snippet::DataType> for DataType {
    fn from(value: tasm_lib::snippet::DataType) -> Self {
        match value {
            tasm_lib::snippet::DataType::Bool => DataType::Bool,
            tasm_lib::snippet::DataType::U32 => DataType::U32,
            tasm_lib::snippet::DataType::U64 => DataType::U64,
            tasm_lib::snippet::DataType::U128 => DataType::U128,
            tasm_lib::snippet::DataType::BFE => DataType::BFE,
            tasm_lib::snippet::DataType::XFE => DataType::XFE,
            tasm_lib::snippet::DataType::Digest => DataType::Digest,
            tasm_lib::snippet::DataType::VoidPointer => DataType::VoidPointer,
            tasm_lib::snippet::DataType::List(elem_type_snip) => {
                let element_type: DataType = (*elem_type_snip).into();
                DataType::List(Box::new(element_type), ListType::Safe)
            }
            tasm_lib::snippet::DataType::Tuple(tasm_types) => {
                let element_types: Vec<DataType> =
                    tasm_types.into_iter().map(|t| t.into()).collect();
                DataType::Tuple(element_types)
            }
        }
    }
}

impl FromStr for DataType {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bool" => Ok(DataType::Bool),
            "u32" => Ok(DataType::U32),

            // `usize` is just an alias for `u32` in this compiler
            "usize" => Ok(DataType::U32),
            "u64" => Ok(DataType::U64),
            "u128" => Ok(DataType::U128),
            "BFieldElement" => Ok(DataType::BFE),
            "XFieldElement" => Ok(DataType::XFE),
            "Digest" => Ok(DataType::Digest),
            ty => bail!("Unsupported type {}", ty),
        }
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use DataType::*;
        let str = match self {
            Bool => "bool".to_string(),
            U32 => "u32".to_string(),
            U64 => "u64".to_string(),
            U128 => "u128".to_string(),
            BFE => "BField".to_string(),
            XFE => "XField".to_string(),
            Digest => "Digest".to_string(),
            List(ty, _list_type) => format!("List({ty})"),
            Tuple(tys) => tys.iter().map(|ty| format!("{ty}")).join(" "),
            VoidPointer => "void pointer".to_string(),
            Function(fn_type) => {
                let input = fn_type.input_argument.to_string();
                let output = fn_type.output.to_string();
                format!("Function: {input} -> {output}")
            }
            Struct(struct_type) => format!("struct_type: {struct_type}"),
            Unresolved(name) => format!("unresolved type {name}"),
            MemPointer(ty) => format!("mempoint({ty})"),
        };
        write!(f, "{str}",)
    }
}
