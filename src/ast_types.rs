use anyhow::bail;
use itertools::Itertools;
use std::{collections::HashMap, fmt::Display, str::FromStr};
use triton_vm::triton_asm;

use crate::{ast::FnSignature, libraries::LibraryFunction};

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
    Tuple(Tuple),
    Struct(StructType),
    VoidPointer,
    Function(Box<FunctionType>),
    MemPointer(Box<DataType>),
    Unresolved(String),
}

impl DataType {
    pub fn get_tuple_elements(&self) -> Tuple {
        match self {
            DataType::Tuple(tuple) => tuple.to_owned(),
            DataType::Struct(StructType {
                name: _,
                is_copy: _,
                variant,
            }) => match variant {
                StructVariant::TupleStruct(ts) => ts.to_owned(),
                StructVariant::NamedFields(_) => todo!(),
            },
            // TODO: Get rid of this mess by using `FieldId` in `Identifier` instead
            DataType::MemPointer(inner) => {
                let inner = *inner.to_owned();
                match inner {
                    DataType::Struct(StructType {
                        name: _,
                        is_copy: _,
                        variant,
                    }) => match variant {
                        StructVariant::TupleStruct(ts) => ts.to_owned(),
                        StructVariant::NamedFields(_) => todo!(),
                    },
                    _ => panic!("Type is not unnamed or named tuple. Type was: {self}"),
                }
            }
            _ => panic!("Type is not unnamed or named tuple. Type was: {self}"),
        }
    }

    /// Return true if it's OK to copy this type including its underlying
    /// data, false if it's expensive to copy.
    pub fn is_copy(&self) -> bool {
        match self {
            DataType::Bool => true,
            DataType::U32 => true,
            DataType::U64 => true,
            DataType::U128 => true,
            DataType::BFE => true,
            DataType::XFE => true,
            DataType::Digest => true,
            DataType::Tuple(_) => true,
            DataType::List(_, _) => false,
            DataType::VoidPointer => false,
            DataType::Function(_) => false,
            DataType::Struct(struct_type) => struct_type.is_copy,
            DataType::Unresolved(_) => false,
            DataType::MemPointer(_) => false,
        }
    }

    /// Use this if the type is used to make labels in the TASM code
    pub fn label_friendly_name(&self) -> String {
        use DataType::*;
        match self {
            Bool => "bool".to_string(),
            U32 => "u32".to_string(),
            U64 => "u64".to_string(),
            U128 => "u128".to_string(),
            BFE => "BField".to_string(),
            XFE => "XField".to_string(),
            Digest => "Digest".to_string(),
            List(ty, _list_type) => format!("Vec_R{}_L", ty.label_friendly_name()),
            Tuple(tys) => format!(
                "({})",
                tys.into_iter().map(|x| x.label_friendly_name()).join("_")
            ),
            VoidPointer => "void_pointer".to_string(),
            Function(fn_type) => {
                let input = fn_type.input_argument.label_friendly_name();
                let output = fn_type.output.label_friendly_name();
                format!("function_from_L{}R__to_L{}R", input, output)
            }
            Struct(struct_type) => struct_type.name.to_owned(),
            Unresolved(name) => name.to_string(),
            MemPointer(ty) => format!("mempointer_L{}R", ty.label_friendly_name()),
        }
    }

    pub fn from_tasm_lib_datatype(
        tasm_lib_type: tasm_lib::snippet::DataType,
        list_type: ListType,
    ) -> Self {
        use DataType::*;
        match tasm_lib_type {
            tasm_lib::snippet::DataType::Bool => Bool,
            tasm_lib::snippet::DataType::U32 => U32,
            tasm_lib::snippet::DataType::U64 => U64,
            tasm_lib::snippet::DataType::U128 => U128,
            tasm_lib::snippet::DataType::BFE => BFE,
            tasm_lib::snippet::DataType::XFE => XFE,
            tasm_lib::snippet::DataType::Digest => Digest,
            tasm_lib::snippet::DataType::List(elem_type) => {
                let element_type = Self::from_tasm_lib_datatype(*elem_type, list_type);
                List(Box::new(element_type), list_type)
            }
            tasm_lib::snippet::DataType::Tuple(tasm_types) => {
                let element_types: Vec<DataType> = tasm_types
                    .into_iter()
                    .map(|t| Self::from_tasm_lib_datatype(t, list_type))
                    .collect();
                Tuple(element_types.into())
            }
            tasm_lib::snippet::DataType::VoidPointer => VoidPointer,
        }
    }

    /// What type is returned when type is accessed with a field of name `field_name`?
    pub fn field_access_returned_type(&self, field_name: &str) -> Self {
        match &self {
            // If something is a `MemPointer`, it stays a `MemPointer` after field access,
            // unless the field is a copy type.
            DataType::MemPointer(inner_type) => {
                let struct_type = match &**inner_type {
                    DataType::Struct(struct_type) => struct_type,
                    _ => panic!("Field access can only be performed on structs"),
                };

                let field_type = struct_type.get_field_type(field_name.into());
                if field_type.is_copy() {
                    field_type
                } else {
                    DataType::MemPointer(Box::new(field_type))
                }
            }
            DataType::Struct(struct_type) => {
                struct_type.get_field_type(field_name.into())
            },
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
                .into_iter()
                .map(|x| x.bfield_codec_length())
                .try_fold(0, |acc: usize, x| x.map(|x| x + acc)),
            DataType::List(_, _) => None,
            DataType::Struct(struct_type) => struct_type
                .field_types()
                .try_fold(0, |acc: usize, field_type| {
                    field_type.bfield_codec_length().map(|v| acc + v)
                }),
            DataType::MemPointer(inner_type) => inner_type.bfield_codec_length(),
            DataType::VoidPointer => todo!(),
            DataType::Function(_) => todo!(),
            DataType::Unresolved(_) => todo!(),
        }
    }

    pub fn unit() -> Self {
        Self::Tuple(vec![].into())
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
            Self::Tuple(tuple_type) => tuple_type.into_iter().map(|x| Self::stack_size(&x)).sum(),
            Self::VoidPointer => 1,
            Self::Function(_) => todo!(),
            Self::Struct(_) => 1, // a pointer to a struct in memory
            Self::Unresolved(name) => panic!("cannot get size of unresolved type {name}"),
            Self::MemPointer(_) => 1,
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
            List(ty, _list_type) => format!("Vec<{ty}>"),
            Tuple(tys) => format!("({})", tys.into_iter().join(", ")),
            VoidPointer => "void pointer".to_string(),
            Function(fn_type) => {
                let input = fn_type.input_argument.to_string();
                let output = fn_type.output.to_string();
                format!("Function: {input} -> {output}")
            }
            Struct(struct_type) => struct_type.name.to_owned(),
            Unresolved(name) => name.to_string(),
            MemPointer(ty) => format!("*{ty}"),
        };
        write!(f, "{str}",)
    }
}

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
    pub is_copy: bool,
    pub variant: StructVariant,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum StructVariant {
    TupleStruct(Tuple),
    NamedFields(NamedFieldsStruct),
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
    /// Only named tuples, i.e. tuple structs should use this constructor.
    pub fn constructor(&self) -> LibraryFunction {
        let tuple = if let StructVariant::TupleStruct(tuple) = &self.variant {
            tuple
        } else {
            panic!("Only tuple structs have constructor functions. Attempted to get constructor for struct {}", self.name);
        };
        let args = tuple
            .fields
            .iter()
            .enumerate()
            .map(|(i, x)| {
                AbstractArgument::ValueArgument(AbstractValueArg {
                    name: format!("tuple_elem_{i}"),
                    data_type: x.to_owned(),
                    mutable: false,
                })
            })
            .collect_vec();
        let signature = FnSignature {
            name: self.name.to_owned(),
            args,
            output: DataType::Struct(self.to_owned()),
            arg_evaluation_order: Default::default(),
        };

        // Function body of the tuple-struct constructor is empty, since
        // the construction simply corresponds to the evaluation of arguments
        // from left-to-rigth, as this will leave the last element of the
        // tuple on top of the stack.
        let body = triton_asm!();
        LibraryFunction { signature, body }
    }

    pub fn get_field_type(&self, field_id: FieldId) -> DataType {
        let res = match &self.variant {
            StructVariant::TupleStruct(ts) => match &field_id {
                FieldId::NamedField(_) => todo!(),
                FieldId::UnnamedField(tuple_index) => {
                    ts.fields.get(*tuple_index).map(|x| x.to_owned())
                }
            },
            StructVariant::NamedFields(nfs) => match &field_id {
                FieldId::NamedField(field_name) => nfs
                    .fields
                    .iter()
                    .find(|&field| field.0 == *field_name)
                    .map(|x| x.1.to_owned()),
                FieldId::UnnamedField(_) => todo!(),
            },
        };

        match res {
            Some(dtype) => dtype,
            None => panic!("Struct {self} has no field '{field_id}'"),
        }
    }

    pub fn field_types<'a>(&'a self) -> Box<dyn Iterator<Item = &'a DataType> + 'a> {
        match &self.variant {
            StructVariant::TupleStruct(ts) => Box::new(ts.fields.iter()),
            StructVariant::NamedFields(nfs) => {
                Box::new(nfs.fields.iter().map(|(_name, dtype)| dtype))
            }
        }
    }

    pub fn field_types_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut DataType> + 'a> {
        match &mut self.variant {
            StructVariant::TupleStruct(ts) => Box::new(ts.fields.iter_mut()),
            StructVariant::NamedFields(nfs) => {
                Box::new(nfs.fields.iter_mut().map(|(_name, dtype)| dtype))
            }
        }
    }

    pub fn field_ids_and_types<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = (FieldId, &'a DataType)> + 'a> {
        match &self.variant {
            StructVariant::TupleStruct(ts) => Box::new(
                ts.fields
                    .iter()
                    .enumerate()
                    .map(|(tuple_idx, element_type)| (tuple_idx.into(), element_type)),
            ),
            StructVariant::NamedFields(nfs) => Box::new(
                nfs.fields
                    .iter()
                    .map(|(field_name, element_type)| (field_name.into(), element_type)),
            ),
        }
    }
}

impl Display for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Tuple {
    pub fields: Vec<DataType>,
}

impl From<Vec<DataType>> for Tuple {
    fn from(fields: Vec<DataType>) -> Self {
        Self { fields }
    }
}

impl IntoIterator for Tuple {
    type Item = DataType;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.into_iter()
    }
}

impl<'a> IntoIterator for &'a Tuple {
    type Item = &'a DataType;
    type IntoIter = std::slice::Iter<'a, DataType>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.iter()
    }
}

impl<'a> IntoIterator for &'a mut Tuple {
    type Item = &'a mut DataType;
    type IntoIter = std::slice::IterMut<'a, DataType>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.iter_mut()
    }
}

impl std::ops::Index<usize> for Tuple {
    type Output = DataType;

    fn index(&self, index: usize) -> &Self::Output {
        &self.fields[index]
    }
}

impl Tuple {
    pub fn len(&self) -> usize {
        self.fields.len()
    }

    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum FieldId {
    NamedField(String),
    UnnamedField(usize),
}

impl From<&str> for FieldId {
    fn from(value: &str) -> Self {
        Self::NamedField(value.to_owned())
    }
}

impl From<&String> for FieldId {
    fn from(value: &String) -> Self {
        Self::NamedField(value.to_owned())
    }
}

impl From<&usize> for FieldId {
    fn from(value: &usize) -> Self {
        Self::UnnamedField(*value)
    }
}

impl From<usize> for FieldId {
    fn from(value: usize) -> Self {
        Self::UnnamedField(value)
    }
}

impl Display for FieldId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            FieldId::NamedField(name) => name.to_owned(),
            FieldId::UnnamedField(tuple_index) => tuple_index.to_string(),
        };
        write!(f, "{output}")
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct NamedFieldsStruct {
    pub name: String,
    pub fields: Vec<(String, DataType)>,
}

impl Display for NamedFieldsStruct {
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
                _ => DataType::Tuple(input_args.into()),
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

impl From<ListType> for tasm_lib::list::ListType {
    fn from(value: ListType) -> Self {
        match value {
            ListType::Safe => tasm_lib::list::ListType::Safe,
            ListType::Unsafe => tasm_lib::list::ListType::Unsafe,
        }
    }
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
