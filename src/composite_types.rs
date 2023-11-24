use itertools::Itertools;
use num::One;
use triton_vm::instruction::LabelledInstruction;

use crate::{
    ast,
    ast_types::{self, CustomTypeOil, EnumType},
    type_checker::{self, GetType, Typing},
};
use std::collections::HashMap;

#[derive(Debug, Default, Clone)]
pub struct CompositeTypes {
    // `types` contains both custom types and types from libraries,
    // like `Result`` and `Option`.
    types: HashMap<String, Vec<ast_types::CustomTypeOil>>,
}

impl CompositeTypes {
    // Returns an iterator over the `Vec<CustomTypeOil>` values in the hashmap.
}

impl IntoIterator for CompositeTypes {
    type Item = (String, Vec<ast_types::CustomTypeOil>);
    type IntoIter = <HashMap<String, Vec<CustomTypeOil>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.types.into_iter()
    }
}

impl CompositeTypes {
    /// Return the instructions for the constructor of the matching type, otherwise
    /// returns None.
    pub(crate) fn constructor_code(
        &self,
        fn_call: &ast::FnCall<Typing>,
    ) -> Option<Vec<LabelledInstruction>> {
        let match_type = if let Some(m) = self.constructor_match(fn_call) {
            m
        } else {
            return None;
        };

        match match_type {
            (CustomTypeOil::Enum(e), Some(variant_name)) => {
                Some(e.variant_tuple_constructor(&variant_name).body)
            }
            (CustomTypeOil::Struct(s), None) => Some(s.constructor().body),
            (CustomTypeOil::Enum(_), None) => unreachable!(),
            (CustomTypeOil::Struct(_), Some(_)) => unreachable!(),
        }
    }

    pub(crate) fn get_all_constructor_signatures(&self) -> HashMap<String, Vec<ast::FnSignature>> {
        let mut ftable: HashMap<String, Vec<ast::FnSignature>> = HashMap::default();
        for (type_name, custom_types) in self.types.iter() {
            for custom_type in custom_types {
                match custom_type {
                    ast_types::CustomTypeOil::Struct(struct_type) => {
                        if let ast_types::StructVariant::TupleStruct(_) = &struct_type.variant {
                            let constructor_signature = struct_type.constructor().signature;
                            ftable
                                .entry(type_name.to_owned())
                                .and_modify(|signatures| {
                                    signatures.push(constructor_signature.clone())
                                })
                                .or_insert(vec![constructor_signature]);
                        }
                    }
                    ast_types::CustomTypeOil::Enum(enum_type) => {
                        for (variant_name, variant_type) in enum_type.variants.iter() {
                            if !variant_type.is_unit() {
                                let constructor_name = if enum_type.is_prelude {
                                    format!("{variant_name}")
                                } else {
                                    format!("{}::{variant_name}", enum_type.name)
                                };
                                let constructor_signature =
                                    enum_type.variant_tuple_constructor(variant_name).signature;
                                ftable
                                    .entry(constructor_name)
                                    .and_modify(|signatures| {
                                        signatures.push(constructor_signature.clone())
                                    })
                                    .or_insert(vec![constructor_signature]);
                            }
                        }
                    }
                }
            }
        }

        ftable
    }

    /// Merge two composite type collections. Panics if intersection is non-empty.
    pub(crate) fn checked_merge(&mut self, other: Self) {
        for (type_name, dtypes) in other.into_iter() {
            for dtype in dtypes {
                self.add(type_name.clone(), dtype);
            }
        }
    }

    pub(crate) fn values_mut(
        &mut self,
    ) -> std::collections::hash_map::ValuesMut<String, Vec<ast_types::CustomTypeOil>> {
        self.types.values_mut()
    }

    /// Add a composite type to the collection. Panics if it's already included.
    pub(crate) fn add(&mut self, type_name: String, dtype: ast_types::CustomTypeOil) {
        self.types
            .entry(type_name.clone())
            .and_modify(|types| {
                assert!(
                    types.iter().all(|x| *x != dtype),
                    "Attempted to insert repeated composite type with name {type_name}",
                );
                types.push(dtype.clone())
            })
            .or_insert(vec![dtype]);
    }

    /// Return a type that must be uniquely identified by its name. Otherwise this function
    /// panics.
    pub(crate) fn get_exactly_one(&self, type_name: &str) -> ast_types::CustomTypeOil {
        match self.types.get(type_name) {
            None => panic!("Did not find composite type with name {type_name}"),
            Some(entry) => {
                assert!(
                    entry.len().is_one(),
                    "Type with name {type_name} may only be defined once."
                );
                entry[0].clone()
            }
        }
    }

    /// Return the composite type for which the input function call is a constructor
    /// `Foo::A(100)` will match
    /// enum Foo {
    ///     A(u32),
    /// }
    fn constructor_match(
        &self,
        ast::FnCall {
            name,
            args,
            type_parameter: _,
            arg_evaluation_order: _,
            annot: _,
        }: &ast::FnCall<type_checker::Typing>,
    ) -> Option<(ast_types::CustomTypeOil, Option<String>)> {
        let split_name = name.split("::").collect_vec();

        // Is this an enum constructor for a type in `prelude`? E.g. `Ok(...)`.
        for prelude in self.get_preludes() {
            if prelude.has_variant_of_name(split_name[0]) {
                let variant_name = split_name[0];
                if prelude
                    .variant_data_type(variant_name)
                    .as_tuple_type()
                    .into_iter()
                    .zip(args.iter())
                    .all(|(constructor_abstr_arg, actual_arg)| {
                        constructor_abstr_arg == actual_arg.get_type()
                    })
                {
                    return Some((prelude.into(), Some(split_name[0].to_owned())));
                }
            }
        }

        // Is this a constructor for a non-prelude enum?
        let type_name = split_name[0];
        if split_name.len() > 1
            && split_name[1].chars().next().unwrap().is_uppercase()
            && self.types.contains_key(type_name)
        {
            let candidates = &self.types[type_name];
            for candidate in candidates.iter() {
                match candidate {
                    ast_types::CustomTypeOil::Struct(_) => {
                        return None;
                    }
                    ast_types::CustomTypeOil::Enum(e) => {
                        if e.has_variant_of_name(split_name[1]) {
                            let variant_name = split_name[1];
                            let variant_data_type = e.variant_data_type(variant_name);
                            if variant_data_type
                                .as_tuple_type()
                                .into_iter()
                                .zip(args.iter())
                                .all(|(constructor_abstr_arg, actual_arg)| {
                                    constructor_abstr_arg == actual_arg.get_type()
                                })
                            {
                                return Some((candidate.to_owned(), Some(variant_name.to_owned())));
                            }
                        }
                    }
                }
            }
        }

        // Is this a constructor for a tuple-type?
        if split_name.len().is_one() {
            if let Some(candidates) = self.types.get(split_name[0]) {
                for candidate in candidates {
                    match candidate {
                        ast_types::CustomTypeOil::Struct(s) => {
                            if s.field_ids_and_types()
                                .zip(args.iter())
                                .all(|((_, aa), ca)| *aa == ca.get_type())
                            {
                                return Some((candidate.to_owned(), None));
                            }
                        }
                        ast_types::CustomTypeOil::Enum(_) => (),
                    }
                }
            }
        }

        None
    }

    fn get_preludes(&self) -> Vec<EnumType> {
        let mut ret = vec![];
        for dtypes in self.types.values() {
            for dtype in dtypes {
                if dtype.is_prelude() {
                    ret.push(dtype.try_into().unwrap());
                }
            }
        }

        ret
    }
}
