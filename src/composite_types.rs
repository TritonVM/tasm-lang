use itertools::Itertools;
use num::One;
use triton_vm::instruction::LabelledInstruction;

use crate::{
    ast,
    ast_types::{self, CustomTypeOil, EnumType},
    custom_type_resolver::CustomTypeResolution,
    type_checker::{self, GetType, Typing},
};
use std::collections::HashMap;

/// A type definition, its methods, and its association functions
#[derive(Debug, Clone)]
pub(crate) struct TypeContext {
    pub(crate) composite_type: ast_types::CustomTypeOil,
    pub(crate) methods: Vec<ast::Method<Typing>>,
    pub(crate) associated_functions: Vec<ast::Fn<Typing>>,
}

impl From<CustomTypeOil> for TypeContext {
    fn from(value: CustomTypeOil) -> Self {
        Self {
            composite_type: value,
            methods: Default::default(),
            associated_functions: Default::default(),
        }
    }
}

impl TypeContext {
    /// Add a method to this type. Panics if method name already there.
    pub(crate) fn add_method(&mut self, new_method: ast::Method<Typing>) {
        assert!(
            !self
                .methods
                .iter()
                .any(|m| m.signature.name == new_method.signature.name),
            "Duplicate method with name {} for type {}",
            new_method.signature.name,
            self.composite_type.name()
        );
        self.methods.push(new_method);
    }

    /// Get a method identified by name
    pub(crate) fn get_method(&self, method_name: &str) -> Option<&ast::Method<Typing>> {
        self.methods
            .iter()
            .find(|x| x.signature.name == method_name)
    }

    /// Add an associated function to this type. Panics if function name is already there.
    pub(crate) fn add_associated_function(&mut self, new_fun: ast::Fn<Typing>) {
        assert!(
            !self
                .methods
                .iter()
                .any(|m| m.signature.name == new_fun.signature.name),
            "Duplicate associated function with name {} for type {}",
            new_fun.signature.name,
            self.composite_type.name()
        );
        self.associated_functions.push(new_fun)
    }

    /// Get an associated function identifier by name
    pub(crate) fn get_associated_function(&self, fname: &str) -> Option<&ast::Fn<Typing>> {
        self.associated_functions
            .iter()
            .find(|x| x.signature.name == fname)
    }
}

impl PartialEq for TypeContext {
    fn eq(&self, other: &Self) -> bool {
        self.composite_type == other.composite_type
    }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct CompositeTypes {
    by_name: HashMap<String, Vec<usize>>,
    by_type: HashMap<ast_types::DataType, usize>,
    composite_types: Vec<TypeContext>,
}

impl IntoIterator for CompositeTypes {
    type Item = TypeContext;
    type IntoIter = std::vec::IntoIter<TypeContext>;

    fn into_iter(self) -> Self::IntoIter {
        self.composite_types.into_iter()
    }
}

impl CompositeTypes {
    /// Merge two composite type collections. Panics if intersection is non-empty.
    pub(crate) fn checked_merge(&mut self, other: Self) {
        for (type_name, dtype_indices) in other.by_name.into_iter() {
            for dtype_index in dtype_indices {
                self.add(
                    type_name.clone(),
                    other.composite_types[dtype_index].clone(),
                )
            }
        }
    }

    /// Resolve all types. Handles nested type definitions and type resolution on methods and associated functions
    pub(crate) fn resolve_nested(&mut self) {
        let mut custom_types_copy = self.clone();
        while custom_types_copy
            .clone()
            .into_iter()
            .any(|tctx| tctx.composite_type.is_unresolved())
        {
            self.composite_types.iter_mut().for_each(|x| {
                x.composite_type
                    .field_or_variant_types_mut()
                    .for_each(|y| y.resolve_custom_types(&custom_types_copy))
            });
            custom_types_copy = self.to_owned();
        }

        self.composite_types.iter_mut().for_each(|tyctx| {
            tyctx
                .methods
                .iter_mut()
                .for_each(|method| method.resolve_custom_types(&custom_types_copy));

            tyctx
                .associated_functions
                .iter_mut()
                .for_each(|method| method.resolve_custom_types(&custom_types_copy))
        });

        // Ensure that all types in the (type => index) mapping are also resolved,
        // otherwise nested types would still be unresolved here.
        self.by_type = Default::default();
        for (mut k, v) in custom_types_copy.clone().by_type.drain() {
            k.resolve_custom_types(&custom_types_copy);
            self.by_type.insert(k, v);
        }
    }

    /// Add a composite type to the collection. Panics if it's already included.
    pub(crate) fn add_type(&mut self, type_name: String, dtype: CustomTypeOil) {
        let tyctx: TypeContext = dtype.into();
        self.add(type_name, tyctx);
    }

    /// Add a composite type to the collection. Panics if it's already included.
    fn add(&mut self, type_name: String, tyctx: TypeContext) {
        let type_count = self.composite_types.len();
        self.by_name
            .entry(type_name.clone())
            .and_modify(|types_w_same_name| {
                assert!(
                    types_w_same_name
                        .iter()
                        .all(|x| self.composite_types[*x] != tyctx),
                    "Attempted to insert repeated composite type with name {type_name}",
                );
                types_w_same_name.push(type_count);
            })
            .or_insert(vec![type_count]);

        self.by_type
            .insert(tyctx.composite_type.clone().into(), type_count);
        self.composite_types.push(tyctx);
    }

    /// Return a type context that must be uniquely identified by its name.
    /// Otherwise this function panics.
    pub(crate) fn get_unique_by_name(&self, type_name: &str) -> TypeContext {
        match self.by_name.get(type_name) {
            None => panic!("Did not find composite type with name {type_name}"),
            Some(indices) => {
                assert!(
                    indices.len().is_one(),
                    "Type with name {type_name} was defined more than once."
                );
                self.composite_types[indices[0]].clone()
            }
        }
    }

    /// Return a mutable pointer to a type context that must be uniquely identified
    /// by its name. Otherwise this function panics.
    pub(crate) fn get_mut_unique_by_name(&mut self, type_name: &str) -> &mut TypeContext {
        match self.by_name.get_mut(type_name) {
            None => panic!("Did not find composite type with name {type_name}"),
            Some(indices) => {
                assert!(
                    indices.len().is_one(),
                    "Type with name {type_name} was defined more than once."
                );
                &mut self.composite_types[indices[0]]
            }
        }
    }

    /// Return a type context that is uniquely identified by its data type.
    pub(crate) fn get_by_type(&self, composite_type: &ast_types::DataType) -> Option<&TypeContext> {
        let index = self.by_type.get(composite_type);
        index.map(|index| &self.composite_types[*index])
    }

    pub(crate) fn get_method(
        &self,
        method_call: &ast::MethodCall<type_checker::Typing>,
    ) -> Option<ast::Method<Typing>> {
        self.get_by_type(&method_call.associated_type.as_ref().unwrap())
            .map(|tyctx| tyctx.get_method(&method_call.method_name).unwrap())
            .cloned()
    }

    /********** Type Checking **********/
    pub(crate) fn methods_mut(&mut self) -> std::vec::IntoIter<&mut ast::Method<Typing>> {
        self.composite_types
            .iter_mut()
            .flat_map(|x| x.methods.iter_mut())
            .collect_vec()
            .into_iter()
    }

    pub(crate) fn associated_functions_mut(&mut self) -> std::vec::IntoIter<&mut ast::Fn<Typing>> {
        self.composite_types
            .iter_mut()
            .flat_map(|x| x.associated_functions.iter_mut())
            .collect_vec()
            .into_iter()
    }

    pub(crate) fn get_associated_function_signature(&self, name: &str) -> Option<ast::FnSignature> {
        self.get_associated_function(name).map(|x| x.signature)
    }

    pub(crate) fn get_all_constructor_signatures(&self) -> HashMap<String, Vec<ast::FnSignature>> {
        let mut ftable: HashMap<String, Vec<ast::FnSignature>> = HashMap::default();
        for (type_name, custom_types) in self.by_name.iter() {
            for type_context in custom_types {
                match &self.composite_types[*type_context].composite_type {
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
                                    variant_name.to_owned()
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

    /********** Code Generation **********/
    /// Return the instructions for the constructor of the matching type, otherwise
    /// returns None.
    pub(crate) fn constructor_code(
        &self,
        fn_call: &ast::FnCall<Typing>,
    ) -> Option<Vec<LabelledInstruction>> {
        let match_type = self.constructor_match(fn_call)?;

        match match_type {
            (CustomTypeOil::Enum(e), Some(variant_name)) => {
                Some(e.variant_tuple_constructor(&variant_name).body)
            }
            (CustomTypeOil::Struct(s), None) => Some(s.constructor().body),
            (CustomTypeOil::Enum(_), None) => unreachable!(),
            (CustomTypeOil::Struct(_), Some(_)) => unreachable!(),
        }
    }

    /* Debugging */
    pub(crate) fn all_method_names(&self) -> String {
        self.composite_types
            .iter()
            .map(|tyctx| {
                let method_names_of_type = tyctx.methods.iter().join(", ");
                format!("{}:\n{method_names_of_type}", tyctx.composite_type.name())
            })
            .join("\n\n")
    }

    // TODO: Delete this method
    pub(crate) fn by_types(&self) -> String {
        self.by_type
            .iter()
            .map(|(k, v)| format!("{k:?} => {v}"))
            .join("\n\n")
    }

    pub(crate) fn all_composite_type_names(&self) -> String {
        self.composite_types
            .iter()
            .map(|tyctx| {
                let as_dt: ast_types::DataType = tyctx.composite_type.clone().into();
                format!("{as_dt}")
            })
            .join("\n\n")
    }

    /********** Shared Methods **********/
    pub(crate) fn get_associated_function(&self, name: &str) -> Option<ast::Fn<Typing>> {
        // Associated functions be called with `<Type>::<function_name>`, where `function_name`
        // must be lower-cased.
        let split_name = name.split("::").collect_vec();
        if !(split_name.len() > 1 && split_name[1].chars().next().unwrap().is_lowercase()) {
            return None;
        }

        let type_name = split_name[0];
        let fname = split_name[1];

        let ty_ctx = if let Some(indices) = self.by_name.get(type_name) {
            assert!(
                indices.len().is_one(),
                "Multiple composite types with name {} found",
                type_name
            );
            &self.composite_types[indices[0]]
        } else {
            return None;
        };

        ty_ctx.get_associated_function(&fname).map(|x| x.to_owned())
    }

    /// Return all enums that are included in `prelude`, meaning that
    /// the programmer only has to specify the variant name, not the type.
    /// E.g.: `Ok(5)` instead of `Return::Ok(5)`.
    fn get_preludes(&self) -> Vec<EnumType> {
        let mut ret = vec![];
        for dtype in self.composite_types.iter() {
            if dtype.composite_type.is_prelude() {
                ret.push((&dtype.composite_type).try_into().unwrap());
            }
        }

        ret
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
            && self.by_name.contains_key(type_name)
        {
            let candidates = &self.by_name[type_name];
            for candidate in candidates.iter() {
                match &self.composite_types[*candidate].composite_type {
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
                                return Some((
                                    self.composite_types[*candidate].composite_type.to_owned(),
                                    Some(variant_name.to_owned()),
                                ));
                            }
                        }
                    }
                }
            }
        }

        // Is this a constructor for a tuple-type?
        if split_name.len().is_one() {
            if let Some(candidates) = self.by_name.get(split_name[0]) {
                for candidate in candidates {
                    match &self.composite_types[*candidate].composite_type {
                        ast_types::CustomTypeOil::Struct(s) => {
                            if s.field_ids_and_types()
                                .zip(args.iter())
                                .all(|((_, aa), ca)| *aa == ca.get_type())
                            {
                                return Some((
                                    self.composite_types[*candidate].composite_type.to_owned(),
                                    None,
                                ));
                            }
                        }
                        ast_types::CustomTypeOil::Enum(_) => (),
                    }
                }
            }
        }

        None
    }
}
