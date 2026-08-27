//! Mapping of `rustc`'s types to this compiler's [`DataType`]s.
//!
//! The types that are native to Triton VM -- `BFieldElement`,
//! `XFieldElement`, and `Digest` -- are recognized *by name* and mapped to
//! their opaque, fixed-size counterparts. Their representation on the host
//! machine is irrelevant for the generated code.

use itertools::Itertools;
use rustc_hir::attrs::lang_items::LangItem;
use rustc_hir::def::DefKind;
use rustc_hir::def_id::DefId;
use rustc_middle::ty;
use rustc_middle::ty::Ty;
use rustc_middle::ty::TyCtxt;
use rustc_span::sym;

use super::lower::Context;
use super::prelude;
use crate::ast;
use crate::ast::FnSignature;
use crate::ast_types;
use crate::ast_types::CustomTypeOil;
use crate::ast_types::DataType;
use crate::libraries::core::option_type;
use crate::libraries::core::result_type;
use crate::libraries::polynomial;
use crate::libraries::recufy;

impl<'a, 'tcx> Context<'a, 'tcx> {
    /// Whether the definition lives in the prelude, as opposed to the program
    /// or an external crate.
    pub(super) fn is_prelude_def(&self, def_id: DefId) -> bool {
        let Some(local) = def_id.as_local() else {
            return false;
        };
        let mut current = local.to_def_id();
        loop {
            if self.tcx.def_kind(current) == DefKind::Mod
                && self.tcx.opt_item_name(current) == Some(self.prelude_module_symbol)
            {
                return true;
            }
            let Some(parent) = self.tcx.opt_parent(current) else {
                return false;
            };
            current = parent;
        }
    }

    /// Whether the definition is part of the program being compiled.
    pub(super) fn is_program_def(&self, def_id: DefId) -> bool {
        def_id.is_local() && !self.is_prelude_def(def_id)
    }

    /// The name of the innermost module containing the definition, if any.
    pub(super) fn parent_module_name(&self, def_id: DefId) -> Option<String> {
        let parent = self.tcx.opt_parent(def_id)?;
        if self.tcx.def_kind(parent) != DefKind::Mod {
            return None;
        }
        Some(self.tcx.opt_item_name(parent)?.to_string())
    }

    /// Map a `rustc` type to a [`DataType`]. Composite types encountered on
    /// the way are registered with the compilation's composite types.
    pub(super) fn map_ty(&mut self, ty: Ty<'tcx>) -> DataType {
        match ty.kind() {
            ty::Bool => DataType::Bool,
            ty::Uint(ty::UintTy::U32 | ty::UintTy::Usize) => DataType::U32,
            ty::Uint(ty::UintTy::U64) => DataType::U64,
            ty::Uint(ty::UintTy::U128) => DataType::U128,
            ty::Tuple(elements) => {
                let elements = elements.iter().map(|elem| self.map_ty(elem)).collect_vec();
                DataType::Tuple(elements.into())
            }
            ty::Array(element_type, length) => {
                let length = evaluate_array_length(self.tcx, *length);
                DataType::Array(ast_types::ArrayType {
                    element_type: Box::new(self.map_ty(*element_type)),
                    length,
                })
            }
            ty::Ref(_, inner, _) => Self::boxed_once(self.map_ty(*inner)),
            ty::Slice(element_type) => DataType::List(Box::new(self.map_ty(*element_type))),
            ty::Adt(adt_def, args) => self.map_adt(adt_def.did(), args, ty),
            ty::FnDef(def_id, _) => {
                let signature = self.function_signature(*def_id);
                signature.into()
            }
            ty::Never => DataType::unit(),
            other => panic!("Unsupported type `{ty}` ({other:?})"),
        }
    }

    /// References are modelled as boxes, but boxes are not nested.
    pub(super) fn boxed_once(data_type: DataType) -> DataType {
        match data_type {
            DataType::Boxed(_) => data_type,
            other => DataType::Boxed(Box::new(other)),
        }
    }

    fn map_adt(&mut self, did: DefId, args: ty::GenericArgsRef<'tcx>, ty: Ty<'tcx>) -> DataType {
        let tcx = self.tcx;
        let name = tcx.item_name(did);

        if tcx.is_lang_item(did, LangItem::OwnedBox) {
            return DataType::Boxed(Box::new(self.map_ty(args.type_at(0))));
        }
        if tcx.is_diagnostic_item(sym::Vec, did) {
            return DataType::List(Box::new(self.map_ty(args.type_at(0))));
        }
        if tcx.is_diagnostic_item(sym::Option, did) {
            let payload_type = self.map_ty(args.type_at(0));
            let type_context = option_type::option_type(payload_type);
            self.composite_types
                .add_type_context_if_new(type_context.clone());
            return type_context.into();
        }
        if tcx.is_diagnostic_item(sym::Result, did) {
            let ok_type = self.map_ty(args.type_at(0));
            return result_type::wrap_and_import_result_type(ok_type, &mut self.composite_types);
        }

        if self.is_prelude_def(did) {
            match name.as_str() {
                "BFieldElement" => return DataType::Bfe,
                "XFieldElement" => return DataType::Xfe,
                "Digest" => return DataType::Digest,
                "Polynomial" => {
                    let coefficient_type = self.map_ty(args.types().next().unwrap());
                    let type_context = polynomial::polynomial_type(
                        coefficient_type
                            .try_into()
                            .expect("polynomial coefficients must be BFEs or XFEs"),
                    );
                    self.composite_types
                        .add_type_context_if_new(type_context.clone());
                    return type_context.into();
                }
                "VmProofIter" => {
                    let type_context = recufy::vm_proof_iter::vm_proof_iter_type_context(
                        &mut self.composite_types,
                    );
                    self.composite_types
                        .add_type_context_if_new(type_context.clone());
                    return type_context.into();
                }
                _ => (),
            }
        }

        // Structs and enums declared by the program (or in the prelude, e.g.
        // `Claim`), which are described by their definition.
        assert!(
            did.is_local(),
            "Unsupported type `{ty}` from an external crate"
        );
        let custom_type = self.custom_type_of_adt(did, ty);
        let data_type: DataType = custom_type.clone().into();
        if self.composite_types.get_by_type(&data_type).is_none() {
            self.composite_types.add_custom_type(custom_type);
        }

        data_type
    }

    /// Build the description of a struct or enum from its definition.
    fn custom_type_of_adt(&mut self, did: DefId, ty: Ty<'tcx>) -> CustomTypeOil {
        let tcx = self.tcx;
        let adt_def = tcx.adt_def(did);
        let ty::Adt(_, args) = ty.kind() else {
            unreachable!()
        };
        assert!(
            args.is_empty(),
            "Generic types are not supported. Got type `{ty}`"
        );
        let name = tcx.item_name(did).to_string();
        let is_copy = tcx.type_is_copy_modulo_regions(ty::TypingEnv::fully_monomorphized(), ty);

        if adt_def.is_enum() {
            let variants = adt_def
                .variants()
                .iter()
                .map(|variant| {
                    let field_types = variant
                        .fields
                        .iter()
                        .map(|field| {
                            self.map_ty(
                                tcx.type_of(field.did)
                                    .instantiate_identity()
                                    .skip_normalization(),
                            )
                        })
                        .collect_vec();
                    (
                        variant.name.to_string(),
                        DataType::Tuple(field_types.into()),
                    )
                })
                .collect_vec();
            return CustomTypeOil::Enum(ast_types::EnumType {
                name,
                is_copy,
                variants,
                is_prelude: false,
                type_parameter: None,
            });
        }

        let variant = adt_def.non_enum_variant();
        let struct_variant = match variant.ctor_kind() {
            // `struct Foo { a: u32 }`
            None => {
                let fields = variant
                    .fields
                    .iter()
                    .map(|field| {
                        let field_type = self.map_ty(
                            tcx.type_of(field.did)
                                .instantiate_identity()
                                .skip_normalization(),
                        );
                        (field.name.to_string(), field_type)
                    })
                    .collect_vec();
                ast_types::StructVariant::NamedFields(ast_types::NamedFieldsStruct { fields })
            }
            // `struct Foo(u32);` and `struct Foo;`
            Some(_) => {
                let field_types = variant
                    .fields
                    .iter()
                    .map(|field| {
                        self.map_ty(
                            tcx.type_of(field.did)
                                .instantiate_identity()
                                .skip_normalization(),
                        )
                    })
                    .collect_vec();
                ast_types::StructVariant::TupleStruct(field_types.into())
            }
        };

        CustomTypeOil::Struct(ast_types::StructType {
            name,
            is_copy,
            variant: struct_variant,
        })
    }

    /// The signature of a function or method declared in the program or in
    /// the prelude.
    pub(super) fn function_signature(&mut self, def_id: DefId) -> FnSignature {
        let tcx = self.tcx;
        let fn_sig = tcx
            .fn_sig(def_id)
            .instantiate_identity()
            .skip_normalization()
            .skip_binder();
        let param_names = tcx.fn_arg_idents(def_id);
        let args = fn_sig
            .inputs()
            .iter()
            .zip_eq(param_names.iter())
            .map(|(input_ty, ident)| {
                let name = ident.map(|ident| ident.to_string()).unwrap_or_default();
                let mutable = matches!(input_ty.kind(), ty::Ref(_, _, ty::Mutability::Mut));
                ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
                    name,
                    data_type: self.map_ty(*input_ty),
                    mutable,
                })
            })
            .collect_vec();
        let output = self.map_ty(fn_sig.output());

        FnSignature {
            name: self.function_name(def_id),
            args,
            output,
            arg_evaluation_order: Default::default(),
        }
    }

    /// The name used to refer to a function declared in the program: `foo`
    /// for free functions, `Foo::bar` for associated functions.
    pub(super) fn function_name(&self, def_id: DefId) -> String {
        let tcx = self.tcx;
        let name = tcx.item_name(def_id).to_string();
        match tcx.impl_of_assoc(def_id) {
            Some(impl_def_id) => {
                let self_ty = tcx
                    .type_of(impl_def_id)
                    .instantiate_identity()
                    .skip_normalization();
                format!("{}::{name}", type_head_name(tcx, self_ty))
            }
            None => name,
        }
    }
}

/// `Foo` for `Foo<T>`, `Vec` for `Vec<T>`, `[T; N]` for arrays, etc.
pub(super) fn type_head_name<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> String {
    match ty.kind() {
        ty::Adt(adt_def, _) => tcx.item_name(adt_def.did()).to_string(),
        ty::Uint(ty::UintTy::U32) => "u32".to_owned(),
        ty::Uint(ty::UintTy::Usize) => "usize".to_owned(),
        ty::Uint(ty::UintTy::U64) => "u64".to_owned(),
        ty::Uint(ty::UintTy::U128) => "u128".to_owned(),
        ty::Bool => "bool".to_owned(),
        _ => format!("{ty}"),
    }
}

/// Evaluate an array length, which may be given by a constant expression.
pub(super) fn evaluate_array_length<'tcx>(tcx: TyCtxt<'tcx>, length: ty::Const<'tcx>) -> usize {
    let length = tcx.normalize_erasing_regions(
        ty::TypingEnv::fully_monomorphized(),
        rustc_middle::ty::Unnormalized::new(length),
    );
    let length = length
        .try_to_target_usize(tcx)
        .expect("array length must be known at compile time");
    length as usize
}

/// Convenience: the prelude module's name as a symbol.
pub(super) fn prelude_module_symbol() -> rustc_span::Symbol {
    rustc_span::Symbol::intern(prelude::PRELUDE_MODULE_NAME)
}

/// The variant of a `match` arm binding: whether it binds by reference.
pub(super) fn binding_is_by_ref(mode: rustc_hir::BindingMode) -> bool {
    matches!(mode.0, rustc_hir::ByRef::Yes(..))
}

pub(super) fn binding_is_mutable(mode: rustc_hir::BindingMode) -> bool {
    matches!(mode.1, ty::Mutability::Mut)
}

/// The data type of an AST node whose type must already be known.
pub(super) fn data_type_of(expr: &ast::Expr<crate::type_checker::Typing>) -> DataType {
    use crate::type_checker::GetType;
    expr.get_type()
}
