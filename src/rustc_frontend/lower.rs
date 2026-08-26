//! Lowering of THIR -- `rustc`'s *typed* high-level intermediate
//! representation -- into this compiler's AST.
//!
//! By the time THIR exists, `rustc` has parsed the program, resolved all
//! names, and type-checked everything: every expression carries its type,
//! every call site names the exact function it calls, and all method calls,
//! operator overloads, auto-referencing and auto-dereferencing have been made
//! explicit. This module maps that structure back onto the AST that the
//! code generator consumes, which is a slightly simpler, fully annotated
//! version of the same program.

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

use itertools::Itertools;
use num::One;
use num::Zero;
use rustc_hir::attrs::lang_items::LangItem;
use rustc_hir::def::DefKind;
use rustc_hir::def_id::DefId;
use rustc_hir::def_id::LocalDefId;
use rustc_middle::mir::BinOp as MirBinOp;
use rustc_middle::mir::UnOp as MirUnOp;
use rustc_middle::thir;
use rustc_middle::thir::ExprId;
use rustc_middle::thir::ExprKind;
use rustc_middle::thir::LocalVarId;
use rustc_middle::thir::PatKind;
use rustc_middle::thir::StmtKind;
use rustc_middle::thir::Thir;
use rustc_middle::ty;
use rustc_middle::ty::Ty;
use rustc_middle::ty::TyCtxt;
use rustc_span::hygiene::ExpnKind;
use rustc_span::hygiene::MacroKind;
use rustc_span::Span;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::math::traits::PrimitiveRootOfUnity;

use super::prelude;
use super::types::binding_is_by_ref;
use super::types::binding_is_mutable;
use super::types::data_type_of;
use super::types::evaluate_array_length;
use super::types::prelude_module_symbol;
use super::types::type_head_name;
use crate::ast;
use crate::ast::FnSignature;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::ast_types::FieldId;
use crate::composite_types::CompositeTypes;
use crate::libraries::Library;
use crate::type_checker::CheckState;
use crate::type_checker::Typing;

type Expr = ast::Expr<Typing>;
type Stmt = ast::Stmt<Typing>;
type Identifier = ast::Identifier<Typing>;

/// The result of lowering a program: the entrypoint, with all other
/// functions it (transitively) uses declared inside it, and all composite
/// types with their methods and associated functions.
pub(crate) struct LoweredProgram {
    pub(crate) entrypoint: ast::Fn<Typing>,
    pub(crate) composite_types: CompositeTypes,
}

/// State shared by the lowering of all bodies of a program.
pub(crate) struct Context<'a, 'tcx> {
    pub(super) tcx: TyCtxt<'tcx>,
    pub(super) libraries: &'a [Box<dyn Library>],
    pub(super) composite_types: CompositeTypes,
    pub(super) prelude_module_symbol: rustc_span::Symbol,

    /// Unique names of all functions declared by the program.
    function_names: HashMap<DefId, String>,

    /// Free functions used by the entrypoint, lowered or awaiting lowering.
    hoisted_functions: Vec<ast::Fn<Typing>>,
    hoisted_function_ids: HashSet<DefId>,
    functions_to_lower: VecDeque<DefId>,

    /// Signatures of all free functions, by name.
    ftable: HashMap<String, Vec<FnSignature>>,
}

pub(crate) fn lower_program<'a, 'tcx>(
    tcx: TyCtxt<'tcx>,
    libraries: &'a [Box<dyn Library>],
    entrypoint: &str,
) -> LoweredProgram {
    let mut context = Context {
        tcx,
        libraries,
        composite_types: CompositeTypes::default(),
        prelude_module_symbol: prelude_module_symbol(),
        function_names: HashMap::default(),
        hoisted_functions: vec![],
        hoisted_function_ids: HashSet::default(),
        functions_to_lower: VecDeque::default(),
        ftable: HashMap::default(),
    };

    context.assign_function_names();
    context.register_program_types();
    context.register_impl_item_signatures();

    let entrypoint_id = context
        .program_functions()
        .into_iter()
        .find(|def_id| {
            tcx.item_name(def_id.to_def_id()).as_str() == entrypoint
                && tcx.opt_parent(def_id.to_def_id())
                    == Some(rustc_hir::def_id::CRATE_DEF_ID.to_def_id())
        })
        .unwrap_or_else(|| panic!("Failed to locate entrypoint `{entrypoint}`"));
    context
        .hoisted_function_ids
        .insert(entrypoint_id.to_def_id());
    let mut entrypoint_fn = context.lower_function(entrypoint_id.to_def_id());

    // Lower all methods and associated functions of the program's types
    context.lower_all_impl_items();

    // Lower all free functions that are (transitively) used, and declare
    // them inside the entrypoint.
    while let Some(def_id) = context.functions_to_lower.pop_front() {
        let function = context.lower_function(def_id);
        context.hoisted_functions.push(function);
    }
    let ast::RoutineBody::Ast(body) = &mut entrypoint_fn.body else {
        unreachable!()
    };
    let declarations = context
        .hoisted_functions
        .drain(..)
        .map(Stmt::FnDeclaration)
        .collect_vec();
    body.splice(0..0, declarations);

    LoweredProgram {
        entrypoint: entrypoint_fn,
        composite_types: context.composite_types,
    }
}

impl<'a, 'tcx> Context<'a, 'tcx> {
    /// All functions and methods declared by the program.
    fn program_functions(&self) -> Vec<LocalDefId> {
        self.tcx
            .hir_body_owners()
            .filter(|def_id| {
                matches!(
                    self.tcx.def_kind(def_id.to_def_id()),
                    DefKind::Fn | DefKind::AssocFn
                )
            })
            .filter(|def_id| self.is_program_def(def_id.to_def_id()))
            .collect_vec()
    }

    /// Give every function a unique name. Nested functions can have the same
    /// name in Rust, but are all declared in the same scope by this compiler.
    fn assign_function_names(&mut self) {
        let mut seen: HashMap<String, usize> = HashMap::default();
        for def_id in self.program_functions() {
            let def_id = def_id.to_def_id();
            let name = self.function_name(def_id);
            let count = seen.entry(name.clone()).or_default();
            let unique_name = if *count == 0 {
                name
            } else {
                format!("{name}__{count}")
            };
            *count += 1;
            self.function_names.insert(def_id, unique_name);
        }

        for def_id in self.program_functions() {
            let def_id = def_id.to_def_id();
            if self.tcx.impl_of_assoc(def_id).is_some() {
                continue;
            }
            let signature = self.function_signature(def_id);
            self.ftable
                .entry(signature.name.clone())
                .or_default()
                .push(signature);
        }
    }

    /// Register all structs and enums declared by the program.
    fn register_program_types(&mut self) {
        let tcx = self.tcx;
        let items = tcx.hir_crate_items(());
        for item_id in items.free_items() {
            let def_id = item_id.owner_id.to_def_id();
            if !matches!(tcx.def_kind(def_id), DefKind::Struct | DefKind::Enum) {
                continue;
            }
            if !self.is_program_def(def_id) {
                continue;
            }
            let ty = tcx
                .type_of(def_id)
                .instantiate_identity()
                .skip_normalization();
            let _ = self.map_ty(ty);
        }
    }

    /// The methods and associated functions of the program's types, with the
    /// type each belongs to.
    fn program_impl_items(&mut self) -> Vec<(DefId, DataType)> {
        let mut items = vec![];
        for def_id in self.program_functions() {
            let def_id = def_id.to_def_id();
            let Some(impl_def_id) = self.tcx.impl_of_assoc(def_id) else {
                continue;
            };
            if self.tcx.impl_opt_trait_id(impl_def_id).is_some() {
                // Trait implementations are not compiled; `BFieldCodec` etc.
                // are provided by the compiler.
                continue;
            }
            let self_ty = self
                .tcx
                .type_of(impl_def_id)
                .instantiate_identity()
                .skip_normalization();
            items.push((def_id, self_ty));
        }
        let mut with_types = vec![];
        for (def_id, self_ty) in items {
            let self_type = self.map_ty(self_ty);
            with_types.push((def_id, self_type));
        }
        with_types
    }

    /// Declare all methods and associated functions of the program's types,
    /// so that calls to them can be resolved. Their bodies are lowered later.
    fn register_impl_item_signatures(&mut self) {
        for (def_id, self_type) in self.program_impl_items() {
            let mut signature = self.function_signature(def_id);
            // Methods and associated functions are known by their bare name
            signature.name = self.tcx.item_name(def_id).to_string();
            let placeholder = ast::RoutineBody::Ast(vec![]);
            let type_context = self
                .composite_types
                .get_mut_by_type(&self_type)
                .unwrap_or_else(|| panic!("Type {self_type} must be registered"));
            if self.tcx.associated_item(def_id).is_method() {
                type_context.add_method(ast::Method {
                    signature,
                    body: placeholder,
                });
            } else {
                type_context.add_associated_function(ast::Fn {
                    signature,
                    body: placeholder,
                });
            }
        }
    }

    /// Lower all methods and associated functions of the program's types.
    fn lower_all_impl_items(&mut self) {
        for (def_id, self_type) in self.program_impl_items() {
            let function = self.lower_function(def_id);
            let type_context = self
                .composite_types
                .get_mut_by_type(&self_type)
                .unwrap_or_else(|| panic!("Type {self_type} must be registered"));
            if self.tcx.associated_item(def_id).is_method() {
                let method = type_context
                    .methods
                    .iter_mut()
                    .find(|method| method.signature.name == function.signature.name)
                    .expect("method must have been declared");
                method.body = function.body;
            } else {
                let afunc = type_context
                    .associated_functions
                    .iter_mut()
                    .find(|afunc| afunc.signature.name == function.signature.name)
                    .expect("associated function must have been declared");
                afunc.body = function.body;
            }
        }
    }

    /// Lower one function body.
    fn lower_function(&mut self, def_id: DefId) -> ast::Fn<Typing> {
        let tcx = self.tcx;
        let (thir, root) = tcx
            .thir_body(def_id.expect_local())
            .expect("type-checked program must have THIR");
        let thir = thir.borrow();

        let mut lowerer = BodyLowerer {
            context: self,
            thir: &thir,
            var_names: HashMap::default(),
            used_var_names: HashMap::default(),
        };
        let args = lowerer.parameters();
        let output = lowerer.context.map_ty(
            tcx.fn_sig(def_id)
                .instantiate_identity()
                .skip_normalization()
                .skip_binder()
                .output(),
        );
        let name = if tcx.impl_of_assoc(def_id).is_some() {
            tcx.item_name(def_id).to_string()
        } else {
            lowerer.context.function_names[&def_id].clone()
        };
        let signature = FnSignature {
            name,
            args,
            output,
            arg_evaluation_order: Default::default(),
        };

        let mut body = lowerer.lower_function_body(root, &signature.output);
        if !matches!(body.last(), Some(Stmt::Return(_))) {
            body.push(Stmt::Return(None));
        }

        ast::Fn {
            signature,
            body: ast::RoutineBody::Ast(body),
        }
    }

    /// Note that a free function is used, so that it gets lowered.
    fn use_free_function(&mut self, def_id: DefId) -> String {
        if self.hoisted_function_ids.insert(def_id) {
            self.functions_to_lower.push_back(def_id);
        }
        self.function_names[&def_id].clone()
    }

    fn check_state(&self) -> CheckState {
        CheckState {
            ftable: self.ftable.clone(),
        }
    }
}

/// Lowering of one body.
struct BodyLowerer<'b, 'a, 'tcx> {
    context: &'b mut Context<'a, 'tcx>,
    thir: &'b Thir<'tcx>,

    /// Names of the variables bound in this body.
    var_names: HashMap<LocalVarId, String>,

    /// How often each name has been bound, for disambiguation.
    used_var_names: HashMap<String, usize>,
}

impl<'b, 'a, 'tcx> BodyLowerer<'b, 'a, 'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.context.tcx
    }

    fn expr(&self, id: ExprId) -> &'b thir::Expr<'tcx> {
        &self.thir.exprs[id]
    }

    /// Declare a variable. The returned name is unique within the body.
    fn declare_var(&mut self, var: LocalVarId, name: &str) -> String {
        let count = self.used_var_names.entry(name.to_owned()).or_default();
        let unique_name = if *count == 0 {
            name.to_owned()
        } else {
            format!("{name}_{count}")
        };
        *count += 1;
        self.var_names.insert(var, unique_name.clone());
        unique_name
    }

    fn var_name(&self, var: LocalVarId) -> String {
        match self.var_names.get(&var) {
            Some(name) => name.clone(),
            None => self.tcx().hir_name(var.0).to_string(),
        }
    }

    fn parameters(&mut self) -> Vec<ast_types::AbstractArgument> {
        let mut args = vec![];
        for param in self.thir.params.iter() {
            let Some(pat) = &param.pat else {
                panic!("Function parameters must be named");
            };
            let PatKind::Binding {
                name, mode, var, ..
            } = &pat.kind
            else {
                panic!("Function parameters must be simple bindings");
            };
            let name = self.declare_var(*var, name.as_str());
            let mutable = binding_is_mutable(*mode)
                || matches!(param.ty.kind(), ty::Ref(_, _, ty::Mutability::Mut));
            let data_type = self.context.map_ty(param.ty);
            args.push(ast_types::AbstractArgument::ValueArgument(
                ast_types::AbstractValueArg {
                    name,
                    data_type,
                    mutable,
                },
            ));
        }

        args
    }

    /* ---------------------------------------------------------------- */
    /* Statements                                                       */
    /* ---------------------------------------------------------------- */

    fn lower_function_body(&mut self, root: ExprId, output: &DataType) -> Vec<Stmt> {
        let root = self.strip(root);
        let ExprKind::Block { block } = &self.expr(root).kind else {
            panic!("Function body must be a block");
        };
        let block = &self.thir.blocks[*block];
        let mut stmts = self.lower_stmts(&block.stmts);
        if let Some(trailing) = block.expr {
            if output.is_unit() && self.expr(self.strip(trailing)).ty.is_unit() {
                self.lower_expr_as_stmts(trailing, &mut stmts);
            } else {
                let return_expr = self.lower_expr(trailing);
                stmts.push(Stmt::Return(Some(return_expr)));
            }
        }

        stmts
    }

    fn lower_stmts(&mut self, stmt_ids: &[thir::StmtId]) -> Vec<Stmt> {
        let mut stmts = vec![];
        for stmt_id in stmt_ids {
            self.lower_stmt(*stmt_id, &mut stmts);
        }
        stmts
    }

    /// Lower a block that appears in statement position.
    fn lower_block_stmt(&mut self, block_id: thir::BlockId) -> ast::BlockStmt<Typing> {
        let block = &self.thir.blocks[block_id];
        let mut stmts = self.lower_stmts(&block.stmts);
        if let Some(trailing) = block.expr {
            self.lower_expr_as_stmts(trailing, &mut stmts);
        }
        ast::BlockStmt { stmts }
    }

    fn lower_stmt(&mut self, stmt_id: thir::StmtId, out: &mut Vec<Stmt>) {
        let stmt = &self.thir.stmts[stmt_id];
        match &stmt.kind {
            StmtKind::Expr { expr, .. } => self.lower_expr_as_stmts(*expr, out),
            StmtKind::Let {
                pattern,
                initializer,
                else_block,
                ..
            } => {
                assert!(else_block.is_none(), "`let ... else` is not supported");
                let Some(initializer) = initializer else {
                    panic!("Variables must be initialized when declared");
                };
                let expr = self.lower_expr(*initializer);
                self.lower_let_pattern(pattern, expr, out);
            }
        }
    }

    fn lower_let_pattern(&mut self, pattern: &thir::Pat<'tcx>, expr: Expr, out: &mut Vec<Stmt>) {
        match &pattern.kind {
            PatKind::Binding {
                name,
                mode,
                var,
                subpattern: None,
                ..
            } => {
                assert!(
                    !binding_is_by_ref(*mode),
                    "`ref` bindings are not supported"
                );
                let data_type = data_type_of(&expr);
                let var_name = self.declare_var(*var, name.as_str());
                out.push(Stmt::Let(ast::LetStmt {
                    var_name,
                    mutable: binding_is_mutable(*mode),
                    data_type,
                    expr,
                }));
            }
            PatKind::Wild => {
                // `let _ = expr;` evaluates the expression for its effects.
                self.push_expr_as_stmt(expr, out);
            }
            PatKind::Leaf { subpatterns } => {
                let data_type = data_type_of(&expr);
                match &data_type {
                    // `let Digest([d0, d1, d2, d3, d4]) = digest;`
                    DataType::Digest => {
                        let [field_pat] = &subpatterns[..] else {
                            panic!("Destructuring a digest must bind its five elements");
                        };
                        let PatKind::Array { prefix, .. } = &field_pat.pattern.kind else {
                            panic!("Destructuring a digest must bind its five elements");
                        };
                        let bindings = prefix
                            .iter()
                            .map(|pat| self.pattern_binding(pat))
                            .collect_vec();
                        let ident = self.identifier_or_temporary(expr, out);
                        out.push(Stmt::TupleDestructuring(ast::TupleDestructStmt {
                            bindings,
                            ident,
                        }));
                    }
                    // `let (a, b) = pair;`
                    DataType::Tuple(_) => {
                        let ident = self.identifier_or_temporary(expr, out);
                        for field_pat in subpatterns {
                            let field_id: FieldId = field_pat.field.as_usize().into();
                            let field_type = data_type.field_access_returned_type(&field_id);
                            let field_expr = Expr::Var(Identifier::Field(
                                Box::new(ident.clone()),
                                field_id,
                                Typing::KnownType(field_type),
                            ));
                            self.lower_let_pattern(&field_pat.pattern, field_expr, out);
                        }
                    }
                    other => panic!("Cannot destructure value of type {other}"),
                }
            }
            other => panic!("Unsupported pattern in `let`: {other:?}"),
        }
    }

    fn pattern_binding(&mut self, pat: &thir::Pat<'tcx>) -> ast::PatternMatchedBinding {
        let PatKind::Binding {
            name, mode, var, ..
        } = &pat.kind
        else {
            panic!("Expected a simple binding, got pattern {pat:?}");
        };
        let name = self.declare_var(*var, name.as_str());
        ast::PatternMatchedBinding {
            name,
            mutable: binding_is_mutable(*mode),
        }
    }

    /// Destructuring needs a variable; introduce a temporary if the
    /// expression is not one already.
    fn identifier_or_temporary(&mut self, expr: Expr, out: &mut Vec<Stmt>) -> Identifier {
        if let Expr::Var(ident) = expr {
            return ident;
        }

        let data_type = data_type_of(&expr);
        let count = self
            .used_var_names
            .entry("__destructured".to_owned())
            .or_default();
        let var_name = format!("__destructured_{count}");
        *count += 1;
        out.push(Stmt::Let(ast::LetStmt {
            var_name: var_name.clone(),
            mutable: false,
            data_type: data_type.clone(),
            expr,
        }));

        Identifier::String(var_name, Typing::KnownType(data_type))
    }

    /// Lower an expression that appears in statement position.
    fn lower_expr_as_stmts(&mut self, expr_id: ExprId, out: &mut Vec<Stmt>) {
        let expr_id = self.strip(expr_id);
        let expr = self.expr(expr_id);
        match &expr.kind {
            ExprKind::Assign { lhs, rhs } => {
                let identifier = self.lower_place(*lhs);
                let expr = self.lower_expr(*rhs);
                out.push(Stmt::Assign(ast::AssignStmt { identifier, expr }));
            }
            ExprKind::AssignOp { op, lhs, rhs } => {
                let identifier = self.lower_place(*lhs);
                let lhs_expr = Expr::Var(identifier.clone());
                let rhs_expr = self.lower_expr(*rhs);
                let result_type = data_type_of(&lhs_expr);
                let binop = assign_op_to_binop(*op);
                let expr = Expr::Binop(
                    Box::new(lhs_expr),
                    binop,
                    Box::new(rhs_expr),
                    Typing::KnownType(result_type),
                );
                out.push(Stmt::Assign(ast::AssignStmt { identifier, expr }));
            }
            ExprKind::Loop { body } => out.push(self.lower_while_loop(*body)),
            ExprKind::If {
                cond,
                then,
                else_opt,
                ..
            } => {
                if let Some(assertion) = self.lower_assert_macro(expr.span, *cond, *then, *else_opt)
                {
                    out.push(assertion);
                    return;
                }
                let condition = self.lower_expr(*cond);
                let then_branch = self.lower_expr_as_block(*then);
                let else_branch = match else_opt {
                    Some(else_expr) => self.lower_expr_as_block(*else_expr),
                    None => ast::BlockStmt { stmts: vec![] },
                };
                out.push(Stmt::If(ast::IfStmt {
                    condition,
                    then_branch,
                    else_branch,
                }));
            }
            ExprKind::Match {
                scrutinee, arms, ..
            } if expr.ty.is_unit() => {
                let match_expression = self.lower_expr(*scrutinee);
                let arms = arms
                    .iter()
                    .map(|arm_id| {
                        let arm = &self.thir.arms[*arm_id];
                        assert!(arm.guard.is_none(), "Match guards are not supported");
                        let (match_condition, bindings) = self.lower_match_pattern(&arm.pattern);
                        let body = self.lower_expr_as_block(arm.body);
                        drop(bindings);
                        ast::MatchStmtArm {
                            match_condition,
                            body,
                        }
                    })
                    .collect_vec();
                out.push(Stmt::Match(ast::MatchStmt {
                    match_expression,
                    arms,
                }));
            }
            ExprKind::Block { block } => {
                let block = self.lower_block_stmt(*block);
                out.push(Stmt::Block(block));
            }
            ExprKind::Return { value } => {
                let value = value.map(|value| self.lower_expr(value));
                out.push(Stmt::Return(value));
            }
            ExprKind::Break { .. } | ExprKind::Continue { .. } => {
                panic!("`break` and `continue` are not supported")
            }
            ExprKind::Tuple { fields } if fields.is_empty() => (),
            ExprKind::Call { .. } if self.is_println(expr.span) => (),
            ExprKind::Call { .. } if self.is_panic_call(expr_id) => {
                out.push(Stmt::Panic(ast::PanicMacro))
            }
            _ => {
                if let Some(assignment) = self.try_lower_compound_assignment(expr_id) {
                    out.push(assignment);
                    return;
                }
                let expr = self.lower_expr(expr_id);
                self.push_expr_as_stmt(expr, out);
            }
        }
    }

    /// Turn an already lowered expression into a statement.
    fn push_expr_as_stmt(&mut self, expr: Expr, out: &mut Vec<Stmt>) {
        match expr {
            Expr::FnCall(fn_call) => out.push(Stmt::FnCall(fn_call)),
            Expr::MethodCall(method_call) => out.push(Stmt::MethodCall(method_call)),
            Expr::Panic(_, _) => out.push(Stmt::Panic(ast::PanicMacro)),
            Expr::ReturningBlock(block) => {
                let mut stmts = block.stmts;
                self.push_expr_as_stmt(block.return_expr, &mut stmts);
                out.push(Stmt::Block(ast::BlockStmt { stmts }));
            }
            Expr::Lit(_) | Expr::Var(_) | Expr::Tuple(_) => out.push(Stmt::Nop),
            other => panic!("Unsupported expression in statement position: {other}"),
        }
    }

    /// Lower an expression (usually a block) as a block statement.
    fn lower_expr_as_block(&mut self, expr_id: ExprId) -> ast::BlockStmt<Typing> {
        let stripped = self.strip(expr_id);
        if let ExprKind::Block { block } = &self.expr(stripped).kind {
            return self.lower_block_stmt(*block);
        }
        let mut stmts = vec![];
        self.lower_expr_as_stmts(expr_id, &mut stmts);
        ast::BlockStmt { stmts }
    }

    /// `while cond { body }` is desugared to
    /// `loop { if cond { body } else { break } }` by `rustc`.
    fn lower_while_loop(&mut self, body: ExprId) -> Stmt {
        let unsupported =
            || panic!("Only `while` loops are supported, not `loop`, `for`, or `while let`");
        let body = self.strip(body);
        let ExprKind::Block { block } = &self.expr(body).kind else {
            unsupported()
        };
        let block = &self.thir.blocks[*block];
        if !block.stmts.is_empty() {
            unsupported()
        }
        let Some(if_expr) = block.expr else {
            unsupported()
        };
        let if_expr = self.strip(if_expr);
        let ExprKind::If {
            cond,
            then,
            else_opt: Some(else_expr),
            ..
        } = &self.expr(if_expr).kind
        else {
            unsupported()
        };
        if !self.is_break_block(*else_expr) {
            unsupported()
        }

        let condition = self.lower_expr(*cond);
        let block = self.lower_expr_as_block(*then);
        Stmt::While(ast::WhileStmt { condition, block })
    }

    fn is_break_block(&self, expr_id: ExprId) -> bool {
        let expr_id = self.strip(expr_id);
        match &self.expr(expr_id).kind {
            ExprKind::Break { value: None, .. } => true,
            ExprKind::Block { block } => {
                let block = &self.thir.blocks[*block];
                match (&block.stmts[..], block.expr) {
                    ([], Some(expr)) => self.is_break_block(expr),
                    ([stmt], None) => match &self.thir.stmts[*stmt].kind {
                        StmtKind::Expr { expr, .. } => self.is_break_block(*expr),
                        _ => false,
                    },
                    _ => false,
                }
            }
            _ => false,
        }
    }

    /// `assert!(cond)` expands to `if !cond { panic(...) }`.
    fn lower_assert_macro(
        &mut self,
        span: Span,
        cond: ExprId,
        then: ExprId,
        else_opt: Option<ExprId>,
    ) -> Option<Stmt> {
        if !span_is_from_macro(span, "assert") || else_opt.is_some() {
            return None;
        }
        let cond = self.strip(cond);
        let ExprKind::Unary {
            op: MirUnOp::Not,
            arg,
        } = &self.expr(cond).kind
        else {
            return None;
        };
        let then = self.strip(then);
        let ExprKind::Block { block } = &self.expr(then).kind else {
            return None;
        };
        let block = &self.thir.blocks[*block];
        let panics = match (&block.stmts[..], block.expr) {
            ([], Some(expr)) => self.is_panic_call(expr),
            ([stmt], None) => match &self.thir.stmts[*stmt].kind {
                StmtKind::Expr { expr, .. } => self.is_panic_call(*expr),
                _ => false,
            },
            _ => false,
        };
        if !panics {
            return None;
        }

        let expression = self.lower_expr(*arg);
        Some(Stmt::Assert(ast::AssertStmt { expression }))
    }

    fn is_panic_call(&self, expr_id: ExprId) -> bool {
        let expr_id = self.strip(expr_id);
        let ExprKind::Call { fun, .. } = &self.expr(expr_id).kind else {
            return false;
        };
        let Some((def_id, _)) = self.callee(*fun) else {
            return false;
        };
        let tcx = self.tcx();
        tcx.crate_name(def_id.krate).as_str() == "core"
            && tcx.def_path_str(def_id).starts_with("core::panicking::")
    }

    fn is_println(&self, span: Span) -> bool {
        span_is_from_macro(span, "println") || span_is_from_macro(span, "print")
    }

    /* ---------------------------------------------------------------- */
    /* Expressions                                                      */
    /* ---------------------------------------------------------------- */

    /// Skip nodes that carry no information relevant to this compiler:
    /// scopes, type ascriptions, and the adjustments `rustc` inserts to make
    /// implicit conversions explicit.
    fn strip(&self, mut expr_id: ExprId) -> ExprId {
        loop {
            let expr = self.expr(expr_id);
            expr_id = match &expr.kind {
                ExprKind::Scope { value, .. } => *value,
                ExprKind::Use { source }
                | ExprKind::ValueTypeAscription { source, .. }
                | ExprKind::PlaceTypeAscription { source, .. }
                | ExprKind::PointerCoercion { source, .. }
                | ExprKind::Reborrow { source, .. } => *source,
                ExprKind::NeverToAny { source } => *source,
                ExprKind::Borrow { arg, .. } | ExprKind::Deref { arg }
                    if self.is_adjustment(*arg) =>
                {
                    *arg
                }
                // Deref coercions on types like `Vec` are calls to
                // `Deref::deref`, inserted by `rustc`.
                ExprKind::Call { fun, args, .. }
                    if args.len() == 1
                        && self.is_deref_call(*fun)
                        && self.is_adjustment(args[0]) =>
                {
                    args[0]
                }
                _ => return expr_id,
            };
        }
    }

    fn is_deref_call(&self, fun: ExprId) -> bool {
        let Some((def_id, _)) = self.callee(fun) else {
            return false;
        };
        let tcx = self.tcx();
        let Some(trait_def_id) = tcx.trait_of_assoc(def_id) else {
            return false;
        };
        tcx.is_lang_item(trait_def_id, LangItem::Deref)
            || tcx.is_lang_item(trait_def_id, LangItem::DerefMut)
    }

    /// Whether a `Borrow` or `Deref` node with the given operand was inserted
    /// by `rustc` (auto-ref, auto-deref), as opposed to being written in the
    /// program. Nodes written in the program are wrapped in a `Scope`;
    /// adjustments directly wrap the adjusted expression.
    fn is_adjustment(&self, operand: ExprId) -> bool {
        !matches!(self.expr(operand).kind, ExprKind::Scope { .. })
    }

    fn lower_expr(&mut self, expr_id: ExprId) -> Expr {
        let outer_ty = self.expr(expr_id).ty;
        let expr_id = self.strip(expr_id);
        let expr = self.expr(expr_id);
        match &expr.kind {
            ExprKind::Literal { lit, neg } => {
                assert!(!*neg, "Negative literals are not supported");
                self.lower_literal(&lit.node, expr.ty)
            }
            ExprKind::NonHirLiteral { lit, .. } => {
                let value = lit.to_bits_unchecked();
                self.literal_from_bits(value, expr.ty)
            }
            ExprKind::NamedConst { def_id, .. } => self.lower_named_const(*def_id, expr.ty),
            ExprKind::ZstLiteral { .. } => match expr.ty.kind() {
                ty::FnDef(def_id, _) => {
                    let name = self.context.use_free_function(*def_id);
                    let signature = self.context.function_signature(*def_id);
                    Expr::Var(Identifier::String(
                        name,
                        Typing::KnownType(signature.into()),
                    ))
                }
                _ => panic!("Unsupported zero-sized value of type {}", expr.ty),
            },
            ExprKind::VarRef { .. } | ExprKind::Field { .. } | ExprKind::Index { .. } => {
                Expr::Var(self.lower_place(expr_id))
            }
            ExprKind::Binary { op, lhs, rhs } => self.lower_binary(*op, *lhs, *rhs, expr.ty),
            ExprKind::LogicalOp { op, lhs, rhs } => {
                let binop = match op {
                    thir::LogicalOp::And => ast::BinOp::And,
                    thir::LogicalOp::Or => ast::BinOp::Or,
                };
                let lhs = self.lower_expr(*lhs);
                let rhs = self.lower_expr(*rhs);
                Expr::Binop(
                    Box::new(lhs),
                    binop,
                    Box::new(rhs),
                    Typing::KnownType(DataType::Bool),
                )
            }
            ExprKind::Unary { op, arg } => {
                let unaryop = match op {
                    MirUnOp::Not => ast::UnaryOp::Not,
                    MirUnOp::Neg => ast::UnaryOp::Neg,
                    MirUnOp::PtrMetadata => panic!("Unsupported unary operator"),
                };
                let arg = self.lower_expr(*arg);
                let result_type = self.context.map_ty(expr.ty);
                Expr::Unary(unaryop, Box::new(arg), Typing::KnownType(result_type))
            }
            ExprKind::Cast { source } => {
                let inner = self.lower_expr(*source);
                let target_type = self.context.map_ty(expr.ty);
                if data_type_of(&inner) == target_type {
                    inner
                } else {
                    Expr::Cast(Box::new(inner), target_type)
                }
            }
            ExprKind::Borrow { arg, borrow_kind } => {
                let inner = self.lower_expr(*arg);
                let result_type = Context::boxed_once(data_type_of(&inner));
                let mutable = !matches!(borrow_kind, rustc_middle::mir::BorrowKind::Shared);
                Expr::Unary(
                    ast::UnaryOp::Ref(mutable),
                    Box::new(inner),
                    Typing::KnownType(result_type),
                )
            }
            ExprKind::Deref { arg } => {
                if let Some(identifier) = self.try_lower_overloaded_index(*arg) {
                    return Expr::Var(identifier);
                }
                let inner = self.lower_expr(*arg);
                let DataType::Boxed(inner_type) = data_type_of(&inner) else {
                    panic!("Cannot dereference value of type {}", data_type_of(&inner));
                };
                Expr::Unary(
                    ast::UnaryOp::Deref,
                    Box::new(inner),
                    Typing::KnownType(*inner_type),
                )
            }
            ExprKind::If {
                cond,
                then,
                else_opt,
                ..
            } => {
                let condition = self.lower_expr(*cond);
                let then_branch = self.lower_returning_block(*then);
                let Some(else_expr) = else_opt else {
                    panic!("`if` expressions must have an `else` branch");
                };
                let else_branch = self.lower_returning_block(*else_expr);
                Expr::If(ast::ExprIf {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                })
            }
            ExprKind::Match {
                scrutinee,
                arms,
                match_source,
            } => self.lower_match_expr(*scrutinee, arms, *match_source, expr.ty),
            ExprKind::Block { .. } => {
                Expr::ReturningBlock(Box::new(self.lower_returning_block(expr_id)))
            }
            ExprKind::Call { fun, args, .. } => {
                if self.is_panic_call(expr_id) {
                    let panic_type = self.context.map_ty(outer_ty);
                    return Expr::Panic(ast::PanicMacro, Typing::KnownType(panic_type));
                }
                self.lower_call(expr_id, *fun, args, expr.ty)
            }
            ExprKind::Tuple { fields } => {
                let elements = fields
                    .iter()
                    .map(|field| self.lower_expr(*field))
                    .collect_vec();
                Expr::Tuple(elements)
            }
            ExprKind::Array { fields } => {
                let elements = fields
                    .iter()
                    .map(|field| self.lower_expr(*field))
                    .collect_vec();
                let array_type = self.context.map_ty(expr.ty);
                Expr::Array(
                    ast::ArrayExpression::ElementsSpecified(elements),
                    Typing::KnownType(array_type),
                )
            }
            ExprKind::Repeat { value, count } => {
                let element = self.lower_expr(*value);
                let length = evaluate_array_length(self.tcx(), *count);
                let array_type = self.context.map_ty(expr.ty);
                Expr::Array(
                    ast::ArrayExpression::Repeat {
                        element: Box::new(element),
                        length,
                    },
                    Typing::KnownType(array_type),
                )
            }
            ExprKind::Adt(adt_expr) => self.lower_adt_expr(adt_expr, expr.ty),
            ExprKind::Return { .. } | ExprKind::Break { .. } | ExprKind::Loop { .. } => {
                panic!("Unsupported control flow in expression position")
            }
            ExprKind::Closure(_) => panic!("Closures are not supported"),
            other => panic!("Unsupported expression: {other:?}"),
        }
    }

    /// A block in expression position: statements followed by a value.
    fn lower_returning_block(&mut self, expr_id: ExprId) -> ast::ReturningBlock<Typing> {
        let outer_ty = self.expr(expr_id).ty;
        let stripped = self.strip(expr_id);
        let ExprKind::Block { block } = &self.expr(stripped).kind else {
            // e.g. `else if ...`, or a match arm without braces
            let return_expr = self.lower_expr(expr_id);
            return ast::ReturningBlock {
                stmts: vec![],
                return_expr,
            };
        };
        let block = &self.thir.blocks[*block];
        let mut stmts = self.lower_stmts(&block.stmts);
        let return_expr = match block.expr {
            Some(trailing) => self.lower_expr(trailing),
            None => {
                // A block ending in a `panic!();` is a valid value of any type.
                let result_type = self.context.map_ty(outer_ty);
                match stmts.pop() {
                    Some(Stmt::Panic(_)) => {
                        Expr::Panic(ast::PanicMacro, Typing::KnownType(result_type))
                    }
                    Some(other) => {
                        stmts.push(other);
                        assert!(result_type.is_unit(), "Block must have a value");
                        Expr::Tuple(vec![])
                    }
                    None => Expr::Tuple(vec![]),
                }
            }
        };

        ast::ReturningBlock { stmts, return_expr }
    }

    fn lower_literal(&mut self, lit: &rustc_ast::LitKind, ty: Ty<'tcx>) -> Expr {
        match lit {
            rustc_ast::LitKind::Bool(value) => Expr::Lit(ast::ExprLit::Bool(*value)),
            rustc_ast::LitKind::Int(value, _) => self.literal_from_bits(value.get(), ty),
            other => panic!("Unsupported literal: {other:?}"),
        }
    }

    fn literal_from_bits(&mut self, value: u128, ty: Ty<'tcx>) -> Expr {
        let lit = match ty.kind() {
            ty::Bool => ast::ExprLit::Bool(value != 0),
            ty::Uint(ty::UintTy::U32 | ty::UintTy::Usize) => {
                ast::ExprLit::U32(value.try_into().expect("literal must fit in u32"))
            }
            ty::Uint(ty::UintTy::U64) => ast::ExprLit::U64(value.try_into().unwrap()),
            ty::Uint(ty::UintTy::U128) => ast::ExprLit::U128(value),
            // Shift amounts default to `i32` in Rust; this compiler uses `u32`.
            ty::Int(ty::IntTy::I32) => {
                ast::ExprLit::U32(value.try_into().expect("literal must fit in u32"))
            }
            other => panic!("Unsupported literal type {other:?}"),
        };
        Expr::Lit(lit)
    }

    fn lower_named_const(&mut self, def_id: DefId, ty: Ty<'tcx>) -> Expr {
        let tcx = self.tcx();
        let value = tcx.const_eval_poly(def_id).unwrap_or_else(|_| {
            panic!("Constant `{}` must be evaluable", tcx.def_path_str(def_id))
        });
        let Some(scalar) = value.try_to_scalar_int() else {
            panic!(
                "Only constants of primitive type are supported. Got `{}`",
                tcx.def_path_str(def_id)
            );
        };
        self.literal_from_bits(scalar.to_bits_unchecked(), ty)
    }

    fn lower_binary(&mut self, op: MirBinOp, lhs: ExprId, rhs: ExprId, ty: Ty<'tcx>) -> Expr {
        let lhs = self.lower_expr(lhs);
        let rhs = self.lower_expr(rhs);
        let result_type = self.context.map_ty(ty);
        binop_expr(mir_binop_to_binop(op), lhs, rhs, result_type)
    }

    /* ---------------------------------------------------------------- */
    /* Places: variables, fields, indexing                              */
    /* ---------------------------------------------------------------- */

    fn lower_place(&mut self, expr_id: ExprId) -> Identifier {
        let expr_id = self.strip(expr_id);
        let expr = self.expr(expr_id);
        match &expr.kind {
            ExprKind::VarRef { id } | ExprKind::UpvarRef { var_hir_id: id, .. } => {
                let name = self.var_name(*id);
                let data_type = self.context.map_ty(expr.ty);
                Identifier::String(name, Typing::KnownType(data_type))
            }
            ExprKind::Field { lhs, name, .. } => {
                let base = self.lower_place(*lhs);
                let base_type = base_data_type(&base);
                let field_id = self.field_id(base_type.unbox(), name.as_usize());
                let field_type = base_type.field_access_returned_type(&field_id);
                Identifier::Field(Box::new(base), field_id, Typing::KnownType(field_type))
            }
            ExprKind::Index { lhs, index } => self.lower_index(*lhs, *index),
            ExprKind::Deref { arg } => {
                if let Some(identifier) = self.try_lower_overloaded_index(*arg) {
                    return identifier;
                }
                // `(*boxed).field`: a box is transparent for field access.
                let inner = self.lower_place(*arg);
                assert!(
                    matches!(base_data_type(&inner), DataType::Boxed(_)),
                    "Can only dereference boxed values"
                );
                inner
            }
            _ => {
                let lowered = self.lower_expr(expr_id);
                match lowered {
                    Expr::Var(identifier) => identifier,
                    other => panic!(
                        "Expected a variable, field access, or indexing expression. Got: {other}"
                    ),
                }
            }
        }
    }

    fn field_id(&self, base_type: DataType, field_index: usize) -> FieldId {
        match base_type {
            DataType::Struct(struct_type) => match struct_type.variant {
                ast_types::StructVariant::NamedFields(fields) => {
                    FieldId::NamedField(fields.fields[field_index].0.clone())
                }
                ast_types::StructVariant::TupleStruct(_) => field_index.into(),
            },
            DataType::Tuple(_) => field_index.into(),
            other => panic!("Cannot access field of value of type {other}"),
        }
    }

    /// `vec[i]` is `*Index::index(&vec, i)` after desugaring.
    fn try_lower_overloaded_index(&mut self, arg: ExprId) -> Option<Identifier> {
        let arg = self.strip(arg);
        let ExprKind::Call { fun, args, .. } = &self.expr(arg).kind else {
            return None;
        };
        let (def_id, _) = self.callee(*fun)?;
        let tcx = self.tcx();
        let trait_def_id = tcx.trait_of_assoc(def_id)?;
        if !tcx.is_lang_item(trait_def_id, LangItem::Index)
            && !tcx.is_lang_item(trait_def_id, LangItem::IndexMut)
        {
            return None;
        }
        let [lhs, index] = &args[..] else {
            return None;
        };
        Some(self.lower_index(*lhs, *index))
    }

    fn lower_index(&mut self, lhs: ExprId, index: ExprId) -> Identifier {
        let mut base = self.lower_place(lhs);
        let index = self.lower_expr(index);
        let index_expr = match index {
            Expr::Lit(ast::ExprLit::U32(value)) => ast::IndexExpr::Static(value as usize),
            other => ast::IndexExpr::Dynamic(other),
        };

        // Only lists and arrays can be indexed. A boxed sequence *is* a
        // sequence as far as the code generator is concerned, but its
        // non-copy elements are then reached through a pointer.
        let base_type = base_data_type(&base);
        let was_boxed = matches!(base_type, DataType::Boxed(_));
        let sequence_type = base_type.unbox();
        let element_type = match &sequence_type {
            DataType::List(element_type) => *element_type.clone(),
            DataType::Array(array_type) => *array_type.element_type.clone(),
            other => panic!("Cannot index into value of type {other}"),
        };
        if let (ast::IndexExpr::Static(index), DataType::Array(array_type)) =
            (&index_expr, &sequence_type)
        {
            assert!(
                *index < array_type.length,
                "Index {index} is out of bounds for array of length {}",
                array_type.length
            );
        }
        let element_type = if was_boxed && !element_type.is_copy() {
            DataType::Boxed(Box::new(element_type))
        } else {
            element_type
        };
        if was_boxed {
            base.force_type(&sequence_type);
        }

        Identifier::Index(
            Box::new(base),
            Box::new(index_expr),
            Typing::KnownType(element_type),
        )
    }

    /* ---------------------------------------------------------------- */
    /* Calls                                                            */
    /* ---------------------------------------------------------------- */

    /// The function called by a call expression, with its generic arguments.
    fn callee(&self, fun: ExprId) -> Option<(DefId, ty::GenericArgsRef<'tcx>)> {
        let fun = self.strip(fun);
        match self.expr(fun).ty.kind() {
            ty::FnDef(def_id, args) => Some((*def_id, args.skip_binder())),
            _ => None,
        }
    }

    fn lower_call(&mut self, expr_id: ExprId, fun: ExprId, args: &[ExprId], ty: Ty<'tcx>) -> Expr {
        let tcx = self.tcx();
        let Some((def_id, generic_args)) = self.callee(fun) else {
            panic!("Only calls to statically known functions are supported");
        };

        // Overloaded operators
        if let Some(trait_def_id) = tcx.trait_of_assoc(def_id) {
            if tcx.is_lang_item(trait_def_id, LangItem::Index)
                || tcx.is_lang_item(trait_def_id, LangItem::IndexMut)
            {
                let [lhs, index] = args else {
                    unreachable!("indexing takes two arguments")
                };
                return Expr::Var(self.lower_index(*lhs, *index));
            }
            if let Some(expr) = self.try_lower_operator(trait_def_id, def_id, args, ty) {
                return expr;
            }
        }

        // `T::decode(&tasm::load_from_memory(address))`
        if let Some(expr) = self.try_lower_decode(def_id, generic_args, args) {
            return expr;
        }

        // `x.into_iter().map(f).collect_vec()`
        if let Some(expr) = self.try_lower_map(def_id, args) {
            return expr;
        }

        let name = tcx.item_name(def_id).to_string();
        let is_method = tcx
            .opt_associated_item(def_id)
            .is_some_and(|assoc| assoc.is_method());

        if is_method {
            return self.lower_method_call(def_id, &name, args, ty);
        }

        if self.context.is_program_def(def_id) {
            let args = args.iter().map(|arg| self.lower_expr(*arg)).collect_vec();
            let name = match tcx.impl_of_assoc(def_id) {
                Some(_) => self.context.function_name(def_id),
                None => self.context.use_free_function(def_id),
            };
            let output = self.context.map_ty(ty);
            return Expr::FnCall(ast::FnCall {
                name,
                args,
                type_parameter: None,
                arg_evaluation_order: Default::default(),
                annot: Typing::KnownType(output),
                qualified_self_type: None,
            });
        }

        self.lower_library_function_call(expr_id, def_id, generic_args, &name, args, ty)
    }

    /// Calls to functions provided by the prelude or by `core`: functions
    /// like `BFieldElement::new`, `Vec::default`, `tasm::tasmlib_*`.
    fn lower_library_function_call(
        &mut self,
        _expr_id: ExprId,
        def_id: DefId,
        generic_args: ty::GenericArgsRef<'tcx>,
        name: &str,
        args: &[ExprId],
        ty: Ty<'tcx>,
    ) -> Expr {
        let tcx = self.tcx();
        let lowered_args = args.iter().map(|arg| self.lower_expr(*arg)).collect_vec();

        // The type the function is associated with, e.g. `Vec` in
        // `Vec::<u32>::default()`.
        let self_ty: Option<Ty<'tcx>> = if let Some(impl_def_id) = tcx.impl_of_assoc(def_id) {
            // Instantiate the impl's generics with those of the call
            Some(
                tcx.type_of(impl_def_id)
                    .instantiate(tcx, generic_args)
                    .skip_normalization(),
            )
        } else if tcx.trait_of_assoc(def_id).is_some() {
            Some(generic_args.type_at(0))
        } else {
            None
        };

        if let Some(self_ty) = self_ty {
            if let ty::Adt(adt_def, _) = self_ty.kind() {
                if self.context.is_prelude_def(adt_def.did())
                    && ![
                        "BFieldElement",
                        "XFieldElement",
                        "Digest",
                        "Tip5",
                        "Tip5WithState",
                    ]
                    .contains(&tcx.item_name(adt_def.did()).as_str())
                {
                    // Registers the type and its methods, if not yet done
                    let _ = self.context.map_ty(self_ty);
                }
            }
        }

        let (full_name, qualified_self_type, type_parameter) = match self_ty {
            Some(self_ty) => match self_ty.kind() {
                ty::Array(..) => {
                    let array_type = self.context.map_ty(self_ty);
                    (name.to_owned(), Some(array_type), None)
                }
                ty::Adt(_, adt_args) => {
                    let type_parameter = adt_args
                        .types()
                        .next()
                        .map(|type_arg| self.context.map_ty(type_arg));
                    let type_parameter = type_parameter.or_else(|| {
                        generic_args
                            .types()
                            .next()
                            .map(|type_arg| self.context.map_ty(type_arg))
                    });
                    let full_name = format!("{}::{name}", type_head_name(tcx, self_ty));
                    (full_name, None, type_parameter)
                }
                _ => {
                    let full_name = format!("{}::{name}", type_head_name(tcx, self_ty));
                    (full_name, None, None)
                }
            },
            None => {
                // A free function in a module of the prelude: `tasm::foo`
                let module = self.context.parent_module_name(def_id);
                let full_name = match module.as_deref() {
                    Some(prelude::TASM_MODULE_NAME) => {
                        format!("{}::{name}", prelude::TASM_MODULE_NAME)
                    }
                    Some(prelude::BFIELD_CODEC_MODULE_NAME) => {
                        format!("{}::{name}", prelude::BFIELD_CODEC_MODULE_NAME)
                    }
                    _ => panic!("Unsupported function `{}`", tcx.def_path_str(def_id)),
                };
                let type_parameter = generic_args
                    .types()
                    .next()
                    .map(|type_arg| self.context.map_ty(type_arg));
                (full_name, None, type_parameter)
            }
        };

        // Values that are known at compile time
        if let Some(literal) = self.try_fold_constant_constructor(&full_name, &lowered_args) {
            return literal;
        }

        // `bfield_codec::decode_from_memory::<T>(address)`
        if full_name.starts_with(&format!("{}::", prelude::BFIELD_CODEC_MODULE_NAME)) {
            let Some(mem_pointer_declared_type) = type_parameter else {
                panic!("`{full_name}` needs an explicit type argument");
            };
            return memory_location(lowered_args[0].clone(), mem_pointer_declared_type);
        }

        // Associated functions of types provided by the prelude that behave
        // like user-defined types, e.g. `VmProofIter::new()`.
        if let Some(afunc) = self
            .context
            .composite_types
            .get_associated_function(&full_name)
        {
            return Expr::FnCall(ast::FnCall {
                name: full_name,
                args: lowered_args,
                type_parameter: None,
                arg_evaluation_order: afunc.signature.arg_evaluation_order,
                annot: Typing::KnownType(afunc.signature.output),
                qualified_self_type: None,
            });
        }

        // Everything else must be known to a library
        let libraries = self.context.libraries;
        for lib in libraries.iter() {
            if lib.handle_function_call(&full_name, &qualified_self_type) {
                let signature = lib.function_name_to_signature(
                    &full_name,
                    type_parameter.clone(),
                    &lowered_args,
                    &qualified_self_type,
                    &mut self.context.composite_types,
                );
                let output = if signature.output == DataType::VoidPointer {
                    self.context.map_ty(ty)
                } else {
                    signature.output
                };
                return Expr::FnCall(ast::FnCall {
                    name: full_name,
                    args: lowered_args,
                    type_parameter,
                    arg_evaluation_order: signature.arg_evaluation_order,
                    annot: Typing::KnownType(output),
                    qualified_self_type,
                });
            }
        }

        panic!(
            "Unsupported function `{full_name}` (`{}`)",
            tcx.def_path_str(def_id)
        )
    }

    /// Constructors of native values with constant arguments are evaluated
    /// at compile time.
    fn try_fold_constant_constructor(&self, full_name: &str, args: &[Expr]) -> Option<Expr> {
        let as_bfe = |expr: &Expr| match expr {
            Expr::Lit(ast::ExprLit::Bfe(bfe)) => Some(*bfe),
            Expr::Lit(ast::ExprLit::U64(value)) => Some(BFieldElement::new(*value)),
            Expr::Lit(ast::ExprLit::U32(value)) => Some(BFieldElement::new(*value as u64)),
            _ => None,
        };
        let as_bfe_array = |expr: &Expr| match expr {
            Expr::Array(ast::ArrayExpression::ElementsSpecified(elements), _) => {
                elements.iter().map(as_bfe).collect::<Option<Vec<_>>>()
            }
            _ => None,
        };

        let literal = match (full_name, args) {
            ("BFieldElement::new", [arg]) => ast::ExprLit::Bfe(as_bfe(arg)?),
            ("BFieldElement::zero", []) => ast::ExprLit::Bfe(BFieldElement::zero()),
            ("BFieldElement::one", []) => ast::ExprLit::Bfe(BFieldElement::one()),
            ("BFieldElement::primitive_root_of_unity", [arg]) => {
                let order = match arg {
                    Expr::Lit(ast::ExprLit::U64(value)) => *value,
                    Expr::Lit(ast::ExprLit::U32(value)) => *value as u64,
                    _ => return None,
                };
                ast::ExprLit::Bfe(
                    BFieldElement::primitive_root_of_unity(order)
                        .unwrap_or_else(|| panic!("Primitive root of order {order} must exist")),
                )
            }
            ("XFieldElement::new", [arg]) => {
                let coefficients: [BFieldElement; 3] = as_bfe_array(arg)?.try_into().ok()?;
                ast::ExprLit::Xfe(XFieldElement::new(coefficients))
            }
            ("XFieldElement::new_const", [arg]) => {
                ast::ExprLit::Xfe(XFieldElement::new_const(as_bfe(arg)?))
            }
            ("XFieldElement::zero", []) => ast::ExprLit::Xfe(XFieldElement::zero()),
            ("XFieldElement::one", []) => ast::ExprLit::Xfe(XFieldElement::one()),
            ("Digest::new", [arg]) => {
                let elements: [BFieldElement; Digest::LEN] = as_bfe_array(arg)?.try_into().ok()?;
                ast::ExprLit::Digest(Digest::new(elements))
            }
            ("Digest::default", []) => ast::ExprLit::Digest(Digest::default()),
            _ => return None,
        };

        Some(Expr::Lit(literal))
    }

    /// `T::decode(&tasm::load_from_memory(address)).unwrap()` reads a value
    /// of type `T` from memory, starting at `address`.
    fn try_lower_decode(
        &mut self,
        def_id: DefId,
        generic_args: ty::GenericArgsRef<'tcx>,
        args: &[ExprId],
    ) -> Option<Expr> {
        let tcx = self.tcx();
        if tcx.item_name(def_id).as_str() != "decode" {
            return None;
        }
        let trait_def_id = tcx.trait_of_assoc(def_id)?;
        if tcx.item_name(trait_def_id).as_str() != "BFieldCodec" {
            return None;
        }
        let [arg] = args else {
            return None;
        };

        let arg = self.strip(*arg);
        let load_call = match &self.expr(arg).kind {
            ExprKind::Borrow { arg, .. } => self.strip(*arg),
            _ => arg,
        };
        let ExprKind::Call {
            fun,
            args: load_args,
            ..
        } = &self.expr(load_call).kind
        else {
            panic!("`decode` is only supported as `T::decode(&tasm::load_from_memory(address))`");
        };
        let (load_def_id, _) = self.callee(*fun).unwrap();
        assert_eq!(
            tcx.item_name(load_def_id).as_str(),
            "load_from_memory",
            "`decode` is only supported as `T::decode(&tasm::load_from_memory(address))`"
        );
        let address = self.lower_expr(load_args[0]);
        let decoded_type = self.context.map_ty(generic_args.type_at(0));

        Some(memory_location(address, decoded_type))
    }

    /// `x.into_iter().map(f).collect_vec()` maps a function over a list.
    fn try_lower_map(&mut self, def_id: DefId, args: &[ExprId]) -> Option<Expr> {
        let tcx = self.tcx();
        if tcx.item_name(def_id).as_str() != "collect_vec" {
            return None;
        }
        let [iterator] = args else {
            return None;
        };
        let iterator = self.strip(*iterator);
        let ExprKind::Call {
            fun,
            args: map_args,
            ..
        } = &self.expr(iterator).kind
        else {
            return None;
        };
        let (map_def_id, _) = self.callee(*fun)?;
        if tcx.item_name(map_def_id).as_str() != "map" {
            return None;
        }
        let [into_iter, function] = &map_args[..] else {
            return None;
        };
        let into_iter = self.strip(*into_iter);
        let ExprKind::Call {
            fun,
            args: into_iter_args,
            ..
        } = &self.expr(into_iter).kind
        else {
            panic!("`map` is only supported as `list.into_iter().map(f).collect_vec()`");
        };
        let (into_iter_def_id, _) = self.callee(*fun)?;
        assert_eq!(
            tcx.item_name(into_iter_def_id).as_str(),
            "into_iter",
            "`map` is only supported as `list.into_iter().map(f).collect_vec()`"
        );

        let list = self.lower_expr(into_iter_args[0]);
        let function = self.lower_expr(*function);
        let args = vec![list, function];
        Some(self.resolve_method_call("map", args))
    }

    /// Overloaded operators on native types: `a + b` is `Add::add(a, b)`.
    fn try_lower_operator(
        &mut self,
        trait_def_id: DefId,
        method_def_id: DefId,
        args: &[ExprId],
        ty: Ty<'tcx>,
    ) -> Option<Expr> {
        let tcx = self.tcx();
        let lang_item = tcx.as_lang_item(trait_def_id)?;
        let binop = match lang_item {
            LangItem::Add => ast::BinOp::Add,
            LangItem::Sub => ast::BinOp::Sub,
            LangItem::Mul => ast::BinOp::Mul,
            LangItem::Div => ast::BinOp::Div,
            LangItem::Rem => ast::BinOp::Rem,
            LangItem::BitAnd => ast::BinOp::BitAnd,
            LangItem::BitOr => ast::BinOp::BitOr,
            LangItem::BitXor => ast::BinOp::BitXor,
            LangItem::Shl => ast::BinOp::Shl,
            LangItem::Shr => ast::BinOp::Shr,
            LangItem::PartialEq | LangItem::PartialOrd => {
                let [lhs, rhs] = args else {
                    return None;
                };
                let lhs = self.lower_expr(*lhs);
                let rhs = self.lower_expr(*rhs);
                let mir_op = match tcx.item_name(method_def_id).as_str() {
                    "eq" => MirBinOp::Eq,
                    "ne" => MirBinOp::Ne,
                    "lt" => MirBinOp::Lt,
                    "le" => MirBinOp::Le,
                    "gt" => MirBinOp::Gt,
                    "ge" => MirBinOp::Ge,
                    _ => return None,
                };
                return Some(binop_expr(
                    mir_binop_to_binop(mir_op),
                    lhs,
                    rhs,
                    DataType::Bool,
                ));
            }
            LangItem::Neg | LangItem::Not => {
                let [arg] = args else {
                    return None;
                };
                let arg = self.lower_expr(*arg);
                let unaryop = match lang_item {
                    LangItem::Neg => ast::UnaryOp::Neg,
                    _ => ast::UnaryOp::Not,
                };
                let result_type = self.context.map_ty(ty);
                return Some(Expr::Unary(
                    unaryop,
                    Box::new(arg),
                    Typing::KnownType(result_type),
                ));
            }
            LangItem::AddAssign
            | LangItem::SubAssign
            | LangItem::MulAssign
            | LangItem::DivAssign
            | LangItem::RemAssign
            | LangItem::BitAndAssign
            | LangItem::BitOrAssign
            | LangItem::BitXorAssign
            | LangItem::ShlAssign
            | LangItem::ShrAssign => {
                panic!("Compound assignment must appear in statement position")
            }
            _ => return None,
        };

        let [lhs, rhs] = args else {
            return None;
        };
        let lhs = self.lower_expr(*lhs);
        let rhs = self.lower_expr(*rhs);
        let result_type = self.context.map_ty(ty);
        Some(binop_expr(
            LoweredBinOp::Direct(binop),
            lhs,
            rhs,
            result_type,
        ))
    }

    /// Compound assignments on native types: `a += b` is
    /// `AddAssign::add_assign(&mut a, b)`.
    fn try_lower_compound_assignment(&mut self, expr_id: ExprId) -> Option<Stmt> {
        let expr_id = self.strip(expr_id);
        let ExprKind::Call { fun, args, .. } = &self.expr(expr_id).kind else {
            return None;
        };
        let (def_id, _) = self.callee(*fun)?;
        let tcx = self.tcx();
        let trait_def_id = tcx.trait_of_assoc(def_id)?;
        let binop = match tcx.as_lang_item(trait_def_id)? {
            LangItem::AddAssign => ast::BinOp::Add,
            LangItem::SubAssign => ast::BinOp::Sub,
            LangItem::MulAssign => ast::BinOp::Mul,
            LangItem::DivAssign => ast::BinOp::Div,
            LangItem::RemAssign => ast::BinOp::Rem,
            LangItem::BitAndAssign => ast::BinOp::BitAnd,
            LangItem::BitOrAssign => ast::BinOp::BitOr,
            LangItem::BitXorAssign => ast::BinOp::BitXor,
            LangItem::ShlAssign => ast::BinOp::Shl,
            LangItem::ShrAssign => ast::BinOp::Shr,
            _ => return None,
        };
        let [lhs, rhs] = &args[..] else {
            return None;
        };
        let identifier = self.lower_place(*lhs);
        let lhs_expr = Expr::Var(identifier.clone());
        let rhs_expr = self.lower_expr(*rhs);
        let result_type = data_type_of(&lhs_expr);
        let expr = binop_expr(LoweredBinOp::Direct(binop), lhs_expr, rhs_expr, result_type);
        Some(Stmt::Assign(ast::AssignStmt { identifier, expr }))
    }

    /// A method call, `receiver.method(args)`, where the method is provided
    /// by a library or by a type of the program.
    fn lower_method_call(
        &mut self,
        def_id: DefId,
        name: &str,
        args: &[ExprId],
        ty: Ty<'tcx>,
    ) -> Expr {
        let tcx = self.tcx();
        let lowered_args = args.iter().map(|arg| self.lower_expr(*arg)).collect_vec();

        // `x.unwrap()` where `x` already *is* the value: `list.pop()`,
        // `xfe.unlift()`, `BFieldElement::primitive_root_of_unity(n)`, and
        // reading from memory all produce the value directly.
        let receiver_is_value = matches!(lowered_args[0], Expr::MemoryLocation(_))
            || !matches!(data_type_of(&lowered_args[0]).unbox(), DataType::Enum(_));
        if name == "unwrap" && receiver_is_value {
            return lowered_args.into_iter().next().unwrap();
        }

        // Methods of the program's types are resolved like library methods,
        // by name and receiver type. Since `rustc` resolved the call already,
        // this cannot fail.
        if self.context.is_program_def(def_id) {
            let _ = ty;
        }
        let _ = tcx;

        self.resolve_method_call(name, lowered_args)
    }

    /// Find the signature of a method by its name and its receiver type,
    /// adjusting the receiver by dereferencing boxes until the method
    /// matches. This mirrors Rust's auto-deref rules for method calls.
    fn resolve_method_call(&mut self, method_name: &str, mut args: Vec<Expr>) -> Expr {
        let original_receiver_type = data_type_of(&args[0]);
        let mut forced_type = original_receiver_type.clone();
        let associated_type;
        let signature = 'resolution: loop {
            let dereferenced_type = forced_type.unbox();
            if let Some(type_context) = self.context.composite_types.get_by_type(&dereferenced_type)
            {
                if let Some(method) = type_context.get_method(method_name) {
                    let receiver_type = method.receiver_type();
                    if receiver_type == forced_type {
                        associated_type = Some(dereferenced_type);
                        break 'resolution method.signature.clone();
                    }
                    if !matches!(forced_type, DataType::Boxed(_)) {
                        panic!(
                            "Method `{method_name}` expects a receiver of type {receiver_type}, \
                             but is called on a value of type {original_receiver_type}. \
                             Methods taking `&self` can only be called on boxed values."
                        );
                    }
                }
            }

            let libraries = self.context.libraries;
            for lib in libraries.iter() {
                if lib.handle_method_call(method_name, &forced_type) {
                    associated_type = Some(forced_type.clone());
                    let check_state = self.context.check_state();
                    break 'resolution lib.method_name_to_signature(
                        method_name,
                        &forced_type,
                        &args,
                        &check_state,
                    );
                }
            }

            // Keep stripping `Box` until a match is found
            let DataType::Boxed(inner_type) = &forced_type else {
                panic!(
                    "Unknown method `{method_name}` on receiver of type {original_receiver_type}"
                );
            };
            forced_type = *inner_type.clone();
            let receiver = std::mem::replace(&mut args[0], Expr::Tuple(vec![]));
            args[0] = Expr::Unary(
                ast::UnaryOp::Deref,
                Box::new(receiver),
                Typing::KnownType(forced_type.clone()),
            );
        };

        Expr::MethodCall(ast::MethodCall {
            method_name: method_name.to_owned(),
            args,
            annot: Typing::KnownType(signature.output),
            associated_type,
        })
    }

    /* ---------------------------------------------------------------- */
    /* Structs, enums, and `match`                                      */
    /* ---------------------------------------------------------------- */

    fn lower_adt_expr(&mut self, adt_expr: &thir::AdtExpr<'tcx>, ty: Ty<'tcx>) -> Expr {
        let tcx = self.tcx();
        assert!(
            matches!(adt_expr.base, thir::AdtExprBase::None),
            "Struct update syntax is not supported"
        );
        let data_type = self.context.map_ty(ty);
        let variant = adt_expr.adt_def.variant(adt_expr.variant_index);

        // Fields in the order in which they were written
        let mut fields = adt_expr
            .fields
            .iter()
            .map(|field| (field.name.as_usize(), self.lower_expr(field.expr)))
            .collect_vec();

        if adt_expr.adt_def.is_enum() {
            let DataType::Enum(enum_type) = &data_type else {
                unreachable!()
            };
            let variant_name = variant.name.to_string();
            if fields.is_empty() {
                return Expr::EnumDeclaration(ast::EnumDeclaration {
                    enum_type: data_type.clone(),
                    variant_name,
                });
            }
            fields.sort_by_key(|(index, _)| *index);
            let args = fields.into_iter().map(|(_, expr)| expr).collect_vec();
            let constructor_name = if enum_type.is_prelude {
                variant_name
            } else {
                format!("{}::{variant_name}", enum_type.name)
            };
            return Expr::FnCall(ast::FnCall {
                name: constructor_name,
                args,
                type_parameter: None,
                arg_evaluation_order: Default::default(),
                annot: Typing::KnownType(data_type),
                qualified_self_type: None,
            });
        }

        let DataType::Struct(struct_type) = &data_type else {
            unreachable!()
        };
        match &struct_type.variant {
            ast_types::StructVariant::NamedFields(named_fields) => {
                // The code generator relies on declaration order
                fields.sort_by_key(|(index, _)| *index);
                let field_names_and_values = fields
                    .into_iter()
                    .map(|(index, expr)| (named_fields.fields[index].0.clone(), expr))
                    .collect_vec();
                Expr::Struct(ast::StructExpr {
                    struct_type: data_type.clone(),
                    field_names_and_values,
                })
            }
            ast_types::StructVariant::TupleStruct(_) => {
                fields.sort_by_key(|(index, _)| *index);
                let args = fields.into_iter().map(|(_, expr)| expr).collect_vec();
                let name = tcx.item_name(adt_expr.adt_def.did()).to_string();
                Expr::FnCall(ast::FnCall {
                    name,
                    args,
                    type_parameter: None,
                    arg_evaluation_order: Default::default(),
                    annot: Typing::KnownType(data_type),
                    qualified_self_type: None,
                })
            }
        }
    }

    fn lower_match_expr(
        &mut self,
        scrutinee: ExprId,
        arms: &[thir::ArmId],
        match_source: rustc_hir::MatchSource,
        ty: Ty<'tcx>,
    ) -> Expr {
        // `x?` is the same as `x.unwrap()` for this compiler
        if matches!(match_source, rustc_hir::MatchSource::TryDesugar(_)) {
            let scrutinee = self.strip(scrutinee);
            let ExprKind::Call { args, .. } = &self.expr(scrutinee).kind else {
                panic!("Unexpected desugaring of `?`");
            };
            let receiver = self.lower_expr(args[0]);
            return self.resolve_method_call("unwrap", vec![receiver]);
        }
        assert!(
            matches!(match_source, rustc_hir::MatchSource::Normal),
            "Unsupported desugared `match`"
        );

        let match_expression = self.lower_expr(scrutinee);
        let _ = ty;
        let arms = arms
            .iter()
            .map(|arm_id| {
                let arm = &self.thir.arms[*arm_id];
                assert!(arm.guard.is_none(), "Match guards are not supported");
                let (match_condition, _bindings) = self.lower_match_pattern(&arm.pattern);
                let body = self.lower_returning_block(arm.body);
                ast::MatchExprArm {
                    match_condition,
                    body,
                }
            })
            .collect_vec();

        Expr::Match(ast::MatchExpr {
            match_expression: Box::new(match_expression),
            arms,
        })
    }

    /// Lower the pattern of a match arm. Declares the arm's bindings.
    fn lower_match_pattern(&mut self, pat: &thir::Pat<'tcx>) -> (ast::MatchCondition, Vec<String>) {
        match &pat.kind {
            PatKind::Wild => (ast::MatchCondition::CatchAll, vec![]),
            // Matching on a reference to an enum dereferences implicitly
            PatKind::Deref { subpattern, .. } | PatKind::DerefPattern { subpattern, .. } => {
                self.lower_match_pattern(subpattern)
            }
            PatKind::Variant {
                adt_def,
                variant_index,
                subpatterns,
                ..
            } => {
                let variant = adt_def.variant(*variant_index);
                let enum_type = self.context.map_ty(pat.ty.peel_refs());
                let DataType::Enum(enum_type) = enum_type else {
                    panic!("Can only match on enums");
                };
                let type_name = if enum_type.is_prelude {
                    None
                } else {
                    Some(enum_type.name.clone())
                };

                let mut data_bindings = vec![];
                let mut wildcards = 0;
                for field_pat in subpatterns.iter() {
                    match &field_pat.pattern.kind {
                        PatKind::Wild => wildcards += 1,
                        PatKind::Binding { .. } => {
                            data_bindings.push(self.pattern_binding(&field_pat.pattern))
                        }
                        other => panic!("Unsupported pattern in match arm: {other:?}"),
                    }
                }
                assert!(
                    wildcards == 0 || data_bindings.is_empty(),
                    "Mixing wildcards and bindings in a match arm is not supported"
                );
                let names = data_bindings
                    .iter()
                    .map(|binding| binding.name.clone())
                    .collect_vec();
                let selector = ast::EnumVariantSelector {
                    type_name,
                    variant_name: variant.name.to_string(),
                    data_bindings,
                };

                (ast::MatchCondition::EnumVariant(selector), names)
            }
            other => panic!("Unsupported pattern in match arm: {other:?}"),
        }
    }
}

/* -------------------------------------------------------------------- */
/* Helpers                                                              */
/* -------------------------------------------------------------------- */

fn memory_location(address: Expr, mem_pointer_declared_type: DataType) -> Expr {
    let resolved_type = DataType::Boxed(Box::new(mem_pointer_declared_type.clone()));
    Expr::MemoryLocation(ast::MemPointerExpression {
        mem_pointer_address: Box::new(address),
        mem_pointer_declared_type,
        resolved_type: Typing::KnownType(resolved_type),
    })
}

/// The type of a place expression's base.
fn base_data_type(identifier: &Identifier) -> DataType {
    use crate::type_checker::GetType;
    identifier.get_type()
}

/// `a <= b` is compiled as `!(a > b)`, and `a >= b` as `!(a < b)`.
fn binop_expr(binop: LoweredBinOp, lhs: Expr, rhs: Expr, result_type: DataType) -> Expr {
    match binop {
        LoweredBinOp::Direct(binop) => Expr::Binop(
            Box::new(lhs),
            binop,
            Box::new(rhs),
            Typing::KnownType(result_type),
        ),
        LoweredBinOp::Negated(binop) => Expr::Unary(
            ast::UnaryOp::Not,
            Box::new(Expr::Binop(
                Box::new(lhs),
                binop,
                Box::new(rhs),
                Typing::KnownType(DataType::Bool),
            )),
            Typing::KnownType(DataType::Bool),
        ),
    }
}

enum LoweredBinOp {
    Direct(ast::BinOp),
    Negated(ast::BinOp),
}

fn mir_binop_to_binop(op: MirBinOp) -> LoweredBinOp {
    use LoweredBinOp::*;
    match op {
        MirBinOp::Add | MirBinOp::AddUnchecked | MirBinOp::AddWithOverflow => {
            Direct(ast::BinOp::Add)
        }
        MirBinOp::Sub | MirBinOp::SubUnchecked | MirBinOp::SubWithOverflow => {
            Direct(ast::BinOp::Sub)
        }
        MirBinOp::Mul | MirBinOp::MulUnchecked | MirBinOp::MulWithOverflow => {
            Direct(ast::BinOp::Mul)
        }
        MirBinOp::Div => Direct(ast::BinOp::Div),
        MirBinOp::Rem => Direct(ast::BinOp::Rem),
        MirBinOp::BitXor => Direct(ast::BinOp::BitXor),
        MirBinOp::BitAnd => Direct(ast::BinOp::BitAnd),
        MirBinOp::BitOr => Direct(ast::BinOp::BitOr),
        MirBinOp::Shl | MirBinOp::ShlUnchecked => Direct(ast::BinOp::Shl),
        MirBinOp::Shr | MirBinOp::ShrUnchecked => Direct(ast::BinOp::Shr),
        MirBinOp::Eq => Direct(ast::BinOp::Eq),
        MirBinOp::Ne => Direct(ast::BinOp::Neq),
        MirBinOp::Lt => Direct(ast::BinOp::Lt),
        MirBinOp::Gt => Direct(ast::BinOp::Gt),
        MirBinOp::Le => Negated(ast::BinOp::Gt),
        MirBinOp::Ge => Negated(ast::BinOp::Lt),
        other => panic!("Unsupported binary operator {other:?}"),
    }
}

fn assign_op_to_binop(op: rustc_middle::mir::AssignOp) -> ast::BinOp {
    use rustc_middle::mir::AssignOp::*;
    match op {
        AddAssign => ast::BinOp::Add,
        SubAssign => ast::BinOp::Sub,
        MulAssign => ast::BinOp::Mul,
        DivAssign => ast::BinOp::Div,
        RemAssign => ast::BinOp::Rem,
        BitXorAssign => ast::BinOp::BitXor,
        BitAndAssign => ast::BinOp::BitAnd,
        BitOrAssign => ast::BinOp::BitOr,
        ShlAssign => ast::BinOp::Shl,
        ShrAssign => ast::BinOp::Shr,
    }
}

/// Whether the span originates from the expansion of the named macro.
fn span_is_from_macro(span: Span, macro_name: &str) -> bool {
    let mut ctxt = span.ctxt();
    while !ctxt.is_root() {
        let expn_data = ctxt.outer_expn_data();
        if let ExpnKind::Macro(MacroKind::Bang, name) = expn_data.kind {
            if name.as_str() == macro_name {
                return true;
            }
        }
        ctxt = expn_data.call_site.ctxt();
    }

    false
}
