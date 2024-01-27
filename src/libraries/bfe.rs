use crate::triton_vm::prelude::*;
use crate::triton_vm::triton_asm;
use crate::triton_vm::twenty_first::shared_math::traits::PrimitiveRootOfUnity;
use itertools::Itertools;
use num::One;
use num::Zero;

use crate::ast;
use crate::ast_types;
use crate::graft::Graft;
use crate::subroutine::SubRoutine;

use super::tasm_lib_snippet_to_fn_signature;
use super::Library;
use super::LibraryFunction;

const BFE_STRUCT_NAME: &str = "BFieldElement";
const FUNCTION_NAME_NEW_BFE: &str = "BFieldElement::new";
const FUNCTION_NAME_ZERO: &str = "BFieldElement::zero";
const FUNCTION_NAME_ONE: &str = "BFieldElement::one";
const FUNCTION_ROOT_FULL_NAME: &str = "BFieldElement::primitive_root_of_unity";
const FUNCTION_ROOT_STRIPPED_NAME: &str = "primitive_root_of_unity";
const METHOD_NAME_MOD_POW_U32: &str = "mod_pow_u32";
const METHOD_NAME_LIFT: &str = "lift";
const METHOD_NAME_VALUE: &str = "value";
const UNWRAP_METHOD_NAME: &str = "unwrap";

#[derive(Clone, Debug)]
pub(crate) struct BfeLibrary {
    pub(crate) list_type: ast_types::ListType,
}

impl Library for BfeLibrary {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        if full_name == FUNCTION_NAME_NEW_BFE || full_name == FUNCTION_ROOT_FULL_NAME {
            return Some(full_name.to_owned());
        }

        None
    }

    fn get_method_name(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
    ) -> Option<String> {
        match receiver_type {
            ast_types::DataType::Bfe => {
                if method_name == METHOD_NAME_LIFT
                    || method_name == METHOD_NAME_VALUE
                    || method_name == METHOD_NAME_MOD_POW_U32
                {
                    Some(method_name.to_owned())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature {
        if matches!(receiver_type, ast_types::DataType::Bfe) {
            if method_name == METHOD_NAME_LIFT {
                return bfe_lift_method().signature;
            }

            if method_name == METHOD_NAME_VALUE {
                return bfe_value_method().signature;
            }

            if method_name == METHOD_NAME_MOD_POW_U32 {
                return bfe_mod_pow_method().signature;
            }
        }

        panic!("Unknown method {method_name} for BFE");
    }

    fn function_name_to_signature(
        &self,
        fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        if fn_name == FUNCTION_NAME_NEW_BFE {
            return bfe_new_function().signature;
        }

        if fn_name == FUNCTION_ROOT_FULL_NAME {
            return bfe_root_function_signature(self.list_type);
        }

        panic!("No function name {fn_name} implemented for BFE library.");
    }

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<LabelledInstruction> {
        if matches!(receiver_type, ast_types::DataType::Bfe) {
            // TODO: Instead of inlining this, we could add the subroutine to the
            // compiler state and the call that subroutine. And then let the inline
            // handle whatever needs to be inlined.
            if method_name == METHOD_NAME_LIFT {
                return bfe_lift_method().body;
            }

            if method_name == METHOD_NAME_VALUE {
                return bfe_value_method().body;
            }

            if method_name == METHOD_NAME_MOD_POW_U32 {
                return bfe_mod_pow_method().body;
            }
        }

        panic!("Unknown method {method_name} for BFE");
    }

    fn call_function(
        &self,
        fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<LabelledInstruction> {
        if fn_name == FUNCTION_NAME_NEW_BFE {
            // Import the function and return the code to call it
            let new_bfe_function = bfe_new_function();
            let new_bfe_function: SubRoutine = new_bfe_function.try_into().unwrap();
            let function_label = new_bfe_function.get_label();
            state.add_library_function(new_bfe_function);

            return triton_asm!(call { function_label });
        }

        if fn_name == FUNCTION_ROOT_FULL_NAME {
            let entrypoint = state.import_snippet(Box::new(
                tasm_lib::arithmetic::bfe::primitive_root_of_unity::PrimitiveRootOfUnity,
            ));
            return triton_asm!(call { entrypoint });
        }

        panic!("No function name {fn_name} implemented for BFE library.");
    }

    fn get_graft_function_name(&self, full_name: &str) -> Option<String> {
        if full_name == FUNCTION_NAME_NEW_BFE
            || full_name == FUNCTION_NAME_ZERO
            || full_name == FUNCTION_NAME_ONE
            || full_name == FUNCTION_ROOT_FULL_NAME
        {
            return Some(full_name.to_owned());
        }

        None
    }

    fn graft_function(
        &self,
        graft_config: &mut Graft,
        full_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        _function_type_parameter: Option<ast_types::DataType>,
    ) -> Option<ast::Expr<super::Annotation>> {
        if full_name == FUNCTION_NAME_ZERO {
            return Some(ast::Expr::Lit(ast::ExprLit::Bfe(BFieldElement::zero())));
        }

        if full_name == FUNCTION_NAME_ONE {
            return Some(ast::Expr::Lit(ast::ExprLit::Bfe(BFieldElement::one())));
        }

        if full_name == FUNCTION_NAME_NEW_BFE {
            return Some(graft_bfe_new(graft_config, args));
        }

        if full_name == FUNCTION_ROOT_FULL_NAME {
            return Some(graft_bfe_primitive_root(graft_config, args));
        }

        None
    }

    fn graft_method(
        &self,
        graft_config: &mut Graft,
        rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        // Handle the `unwrap()` when using `BFieldElement::primitive_root_of_unity(order).unwrap()`
        let last_method_name = rust_method_call.method.to_string();
        if last_method_name != UNWRAP_METHOD_NAME {
            return None;
        }

        match rust_method_call.receiver.as_ref() {
            syn::Expr::Call(fn_call) => {
                if let syn::Expr::Path(expr_path) = *fn_call.func.to_owned() {
                    if expr_path.path.segments[0].ident == BFE_STRUCT_NAME
                        && expr_path.path.segments[1].ident == FUNCTION_ROOT_STRIPPED_NAME
                    {
                        return self.graft_function(
                            graft_config,
                            FUNCTION_ROOT_FULL_NAME,
                            &fn_call.args,
                            None,
                        );
                    }
                    None
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

/// Graft `BFieldElement::primitive_root_of_unity` to allow for compile-time resolution,
/// if possible. Also gets rid of `unwrap` method call if needed.
fn graft_bfe_primitive_root(
    graft_config: &mut Graft,
    args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
) -> ast::Expr<super::Annotation> {
    let args = args
        .iter()
        .map(|x| graft_config.graft_expr(x))
        .collect_vec();

    if args.len() != 1 {
        panic!("BFE must be initialized with only one argument. Got: {args:#?}");
    }

    let init_arg = &args[0];
    match init_arg {
        ast::Expr::Lit(lit) => match lit {
            ast::ExprLit::U64(value) => ast::Expr::Lit(ast::ExprLit::Bfe(
                BFieldElement::primitive_root_of_unity(*value)
                    .expect("Primitive root must be known. Got order: {value}"),
            )),
            ast::ExprLit::GenericNum(value, _) => ast::Expr::Lit(ast::ExprLit::Bfe(
                BFieldElement::primitive_root_of_unity(TryInto::<u64>::try_into(*value).unwrap())
                    .expect("Primitive root must be known. Got order: {value}"),
            )),
            _ => {
                panic!("Cannot initialize BFieldElement with {lit:?} from {init_arg:#?}")
            }
        },
        _ => {
            // non-const declaration of `BFieldElement::primitive_root_of_unity(<expr>)`
            ast::Expr::FnCall(ast::FnCall {
                name: FUNCTION_ROOT_FULL_NAME.to_string(),
                args: vec![init_arg.to_owned()],
                type_parameter: None,
                arg_evaluation_order: Default::default(),
                annot: Default::default(),
            })
        }
    }
}

fn graft_bfe_new(
    graft_config: &mut Graft,
    args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
) -> ast::Expr<super::Annotation> {
    let args = args
        .iter()
        .map(|x| graft_config.graft_expr(x))
        .collect_vec();

    if args.len() != 1 {
        panic!("BFE must be initialized with only one argument. Got: {args:#?}");
    }

    let init_arg = &args[0];
    match init_arg {
        ast::Expr::Lit(lit) => match lit {
            ast::ExprLit::U64(value) => {
                ast::Expr::Lit(ast::ExprLit::Bfe(BFieldElement::new(*value)))
            }
            ast::ExprLit::GenericNum(value, _) => ast::Expr::Lit(ast::ExprLit::Bfe(
                BFieldElement::new(TryInto::<u64>::try_into(*value).unwrap()),
            )),
            _ => {
                panic!("Cannot initialize BFieldElement with {lit:?} from {init_arg:#?}")
            }
        },
        // TODO: To handle more advanced expressions here (like BFieldElement::MAX - 1),
        // we might have to implement constant folding on ast::Expr?
        // Unsure how to handle that.
        ast::Expr::Var(ast::Identifier::String(constant, _)) => {
            if constant == "BFieldElement::MAX" {
                // `const` declaration of `BFieldElement::new(BFieldElement::MAX)`
                ast::Expr::Lit(ast::ExprLit::Bfe(BFieldElement::new(BFieldElement::MAX)))
            } else {
                // non-const declaration of `BFieldElement::new(<expr>)`
                let bfe_new_function = bfe_new_function();
                ast::Expr::FnCall(ast::FnCall {
                    name: "BFieldElement::new".to_string(),
                    args: vec![init_arg.to_owned()],
                    type_parameter: None,
                    arg_evaluation_order: bfe_new_function.signature.arg_evaluation_order,
                    annot: Default::default(),
                })
            }
        }
        _ => {
            // non-const declaration of `BFieldElement::new(<expr>)`
            let bfe_new_function = bfe_new_function();
            ast::Expr::FnCall(ast::FnCall {
                name: "BFieldElement::new".to_string(),
                args: vec![init_arg.to_owned()],
                type_parameter: None,
                arg_evaluation_order: bfe_new_function.signature.arg_evaluation_order,
                annot: Default::default(),
            })
        }
    }
}

fn bfe_mod_pow_method() -> LibraryFunction {
    let fn_signature = ast::FnSignature {
        name: METHOD_NAME_MOD_POW_U32.to_owned(),
        args: vec![
            ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
                name: "base".to_owned(),
                data_type: ast_types::DataType::Bfe,
                mutable: false,
            }),
            ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
                name: "exponent".to_owned(),
                data_type: ast_types::DataType::U32,
                mutable: false,
            }),
        ],
        output: ast_types::DataType::Bfe,
        arg_evaluation_order: Default::default(),
    };

    LibraryFunction {
        signature: fn_signature,
        body: triton_asm!(swap 1 pow),
    }
}

fn bfe_lift_method() -> LibraryFunction {
    let fn_signature = ast::FnSignature {
        name: METHOD_NAME_LIFT.to_owned(),
        args: vec![ast_types::AbstractArgument::ValueArgument(
            ast_types::AbstractValueArg {
                name: "value".to_owned(),
                data_type: ast_types::DataType::Bfe,
                mutable: false,
            },
        )],
        output: ast_types::DataType::Xfe,
        arg_evaluation_order: Default::default(),
    };

    LibraryFunction {
        signature: fn_signature,
        body: triton_asm!(push 0 push 0 swap 2),
    }
}

fn bfe_value_method() -> LibraryFunction {
    let fn_signature = ast::FnSignature {
        name: METHOD_NAME_VALUE.to_owned(),
        args: vec![ast_types::AbstractArgument::ValueArgument(
            ast_types::AbstractValueArg {
                name: "bfe_value".to_owned(),
                data_type: ast_types::DataType::Bfe,
                mutable: false,
            },
        )],
        output: ast_types::DataType::U64,
        arg_evaluation_order: Default::default(),
    };

    LibraryFunction {
        signature: fn_signature,
        body: triton_asm!(split),
    }
}

fn bfe_root_function_signature(list_type: crate::libraries::ListType) -> ast::FnSignature {
    let snippet = tasm_lib::arithmetic::bfe::primitive_root_of_unity::PrimitiveRootOfUnity;
    tasm_lib_snippet_to_fn_signature(list_type, Box::new(snippet))
}

fn bfe_new_function() -> LibraryFunction {
    let fn_signature = ast::FnSignature {
        name: "bfe_new_from_u64".to_owned(),
        args: vec![ast_types::AbstractArgument::ValueArgument(
            ast_types::AbstractValueArg {
                name: "u64_value".to_owned(),
                data_type: ast_types::DataType::U64,
                mutable: false,
            },
        )],
        output: ast_types::DataType::Bfe,
        arg_evaluation_order: Default::default(),
    };

    const TWO_POW_32: &str = "4294967296";
    LibraryFunction {
        signature: fn_signature,
        body: triton_asm!(
            // _ hi lo
            swap 1 // _ lo hi
            push {TWO_POW_32}
            mul // _ lo (hi * 2^32)
            add
            // _ (lo + (hi * 2^32))
        ),
    }
}
