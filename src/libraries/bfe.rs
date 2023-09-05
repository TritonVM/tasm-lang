use itertools::Itertools;
use num::{One, Zero};
use triton_vm::triton_asm;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{ast, ast_types, graft::Graft, tasm_code_generator::subroutine::SubRoutine};

use super::{Library, LibraryFunction};

const FUNCTION_NAME_NEW_BFE: &str = "BFieldElement::new";
const FUNCTION_NAME_ZERO: &str = "BFieldElement::zero";
const FUNCTION_NAME_ONE: &str = "BFieldElement::one";

#[derive(Clone, Debug)]
pub struct BfeLibrary;

impl Library for BfeLibrary {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        if full_name == FUNCTION_NAME_NEW_BFE {
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
            ast_types::DataType::BFE => {
                if method_name == "lift" || method_name == "value" {
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
        if matches!(receiver_type, ast_types::DataType::BFE) {
            if method_name == "lift" {
                return bfe_lift_method().signature;
            }

            if method_name == "value" {
                return bfe_value_method().signature;
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

        panic!("No function name {fn_name} implemented for BFE library.");
    }

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        if matches!(receiver_type, ast_types::DataType::BFE) {
            // TODO: Instead of inlining this, we could add the subroutine to the
            // compiler state and the call that subroutine. And then let the inline
            // handle whatever needs to be inlined.
            if method_name == "lift" {
                return bfe_lift_method().body;
            }

            if method_name == "value" {
                return bfe_value_method().body;
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
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        if fn_name == FUNCTION_NAME_NEW_BFE {
            // Import the function and return the code to call it
            let new_bfe_function = bfe_new_function();
            let new_bfe_function: SubRoutine = new_bfe_function.try_into().unwrap();
            let function_label = new_bfe_function.get_label();
            state.add_library_function(new_bfe_function);

            return triton_asm!(call { function_label });
        }

        panic!("No function name {fn_name} implemented for BFE library.");
    }

    fn get_graft_function_name(&self, full_name: &str) -> Option<String> {
        if full_name == FUNCTION_NAME_NEW_BFE
            || full_name == FUNCTION_NAME_ZERO
            || full_name == FUNCTION_NAME_ONE
        {
            return Some(full_name.to_owned());
        }

        None
    }

    fn graft_function(
        &self,
        graft_config: &Graft,
        full_name: &str,
        args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
    ) -> Option<ast::Expr<super::Annotation>> {
        fn handle_bfe_new(
            graft_config: &Graft,
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
                        ast::Expr::Lit(ast::ExprLit::BFE(BFieldElement::new(*value)))
                    }
                    ast::ExprLit::GenericNum(value, _) => ast::Expr::Lit(ast::ExprLit::BFE(
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
                        ast::Expr::Lit(ast::ExprLit::BFE(BFieldElement::new(BFieldElement::MAX)))
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

        if full_name == FUNCTION_NAME_ZERO {
            return Some(ast::Expr::Lit(ast::ExprLit::BFE(BFieldElement::zero())));
        }

        if full_name == FUNCTION_NAME_ONE {
            return Some(ast::Expr::Lit(ast::ExprLit::BFE(BFieldElement::one())));
        }

        if full_name == FUNCTION_NAME_NEW_BFE {
            return Some(handle_bfe_new(graft_config, args));
        }

        None
    }

    fn graft_method(
        &self,
        _graft_config: &Graft,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        None
    }
}

fn bfe_lift_method() -> LibraryFunction {
    let fn_signature = ast::FnSignature {
        name: "lift".to_owned(),
        args: vec![ast_types::AbstractArgument::ValueArgument(
            ast_types::AbstractValueArg {
                name: "value".to_owned(),
                data_type: ast_types::DataType::BFE,
                mutable: false,
            },
        )],
        output: ast_types::DataType::XFE,
        arg_evaluation_order: Default::default(),
    };

    LibraryFunction {
        signature: fn_signature,
        body: triton_asm!(push 0 push 0 swap 2),
    }
}

fn bfe_value_method() -> LibraryFunction {
    let fn_signature = ast::FnSignature {
        name: "value".to_owned(),
        args: vec![ast_types::AbstractArgument::ValueArgument(
            ast_types::AbstractValueArg {
                name: "bfe_value".to_owned(),
                data_type: ast_types::DataType::BFE,
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
        output: ast_types::DataType::BFE,
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
