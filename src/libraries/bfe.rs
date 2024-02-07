use itertools::Itertools;
use num::One;
use num::Zero;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::shared_math::traits::Inverse;
use tasm_lib::twenty_first::shared_math::traits::PrimitiveRootOfUnity;

use crate::ast;
use crate::ast::FnSignature;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::graft::Graft;
use crate::subroutine::SubRoutine;

use super::Library;
use super::LibraryFunction;

const BFE_STRUCT_NAME: &str = "BFieldElement";
const FUNCTION_NAME_NEW_BFE: &str = "BFieldElement::new";
const FUNCTION_NAME_ZERO: &str = "BFieldElement::zero";
const FUNCTION_NAME_ONE: &str = "BFieldElement::one";
const FUNCTION_ROOT_FULL_NAME: &str = "BFieldElement::primitive_root_of_unity";
const FUNCTION_ROOT_STRIPPED_NAME: &str = "primitive_root_of_unity";
const FUNCTION_GENERATOR_NAME: &str = "BFieldElement::generator";
const METHOD_NAME_MOD_POW_U32: &str = "mod_pow_u32";
const METHOD_NAME_LIFT: &str = "lift";
const METHOD_NAME_VALUE: &str = "value";
const UNWRAP_METHOD_NAME: &str = "unwrap";
const INVERSE_METHOD_NAME: &str = "inverse";

#[derive(Clone, Debug)]
pub(crate) struct BfeLibrary {
    pub(crate) list_type: ast_types::ListType,
}

impl BfeLibrary {
    fn has_method(method_name: &str) -> bool {
        method_name == METHOD_NAME_LIFT
            || method_name == METHOD_NAME_VALUE
            || method_name == METHOD_NAME_MOD_POW_U32
            || method_name == INVERSE_METHOD_NAME
    }
}

impl Library for BfeLibrary {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        matches!(
            full_name,
            FUNCTION_NAME_NEW_BFE | FUNCTION_ROOT_FULL_NAME | FUNCTION_GENERATOR_NAME
        )
        .then(|| full_name.to_owned())
    }

    fn get_method_name(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
    ) -> Option<String> {
        if matches!(receiver_type, ast_types::DataType::Bfe) && BfeLibrary::has_method(method_name)
        {
            Some(method_name.to_owned())
        } else {
            None
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

            if method_name == INVERSE_METHOD_NAME {
                return bfe_inverse_method_signature();
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
        match fn_name {
            FUNCTION_NAME_NEW_BFE => bfe_new_function().signature,
            FUNCTION_ROOT_FULL_NAME => bfe_root_function_signature(self.list_type),
            FUNCTION_GENERATOR_NAME => bfe_generator_function_signature(),
            _ => panic!("No function name {fn_name} implemented for BFE library."),
        }
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

            if method_name == INVERSE_METHOD_NAME {
                return triton_asm!(invert);
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

        if fn_name == FUNCTION_GENERATOR_NAME {
            let generator = BFieldElement::generator();
            return triton_asm!(push { generator } hint field_generator = stack[0]);
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

    fn graft_method_call(
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

fn bfe_inverse_method_signature() -> ast::FnSignature {
    let bf: BFieldElement = BFieldElement::one();
    bf.inverse();
    ast::FnSignature::value_function_immutable_args(
        "bfe_inverse",
        vec![("x", DataType::Bfe)],
        DataType::Bfe,
    )
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
    let signature = ast::FnSignature::value_function_immutable_args(
        METHOD_NAME_MOD_POW_U32,
        vec![
            ("base", ast_types::DataType::Bfe),
            ("exponent", ast_types::DataType::U32),
        ],
        ast_types::DataType::Bfe,
    );

    LibraryFunction {
        signature,
        body: triton_asm!(swap 1 pow),
    }
}

fn bfe_lift_method() -> LibraryFunction {
    let signature = ast::FnSignature::value_function_immutable_args(
        METHOD_NAME_LIFT,
        vec![("value", ast_types::DataType::Bfe)],
        ast_types::DataType::Xfe,
    );

    LibraryFunction {
        signature,
        body: triton_asm!(push 0 push 0 swap 2),
    }
}

fn bfe_value_method() -> LibraryFunction {
    let signature = ast::FnSignature::value_function_immutable_args(
        METHOD_NAME_VALUE,
        vec![("bfe_value", ast_types::DataType::Bfe)],
        ast_types::DataType::U64,
    );

    LibraryFunction {
        signature,
        body: triton_asm!(split),
    }
}

fn bfe_root_function_signature(list_type: crate::libraries::ListType) -> ast::FnSignature {
    let snippet = tasm_lib::arithmetic::bfe::primitive_root_of_unity::PrimitiveRootOfUnity;
    ast::FnSignature::from_basic_snippet(Box::new(snippet), list_type)
}

fn bfe_generator_function_signature() -> FnSignature {
    ast::FnSignature::value_function_immutable_args(FUNCTION_GENERATOR_NAME, vec![], DataType::Bfe)
}

fn bfe_new_function() -> LibraryFunction {
    let signature = ast::FnSignature::value_function_immutable_args(
        "bfe_new_from_u64",
        vec![("u64_value", ast_types::DataType::U64)],
        ast_types::DataType::Bfe,
    );

    const TWO_POW_32: &str = "4294967296";
    LibraryFunction {
        signature,
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
