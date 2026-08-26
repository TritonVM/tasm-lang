use num::One;
use tasm_lib::triton_vm::prelude::*;

use super::Library;
use super::LibraryFunction;
use crate::ast;
use crate::ast::FnSignature;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::composite_types::CompositeTypes;
use crate::subroutine::SubRoutine;

const FUNCTION_NAME_NEW_BFE: &str = "BFieldElement::new";
const FUNCTION_ROOT_FULL_NAME: &str = "BFieldElement::primitive_root_of_unity";
const FUNCTION_GENERATOR_NAME: &str = "BFieldElement::generator";
const METHOD_NAME_MOD_POW_U32: &str = "mod_pow_u32";
const METHOD_NAME_LIFT: &str = "lift";
const METHOD_NAME_VALUE: &str = "value";
const INVERSE_METHOD_NAME: &str = "inverse";

#[derive(Clone, Debug)]
pub(crate) struct BfeLibrary;

impl BfeLibrary {
    fn has_method(method_name: &str) -> bool {
        method_name == METHOD_NAME_LIFT
            || method_name == METHOD_NAME_VALUE
            || method_name == METHOD_NAME_MOD_POW_U32
            || method_name == INVERSE_METHOD_NAME
    }
}

impl Library for BfeLibrary {
    fn handle_function_call(
        &self,
        full_name: &str,
        _qualified_self_type: &Option<DataType>,
    ) -> bool {
        matches!(
            full_name,
            FUNCTION_NAME_NEW_BFE | FUNCTION_ROOT_FULL_NAME | FUNCTION_GENERATOR_NAME
        )
    }

    fn handle_method_call(&self, method_name: &str, receiver_type: &ast_types::DataType) -> bool {
        matches!(receiver_type, ast_types::DataType::Bfe) && BfeLibrary::has_method(method_name)
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
        _qualified_self_type: &Option<DataType>,
        _composite_types: &mut CompositeTypes,
    ) -> ast::FnSignature {
        match fn_name {
            FUNCTION_NAME_NEW_BFE => bfe_new_function().signature,
            FUNCTION_ROOT_FULL_NAME => bfe_root_function_signature(),
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
        _qualified_self_type: &Option<DataType>,
    ) -> Vec<LabelledInstruction> {
        if fn_name == FUNCTION_NAME_NEW_BFE {
            // Import the function and return the code to call it
            let new_bfe_function = bfe_new_function();
            let new_bfe_function: SubRoutine = new_bfe_function.try_into().unwrap();
            let function_label = new_bfe_function.get_label();
            state.add_subroutine(new_bfe_function);

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
}

fn bfe_inverse_method_signature() -> ast::FnSignature {
    let bf: BFieldElement = BFieldElement::one();
    let _ = bf.inverse();
    ast::FnSignature::value_function_immutable_args(
        "bfe_inverse",
        vec![("x", DataType::Bfe)],
        DataType::Bfe,
    )
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

fn bfe_root_function_signature() -> ast::FnSignature {
    let snippet = tasm_lib::arithmetic::bfe::primitive_root_of_unity::PrimitiveRootOfUnity;
    ast::FnSignature::from_basic_snippet(Box::new(snippet))
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
