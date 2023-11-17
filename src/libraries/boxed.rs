use super::Library;
use crate::subroutine::SubRoutine;
use crate::{ast, ast_types};
use num::One;
use triton_vm::{instruction::LabelledInstruction, triton_asm, triton_instr};

#[derive(Debug)]
pub struct Boxed;

const FUNCTION_NAME_NEW_BOX: &str = "Box::new";
const AS_REF_METHOD_NAME: &str = "as_ref";

impl Library for Boxed {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        if full_name == FUNCTION_NAME_NEW_BOX {
            return Some(full_name.to_owned());
        }

        None
    }

    fn get_method_name(
        &self,
        method_name: &str,
        receiver_type: &crate::ast_types::DataType,
    ) -> Option<String> {
        if method_name == AS_REF_METHOD_NAME {
            if let ast_types::DataType::Boxed(_inner) = receiver_type {
                return Some(method_name.to_owned());
            }
        }

        None
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        receiver_type: &crate::ast_types::DataType,
        _args: &[crate::ast::Expr<super::Annotation>],
        _type_checker_state: &crate::type_checker::CheckState,
    ) -> crate::ast::FnSignature {
        assert!(matches!(receiver_type, ast_types::DataType::Boxed(_inner)));
        assert_eq!(AS_REF_METHOD_NAME, method_name);

        ast::FnSignature {
            name: String::from(AS_REF_METHOD_NAME),
            args: vec![ast_types::AbstractArgument::ValueArgument(
                ast_types::AbstractValueArg {
                    name: "value".to_owned(),
                    data_type: receiver_type.to_owned(),
                    mutable: false,
                },
            )],
            output: receiver_type.to_owned(),
            arg_evaluation_order: Default::default(),
        }
    }

    fn function_name_to_signature(
        &self,
        full_name: &str,
        type_parameter: Option<crate::ast_types::DataType>,
        args: &[crate::ast::Expr<super::Annotation>],
    ) -> crate::ast::FnSignature {
        if full_name == FUNCTION_NAME_NEW_BOX {
            assert!(
                args.len().is_one(),
                "Box::new only takes one argument. Got: {args:?}"
            );
            assert!(
                type_parameter.is_some(),
                "Box::new needs a type parameter due to restricted type inference"
            );
            return new_box_function_signature(&type_parameter.unwrap());
        }

        panic!("AllocBoxed does not known function with name {full_name}")
    }

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &crate::ast_types::DataType,
        _args: &[crate::ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        assert!(matches!(receiver_type, ast_types::DataType::Boxed(_inner)));
        assert_eq!(AS_REF_METHOD_NAME, method_name);

        // This can simply be implemented as the identity function, I think.
        // The only reason we need this funciton is that Rustc demands it.
        triton_asm!()
    }

    fn call_function(
        &self,
        full_name: &str,
        type_parameter: Option<crate::ast_types::DataType>,
        _args: &[crate::ast::Expr<super::Annotation>],
        state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        if full_name == FUNCTION_NAME_NEW_BOX {
            let box_new: SubRoutine = new_box_function_body(&type_parameter.unwrap(), state)
                .try_into()
                .unwrap();
            let box_new_label = box_new.get_label();
            state.add_library_function(box_new);

            return triton_asm!(call { box_new_label });
        }

        panic!("AllocBoxed does not known function with name {full_name}")
    }

    fn get_graft_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn graft_function(
        &self,
        _graft_config: &crate::graft::Graft,
        _fn_name: &str,
        _args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        _type_parameter: Option<crate::ast_types::DataType>,
    ) -> Option<crate::ast::Expr<super::Annotation>> {
        panic!("AllocBoxed cannot graft functions")
    }

    fn graft_method(
        &self,
        _graft_config: &crate::graft::Graft,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<crate::ast::Expr<super::Annotation>> {
        None
    }
}

/// BEFORE: [value] mem_address_start
/// AFTER: mem_address_start
fn move_top_stack_value_to_memory_keep_address(top_value_size: usize) -> Vec<LabelledInstruction> {
    // A stack value of the form `_ val2 val1 val0`, with `val0` being on the top of the stack
    // is stored in memory as: `val0 val1 val2`, where `val0` is stored on the `memory_location`
    // address.
    let mut ret = vec![];
    // stack: _ [value] mem_address_start

    // _ [value] mem_address_start
    for i in 0..top_value_size {
        ret.push(triton_instr!(swap 1));
        // _ mem_address element

        ret.push(triton_instr!(write_mem));
        // _ mem_address

        if i != top_value_size - 1 {
            ret.append(&mut triton_asm!(push 1 add));
            // _ (mem_address + 1)
        }
    }

    // reset memory address to its initial value
    let decrement_value = -(top_value_size as isize - 1);
    ret.append(&mut triton_asm!(push {decrement_value} add));

    ret
}

fn new_box_function_signature(inner_type: &ast_types::DataType) -> ast::FnSignature {
    ast::FnSignature {
        name: format!("box_new_{}", inner_type.label_friendly_name()),
        args: vec![ast_types::AbstractArgument::ValueArgument(
            ast_types::AbstractValueArg {
                name: "x".to_owned(),
                data_type: inner_type.to_owned(),
                mutable: false,
            },
        )],
        output: ast_types::DataType::Boxed(Box::new(inner_type.to_owned())),
        arg_evaluation_order: Default::default(),
    }
}

fn new_box_function_body(
    inner_type: &ast_types::DataType,
    state: &mut crate::tasm_code_generator::CompilerState,
) -> Vec<LabelledInstruction> {
    let dyn_malloc = state.import_snippet(Box::new(tasm_lib::memory::dyn_malloc::DynMalloc));
    let entrypoint = format!("box_new_{}", inner_type.label_friendly_name());
    let inner_type_size = inner_type.stack_size();

    let move_to_memory_code = move_top_stack_value_to_memory_keep_address(inner_type_size);

    triton_asm!(
        {entrypoint}:
            // dynamically allocate enough memory
            push {inner_type_size}
            call {dyn_malloc}

            // _ [x] *memory_address
            {&move_to_memory_code}

            // _ *memory_address
            return
    )
}
