use crate::ast;
use crate::ast_types;
use crate::tasm_code_generator::inner_function_tasm_code::InnerFunctionTasmCode;
use crate::tasm_code_generator::SubRoutine;
use itertools::Itertools;
use tasm_lib::library::Library as SnippetState;
use tasm_lib::memory::dyn_malloc::DynMalloc;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;
use triton_vm::Program;

pub(crate) struct OuterFunctionTasmCode {
    // TODO: Remove these attributes once we have a sane `main` function that uses these fields
    #[allow(dead_code)]
    pub function_data: InnerFunctionTasmCode,
    #[allow(dead_code)]
    pub snippet_state: SnippetState,
    #[allow(dead_code)]
    pub compiled_method_calls: Vec<InnerFunctionTasmCode>,
    #[allow(dead_code)]
    pub outer_function_signature: ast::FnSignature,
}

impl OuterFunctionTasmCode {
    fn get_inputs_function(&self) -> String {
        // fn inputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        //     vec![
        //         (DataType::U64, "lhs".to_string()),
        //         (DataType::U64, "rhs".to_string()),
        //     ]
        // }
    }

    fn get_outputs_function(&self) -> String {
        // TODO: Convert type to `tasm-lib` type here
        let output_description = format!(
            "(DataType::{}, \"result\".to_owned())",
            self.outer_function_signature.output
        );

        // for output in self.outer_function_signature.output
        format!(
            "
            fn outputs(&self) -> Vec<(DataType, String)> {{
                vec![{output_description}]
            }}
            "
        )
    }

    fn get_entrypoint_function(&self) -> String {
        let own_fn_name = &self.function_data.name;
        format!(
            "
                fn entrypoint(&self) -> String {{
                    {own_fn_name}
        }}"
        )
    }

    #[allow(dead_code)]
    pub(crate) fn to_basic_snippet(&self) -> String {
        let inner_body = match self
            .function_data
            .call_depth_zero_code
            .get_function_body_for_inlining()
        {
            Some(inner) => inner,
            None => panic!(
                "Inner function must conform to: <label>: <body> return.\nGot:\n{}",
                self.function_data.call_depth_zero_code
            ),
        };
        let name = &self.function_data.name;

        // `methods` list must be sorted to produce deterministic programs
        let mut compiled_methods_sorted = self.compiled_method_calls.clone();
        compiled_methods_sorted.sort_by_cached_key(|x| x.name.to_owned());
        let methods_call_depth_zero = self
            .compiled_method_calls
            .iter()
            .map(|x| x.call_depth_zero_code.clone())
            .collect_vec();
        let method_subroutines = self
            .compiled_method_calls
            .iter()
            .flat_map(|x| x.sub_routines.clone())
            .collect_vec();
        let subroutines = self
            .function_data
            .sub_routines
            .iter()
            .map(|sr| sr.get_whole_function())
            .concat();

        let entrypoint_function = self.get_entrypoint_function();
        let outputs_function = self.get_outputs_function();
        let inputs_function = self.get_inputs_function();

        todo!()
    }

    #[allow(dead_code)]
    pub(crate) fn compose(&self) -> Vec<LabelledInstruction> {
        let inner_body = match self
            .function_data
            .call_depth_zero_code
            .get_function_body_for_inlining()
        {
            Some(inner) => inner,
            None => panic!(
                "Inner function must conform to: <label>: <body> return.\nGot:\n{}",
                self.function_data.call_depth_zero_code
            ),
        };

        // `methods` list must be sorted to produce deterministic programs
        let mut compiled_methods_sorted = self.compiled_method_calls.clone();
        compiled_methods_sorted.sort_by_cached_key(|x| x.name.to_owned());
        let methods_call_depth_zero = self
            .compiled_method_calls
            .iter()
            .map(|x| x.call_depth_zero_code.clone())
            .collect_vec();
        let method_subroutines = self
            .compiled_method_calls
            .iter()
            .flat_map(|x| x.sub_routines.clone())
            .collect_vec();

        let name = &self.function_data.name;
        let dyn_malloc_init = DynMalloc::get_initialization_code(
            self.snippet_state
                .get_next_free_address()
                .try_into()
                .unwrap(),
        );
        let external_dependencies: Vec<SubRoutine> = self
            .snippet_state
            .all_external_dependencies()
            .into_iter()
            .map(|x| x.try_into().unwrap())
            .collect_vec();
        let external_dependencies = external_dependencies
            .iter()
            .map(|x| x.get_whole_function())
            .concat();
        let subroutines = self
            .function_data
            .sub_routines
            .iter()
            .map(|sr| sr.get_whole_function())
            .concat();

        // Wrap entire execution in a call such that `recurse` can be used on the outermost layer, i.e. in `inner_body`.
        let ret = triton_asm!(
            {&dyn_malloc_init}
            call {name}
            halt
            {name}:
                {&inner_body}
                return
            {&subroutines}
            {&methods_call_depth_zero}
            {&method_subroutines}
            {&external_dependencies}
        );

        // Verify that code parses by wrapping it in a program, panics
        // if assembly is invalid.
        let _program = Program::new(&ret);

        ret
    }
}
