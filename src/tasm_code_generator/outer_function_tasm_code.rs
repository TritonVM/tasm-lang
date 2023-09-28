use std::str::FromStr;

use crate::ast;
use crate::ast_types;
use crate::tasm_code_generator::inner_function_tasm_code::InnerFunctionTasmCode;
use crate::tasm_code_generator::SubRoutine;
use itertools::Itertools;
use tasm_lib::library::Library as SnippetState;
use tasm_lib::memory::dyn_malloc::DynMalloc;
use triton_vm::instruction;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;
use triton_vm::triton_instr;
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
        let mut types_and_names = vec![];
        for arg in self.outer_function_signature.args.iter() {
            if let ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
                name,
                data_type,
                mutable: _,
            }) = arg
            {
                let tasm_lib_type: tasm_lib::snippet::DataType =
                    data_type.to_owned().try_into().unwrap();
                types_and_names.push((tasm_lib_type, name.clone()));
            } else {
                panic!("Cannot create snippets from code where outermost function takes a function argument");
            }
        }

        let type_tuples = types_and_names
            .into_iter()
            .map(|(tasm_lib_datatype, name)| {
                format!(
                    "({}, \"{name}\".to_owned())",
                    tasm_lib_datatype.variant_name()
                )
            })
            .join(",\n");

        format!(
            "fn inputs(&self) -> Vec<(crate::snippet::DataType, String)> {{
            vec![
                {type_tuples}
            ]
        }}"
        )
    }

    fn get_outputs_function(&self) -> String {
        let output_type: tasm_lib::snippet::DataType = self
            .outer_function_signature
            .output
            .to_owned()
            .try_into()
            .unwrap();

        // TODO: If there is no return type, conside leaving this function body as `vec![]`
        let output_description = format!("({}, \"result\".to_owned())", output_type.variant_name());

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
                    \"{own_fn_name}\".to_owned()
        }}"
        )
    }

    fn get_code_function(&self) -> String {
        fn replace_hardcoded_snippet_names(
            code: Vec<LabelledInstruction>,
            imported_snippet_names: &[String],
        ) -> String {
            let all_possible_snippet_calls: Vec<LabelledInstruction> = imported_snippet_names
                .iter()
                .map(|name| triton_asm!(call { name }).first().unwrap().to_owned())
                .collect_vec();
            let mut ret = vec![];
            for instruction in code.into_iter() {
                // match all_possible_snippet_calls.into_iter().find(predicate)
                if all_possible_snippet_calls.contains(&instruction) {
                    let snippet_name = if let LabelledInstruction::Instruction(
                        instruction::AnInstruction::Call(label),
                    ) = instruction
                    {
                        label.to_string()
                    } else {
                        unreachable!()
                    };
                    ret.push(format!("call {{{snippet_name}}}"));
                } else {
                    ret.push(instruction.to_string());
                }
            }

            ret.join("\n")
        }

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
        let imported_snippet_names = self.snippet_state.get_all_snippet_names();
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

        let subroutines_as_string =
            replace_hardcoded_snippet_names(subroutines, &imported_snippet_names);
        let code_as_string = replace_hardcoded_snippet_names(inner_body, &imported_snippet_names);
        let mut import_statements = vec![];
        for import in self.snippet_state.get_all_snippet_names().into_iter() {
            let split_at_type = import.split("___").collect_vec();
            let type_parameter = split_at_type.get(1);
            let import = split_at_type[0];
            println!("type_parameter: {type_parameter:?}");
            let type_parameter =
                type_parameter.map(|x| tasm_lib::snippet::DataType::from_str(x).unwrap());
            let snippet_struct_name = import.split("_").last().unwrap();
            let snippet_struct_name = inflections::case::to_pascal_case(snippet_struct_name);
            let module_full_import_path = import.replace("_", "::");
            let module_full_import_path = module_full_import_path.replace("tasm::", "crate::");
            let import_snippet_statement = match type_parameter {
                Some(param) => {
                    let label_name_of_type = param.label_friendly_name();
                    let variant_name = param.variant_name();
                    let module_name_with_type =
                        format!("{module_full_import_path}::{snippet_struct_name}({variant_name})");
                    format!("let {import}___{label_name_of_type} = library.import(Box::new({module_name_with_type}));")
                }
                None => {
                    let module_name = format!("{module_full_import_path}::{snippet_struct_name}");
                    format!("let {import} = library.import(Box::new({module_name}));")
                }
            };
            import_statements.push(import_snippet_statement);
        }
        let import_snippets = import_statements.iter().join("\n");
        format!(
            "
            fn code(&self, library: &mut Library) -> Vec<triton_vm::instruction::LabelledInstruction> {{
                let entrypoint = self.entrypoint();
                {import_snippets}
                triton_asm!(
                    {{entrypoint}}:

                    {code_as_string}

                    return

                    // Subroutines:
                    {subroutines_as_string}

                    // Methods, entrypoints:
                    // todo

                    // Method subroutines
                    // todo
                )
            }}"
        )
    }

    #[allow(dead_code)]
    pub(crate) fn to_basic_snippet(&self) -> String {
        let entrypoint_function = self.get_entrypoint_function();
        let outputs_function = self.get_outputs_function();
        let inputs_function = self.get_inputs_function();
        let code_function = self.get_code_function();
        let name = &self.function_data.name;
        let snippet_struct_name = inflections::case::to_pascal_case(name);

        format!(
            "use crate::snippet::BasicSnippet;
use crate::snippet::DataType;
use crate::Library;
use triton_vm::triton_asm;


pub struct {snippet_struct_name};

impl BasicSnippet for {snippet_struct_name} {{
    {entrypoint_function}

    {outputs_function}

    {inputs_function}

    {code_function}
}}
"
        )
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
