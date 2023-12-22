use std::collections::HashMap;
use std::str::FromStr;

use chrono::Local;
use itertools::Itertools;
use tasm_lib::library::Library as SnippetState;
use triton_vm::instruction;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::op_stack::NumberOfWords;
use triton_vm::triton_asm;
use triton_vm::Program;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::ast;
use crate::ast_types;
use crate::tasm_code_generator::inner_function_tasm_code::InnerFunctionTasmCode;
use crate::tasm_code_generator::SubRoutine;

use super::ValueIdentifier;

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
    #[allow(dead_code)]
    pub library_snippets: HashMap<String, SubRoutine>,
    #[allow(dead_code)]
    pub static_allocations: HashMap<ValueIdentifier, (BFieldElement, ast_types::DataType)>,
}

fn replace_hardcoded_snippet_names_and_spill_addresses(
    code: Vec<LabelledInstruction>,
    imported_snippet_names: &[String],
    static_allocations: &HashMap<ValueIdentifier, (BFieldElement, ast_types::DataType)>,
) -> String {
    use instruction::AnInstruction::*;
    use instruction::LabelledInstruction::Instruction;

    /// _Warning_: only works if the data type of the spilled element is of size 5 or less.
    fn instruction_triplet_looks_like_spill_writing(
        instr_0: &LabelledInstruction,
        instr_1: &LabelledInstruction,
        instr_2: &LabelledInstruction,
        static_allocations: &HashMap<ValueIdentifier, (BFieldElement, ast_types::DataType)>,
    ) -> Option<(ValueIdentifier, BFieldElement)> {
        let Instruction(Push(presumed_address)) = instr_0 else {
            return None;
        };
        let Instruction(WriteMem(number_of_written_words)) = instr_1 else {
            return None;
        };
        let Instruction(Pop(NumberOfWords::N1)) = instr_2 else {
            return None;
        };

        let Some((value_identifier, (static_address, data_type))) = static_allocations
            .iter()
            .find(|(_val_id, (mem_spill_position, _))| mem_spill_position == presumed_address)
        else {
            return None;
        };

        let Ok(data_size) = NumberOfWords::try_from(data_type.stack_size()) else {
            return None;
        };
        if number_of_written_words != &data_size {
            return None;
        }

        Some((value_identifier.to_owned(), *static_address))
    }

    /// _Warning_: only works if the data type of the spilled element is of size 5 or less.
    fn instruction_triplet_looks_like_spill_reading(
        instr_0: &LabelledInstruction,
        instr_1: &LabelledInstruction,
        instr_2: &LabelledInstruction,
        static_allocations: &HashMap<ValueIdentifier, (BFieldElement, ast_types::DataType)>,
    ) -> Option<(ValueIdentifier, BFieldElement)> {
        let Instruction(Push(presumed_address_of_last_word)) = instr_0 else {
            return None;
        };
        let Instruction(ReadMem(number_of_read_words)) = instr_1 else {
            return None;
        };
        let Instruction(Pop(NumberOfWords::N1)) = instr_2 else {
            return None;
        };

        let Some((value_identifier, (static_address, data_type))) =
            static_allocations
                .iter()
                .find(|(_val_id, (mem_spill_position, data_type))| {
                    let mem_pointer_offset = BFieldElement::from(data_type.stack_size() as u32 - 1);
                    *mem_spill_position + mem_pointer_offset == *presumed_address_of_last_word
                })
        else {
            return None;
        };

        let Ok(data_size) = NumberOfWords::try_from(data_type.stack_size()) else {
            return None;
        };
        if number_of_read_words != &data_size {
            return None;
        }

        Some((value_identifier.to_owned(), *static_address))
    }

    let all_possible_snippet_calls: Vec<LabelledInstruction> = imported_snippet_names
        .iter()
        .map(|name| triton_asm!(call { name }).first().unwrap().to_owned())
        .collect_vec();
    let mut ret = vec![];
    for (instr_0, instr_1, instr_2) in code.iter().tuple_windows() {
        let maybe_spill_write = instruction_triplet_looks_like_spill_writing(
            instr_0,
            instr_1,
            instr_2,
            static_allocations,
        );
        let maybe_spill_read = instruction_triplet_looks_like_spill_reading(
            instr_0,
            instr_1,
            instr_2,
            static_allocations,
        );
        if let Some((spill_value, _memory_address)) = maybe_spill_write {
            // Replace `push a` with `"push {a}"`
            ret.push(format!("push {{{spill_value}}}"));
        } else if let Some((spill_value, _memory_address)) = maybe_spill_read {
            // Replace `push a` with `"push {a}"`
            ret.push(format!("push {{{spill_value}}}"));
        } else if all_possible_snippet_calls.contains(instr_0) {
            // Replace `call tasm_foo_bar` with `"call {tasm_foo_bar}"`
            let snippet_name = if let Instruction(Call(label)) = instr_0 {
                label.to_string()
            } else {
                unreachable!()
            };
            ret.push(format!("call {{{snippet_name}}}"));
        } else {
            // Do nothing
            ret.push(instr_0.to_string());
        }
    }

    // Add the last two instructions that were not covered by the above
    // window iteration
    if code.len() > 1 {
        ret.push(code[code.len() - 2].to_string());
    }
    if !code.is_empty() {
        ret.push(code[code.len() - 1].to_string());
    }

    ret.join("\n")
}

impl OuterFunctionTasmCode {
    fn basic_snippet_inputs_function(&self) -> String {
        let mut types_and_names = vec![];
        for arg in self.outer_function_signature.args.iter() {
            if let ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
                name,
                data_type,
                mutable: _,
            }) = arg
            {
                let tasm_lib_type: tasm_lib::data_type::DataType =
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

    fn basic_snippet_outputs_function(&self) -> String {
        let output_type: tasm_lib::data_type::DataType = self
            .outer_function_signature
            .output
            .to_owned()
            .try_into()
            .unwrap();

        // TODO: If there is no return type, consider leaving this function body as `vec![]`
        let output_description = format!("({}, \"result\".to_owned())", output_type.variant_name());

        format!(
            "
            fn outputs(&self) -> Vec<(DataType, String)> {{
                vec![{output_description}]
            }}
            "
        )
    }

    fn basic_snippet_entrypoint_function(&self) -> String {
        let own_fn_name = &self.function_data.name;
        format!(
            "
                fn entrypoint(&self) -> String {{
                    \"{own_fn_name}\".to_owned()
        }}"
        )
    }

    fn basic_snippet_code_function(&self) -> String {
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
        let imported_snippet_names = self.snippet_state.get_all_snippet_names();

        let methods_call_depth_zero = self.get_methods_entrypoint_code();
        let method_subroutines = self.get_methods_subroutines_code();

        let subroutines = self
            .function_data
            .sub_routines
            .iter()
            .map(|sr| sr.get_whole_function())
            .concat();

        let subroutines_as_string = replace_hardcoded_snippet_names_and_spill_addresses(
            subroutines,
            &imported_snippet_names,
            &self.static_allocations,
        );
        let code_as_string = replace_hardcoded_snippet_names_and_spill_addresses(
            inner_body,
            &imported_snippet_names,
            &self.static_allocations,
        );
        let import_snippet_statements = self.get_all_import_snippet_statements();
        let memory_spill_address_declarations = self.get_all_memory_spill_address_declarations();

        format!(
            "
            fn code(&self, library: &mut Library) -> Vec<triton_vm::instruction::LabelledInstruction> {{
                let entrypoint = self.entrypoint();
                {import_snippet_statements}
                {memory_spill_address_declarations}
                triton_asm!(
                    {{entrypoint}}:

                    {code_as_string}

                    return

                    // Subroutines:
                    {subroutines_as_string}

                    // Methods, entrypoints:
                    {methods_call_depth_zero}

                    // Method subroutines
                    {method_subroutines}
                )
            }}"
        )
    }

    #[allow(dead_code)]
    pub(crate) fn generate_basic_snippet_implementation(&self) -> String {
        let entrypoint_function = self.basic_snippet_entrypoint_function();
        let outputs_function = self.basic_snippet_outputs_function();
        let inputs_function = self.basic_snippet_inputs_function();
        let code_function = self.basic_snippet_code_function();
        let name = &self.function_data.name;
        let snippet_struct_name = inflections::case::to_pascal_case(name);
        let date = Local::now();
        let date = date.to_string();

        format!(
            "use crate::snippet::BasicSnippet;
use crate::snippet::DataType;
use crate::Library;
use triton_vm::triton_asm;


pub struct {snippet_struct_name};

// This BasicSnippet implementation was autogenerate by the `tasm-lang` compiler
// on {date}
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
    /// Function for generating a list of instructions from a compile state
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

        // Add all library functions, sorted such that produced code is deterministic
        let mut library_snippets = self.library_snippets.clone().into_iter().collect_vec();
        library_snippets.sort_unstable_by_key(|(name, _code)| name.to_owned());
        let library_snippets: Vec<LabelledInstruction> = library_snippets
            .into_iter()
            .flat_map(|(_name, code)| code.get_whole_function())
            .collect_vec();

        let name = &self.function_data.name;

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
            call {name}
            halt
            {name}:
                {&inner_body}
                return
            {&subroutines}
            {&methods_call_depth_zero}
            {&method_subroutines}
            {&library_snippets}
            {&external_dependencies}
        );

        // Verify that code parses by wrapping it in a program, panics
        // if assembly is invalid.
        let _program = Program::new(&ret);

        ret
    }

    fn get_methods_entrypoint_code(&self) -> String {
        let imported_snippet_names = self.snippet_state.get_all_snippet_names();
        let methods_call_depth_zero = self
            .compiled_method_calls
            .iter()
            .flat_map(|x| x.call_depth_zero_code.get_whole_function())
            .collect_vec();
        replace_hardcoded_snippet_names_and_spill_addresses(
            methods_call_depth_zero,
            &imported_snippet_names,
            &self.static_allocations,
        )
    }

    fn get_methods_subroutines_code(&self) -> String {
        let imported_snippet_names = self.snippet_state.get_all_snippet_names();
        let method_subroutines = self
            .compiled_method_calls
            .iter()
            .flat_map(|x| x.sub_routines.clone())
            .flat_map(|x| x.get_whole_function())
            .collect_vec();
        replace_hardcoded_snippet_names_and_spill_addresses(
            method_subroutines,
            &imported_snippet_names,
            &self.static_allocations,
        )
    }

    fn get_all_memory_spill_address_declarations(&self) -> String {
        let mut declarations = vec![];
        for (name, (_address, datatype)) in self.static_allocations.iter() {
            let size = datatype.stack_size();
            declarations.push(format!("let {name} = library.kmalloc({size});"));
        }

        declarations.join("\n")
    }

    fn get_all_import_snippet_statements(&self) -> String {
        let mut import_statements = vec![];
        for import in self.snippet_state.get_all_snippet_names().into_iter() {
            let split_at_type = import.split("___").collect_vec();
            let type_parameter = split_at_type.get(1);
            let import = split_at_type[0];
            let type_parameter =
                type_parameter.map(|x| tasm_lib::data_type::DataType::from_str(x).unwrap());
            let snippet_struct_name = import.split('_').last().unwrap();
            let snippet_struct_name = inflections::case::to_pascal_case(snippet_struct_name);
            let module_full_import_path = import.replace('_', "::");
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
        import_statements.iter().join("\n")
    }
}
