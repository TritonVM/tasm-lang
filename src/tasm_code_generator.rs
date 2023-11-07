mod function_state;
mod inner_function_tasm_code;
mod outer_function_tasm_code;
mod stack;

use itertools::{Either, Itertools};
use num::{One, Zero};
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use tasm_lib::library::Library as SnippetState;
use tasm_lib::snippet::BasicSnippet;
use tasm_lib::{arithmetic, hashing};
use triton_vm::instruction::LabelledInstruction;
use triton_vm::op_stack::OpStackElement;
use triton_vm::{triton_asm, triton_instr};
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use self::function_state::{FunctionState, VarAddr};
use self::inner_function_tasm_code::InnerFunctionTasmCode;
use self::outer_function_tasm_code::OuterFunctionTasmCode;
use self::stack::VStack;
use crate::ast_types::{self, CustomTypeOil, StructVariant};
use crate::libraries::{self};
use crate::subroutine::SubRoutine;
use crate::type_checker::GetType;
use crate::{ast, type_checker};

#[derive(Clone, Debug)]
pub enum ValueLocation {
    OpStack(usize),
    StaticMemoryAddress(u32),
    DynamicMemoryAddress(Vec<LabelledInstruction>),
}

impl Display for ValueLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let as_string = match self {
            ValueLocation::OpStack(n) => format!("opstack{n}"),
            ValueLocation::StaticMemoryAddress(p) => format!("mempointer({p})"),
            ValueLocation::DynamicMemoryAddress(code) => code.iter().join("\n"),
        };
        write!(f, "{as_string}")
    }
}

impl ValueLocation {
    fn is_accessible(&self, data_type: &ast_types::DataType) -> bool {
        match self {
            ValueLocation::StaticMemoryAddress(_) => true,
            ValueLocation::DynamicMemoryAddress(_) => true,
            ValueLocation::OpStack(n) => {
                let bottom_position = n + data_type.stack_size() - 1;
                bottom_position < SIZE_OF_ACCESSIBLE_STACK
            }
        }
    }
}

/// State that is preserved across the compilation of functions
#[derive(Clone, Debug, Default)]
pub struct GlobalCodeGeneratorState {
    counter: usize,
    snippet_state: SnippetState,
    static_allocations: HashMap<ValueIdentifier, (usize, ast_types::DataType)>,
    compiled_methods_and_afs: HashMap<String, InnerFunctionTasmCode>,
    library_snippets: HashMap<String, SubRoutine>,
}

impl GlobalCodeGeneratorState {
    fn statically_allocate(
        &mut self,
        value_identifier: &ValueIdentifier,
        data_type: &ast_types::DataType,
    ) -> usize {
        let new_address = self.snippet_state.kmalloc(data_type.stack_size());
        self.static_allocations.insert(
            value_identifier.to_owned(),
            (new_address, data_type.to_owned()),
        );
        new_address
    }
}

// TODO: Maybe this needs a new lifetime specifier, `'b`?
#[derive(Debug)]
pub struct CompilerState<'a> {
    /// The part of the compiler state that applies across function calls to locally defined
    global_compiler_state: GlobalCodeGeneratorState,

    /// The part of the compiler state that only applies within a function or a method
    function_state: FunctionState,

    libraries: &'a [Box<dyn libraries::Library>],

    custom_types: &'a HashMap<String, ast_types::CustomTypeOil>,

    declared_methods: &'a [ast::Method<type_checker::Typing>],

    associated_functions: &'a HashMap<String, HashMap<String, ast::Fn<type_checker::Typing>>>,
}

impl<'a> CompilerState<'a> {
    /// Import a dependency in an idempotent manner, ensuring it's only ever imported once
    pub(crate) fn add_library_function(&mut self, subroutine: SubRoutine) {
        // TODO: Can't we include this in a nicer way by e.g. unwrapping inner
        // subroutines?
        self.global_compiler_state
            .library_snippets
            .insert(subroutine.get_label(), subroutine);
    }

    /// Construct a new compiler state with no known values that must be spilled.
    fn new(
        global_compiler_state: GlobalCodeGeneratorState,
        libraries: &'a [Box<dyn libraries::Library>],
        declared_methods: &'a [ast::Method<type_checker::Typing>],
        associated_functions: &'a HashMap<String, HashMap<String, ast::Fn<type_checker::Typing>>>,
        custom_types: &'a HashMap<String, ast_types::CustomTypeOil>,
    ) -> Self {
        Self {
            global_compiler_state,
            function_state: FunctionState::default(),
            libraries,
            declared_methods,
            associated_functions,
            custom_types,
        }
    }

    /// Construct a new compiler state with a defined set of values that should be stored in memory,
    /// rather than on the stack, i.e. "spilled".
    fn with_known_spills(
        global_compiler_state: GlobalCodeGeneratorState,
        required_spills: HashSet<ValueIdentifier>,
        libraries: &'a [Box<dyn libraries::Library>],
        declared_methods: &'a [ast::Method<type_checker::Typing>],
        associated_functions: &'a HashMap<String, HashMap<String, ast::Fn<type_checker::Typing>>>,
        custom_types: &'a HashMap<String, ast_types::CustomTypeOil>,
    ) -> Self {
        Self {
            global_compiler_state,
            function_state: FunctionState {
                vstack: Default::default(),
                var_addr: VarAddr::default(),
                spill_required: required_spills,
                subroutines: Vec::default(),
            },
            libraries,
            declared_methods,
            associated_functions,
            custom_types,
        }
    }

    /// At the beginning of a function call, the caller will have placed all arguments
    /// on top of the stack. This must be reflected in the vstack.
    /// Returns a list of values that must be spilled to memory such that the code
    /// generator can start the function with the spilling of these values.
    fn add_input_arguments_to_vstack_and_return_spilled_fn_args(
        &mut self,
        input_arguments: &[ast_types::AbstractArgument],
    ) -> Vec<ValueIdentifier> {
        const FN_ARG_NAME_PREFIX: &str = "fn_arg";

        let mut fn_arg_spilling = vec![];
        for input_arg in input_arguments {
            match input_arg {
                ast_types::AbstractArgument::FunctionArgument(_) => todo!(),
                ast_types::AbstractArgument::ValueArgument(abstract_input) => {
                    let (fn_arg_addr, spill) =
                        self.new_value_identifier(FN_ARG_NAME_PREFIX, &abstract_input.data_type);
                    self.function_state
                        .var_addr
                        .insert(abstract_input.name.clone(), fn_arg_addr.clone());

                    if let Some(_spill_addr) = spill {
                        fn_arg_spilling.push(fn_arg_addr);
                    }
                }
            }
        }

        fn_arg_spilling
    }

    /// Return the set of values that the compiler has determined must be spilled to memory.
    fn get_required_spills(&self) -> HashSet<ValueIdentifier> {
        self.function_state.spill_required.to_owned()
    }

    /// Return information to show where a value can be found. Options are:
    /// 1. On the stack at depth `n`
    /// 2. In memory at memory address `p`.
    /// 3. In memory at an address that must be resolved at runtime with code `code`.
    fn locate_identifier(
        &mut self,
        identifier: &ast::Identifier<type_checker::Typing>,
    ) -> ValueLocation {
        fn get_lhs_address_code(
            state: &mut CompilerState,
            lhs_location: &ValueLocation,
            field_or_element_type: &ast_types::DataType,
            identifier: &ast::Identifier<type_checker::Typing>,
        ) -> Vec<LabelledInstruction> {
            match lhs_location {
                ValueLocation::OpStack(depth) => {
                    if !lhs_location.is_accessible(field_or_element_type) {
                        let binding_name = identifier.binding_name();
                        let value_ident_of_binding = state
                            .function_state
                            .var_addr
                            .get(&binding_name)
                            .unwrap_or_else(|| {
                                panic!(
                                    "Could not locate value identifier for binding {binding_name}"
                                )
                            })
                            .to_owned();
                        state.mark_as_spilled(&value_ident_of_binding);
                        triton_asm!(push 0 assert)
                    } else {
                        // Type of `l` in `l[<expr>]` is known to be list. So we know stack size = 1
                        triton_asm!(dup { depth })
                    }
                }
                ValueLocation::StaticMemoryAddress(pointer) => {
                    // Type of `l` in `l[<expr>]` is known to be list. So we know stack size = 1
                    // Read the list pointer from memory, then clear the stack, leaving only
                    // the list pointer on the stack.
                    triton_asm!(push {pointer} read_mem swap 1 pop)
                }
                ValueLocation::DynamicMemoryAddress(code) => code.to_owned(),
            }
        }

        match identifier {
            ast::Identifier::String(_, _) => {
                let var_name = identifier.binding_name();
                let var_addr = self
                    .function_state
                    .var_addr
                    .get(&var_name)
                    .unwrap_or_else(|| panic!("Could not locate {var_name} on stack"));
                let (position, _ident_data_type, spilled) =
                    self.function_state.vstack.find_stack_value(var_addr);
                match spilled {
                    Some(mem_addr) => ValueLocation::StaticMemoryAddress(mem_addr),
                    None => ValueLocation::OpStack(position),
                }
            }
            ast::Identifier::ListIndex(ident, index_expr, element_type) => {
                let element_type = element_type.get_type();
                let lhs_location = self.locate_identifier(ident);
                let ident_addr_code =
                    get_lhs_address_code(self, &lhs_location, &element_type, ident);
                // stack: _ *list

                let lhs_type = ident.get_type();
                let list_type = if let ast_types::DataType::List(_, lity) = lhs_type {
                    lity
                } else {
                    panic!("Expected type was list. Got {lhs_type}.")
                };

                self.new_value_identifier("list_expression", &ident.get_type());
                let (_index_expr, index_code) = compile_expr(index_expr, "index_on_assign", self);
                // stack: _ *list index

                let list_metadata_size = list_type.metadata_size();
                let element_address = match element_type.bfield_codec_length() {
                    Some(static_element_size) => {
                        let relative_address = triton_asm!(
                            {&index_code}
                            push {static_element_size}
                            mul
                            push {list_metadata_size}
                            add
                        );
                        triton_asm!(
                            {&ident_addr_code}
                            {&relative_address}
                            add
                        )
                    }
                    // We need a while loop here, and we need a unique identifier for that
                    None => {
                        // Notice that the following subroutine is always the same, so
                        // we only need to import it once, no matter if we index into
                        // different lists with dynamically sized elements in the same
                        // program.
                        let loop_label = "tasm_langs_dynamic_list_element_finder".to_owned();

                        let loop_subroutine = triton_asm!(
                            {&loop_label}:
                                // _ *vec<T>[n]_size index
                                dup 0
                                push 0
                                eq
                                skiz
                                    return
                                // _ *vec<T>[n]_size index

                                swap 1
                                read_mem
                                // _ index *vec<T>[n]_size vec<T>[n]_size

                                push 1 add add
                                // _ index *vec<T>[n+1]_size

                                swap 1
                                // _ *vec<T>[n+1]_size index

                                push -1
                                add
                                recurse

                        );
                        let loop_subroutine: SubRoutine = loop_subroutine.try_into().unwrap();

                        self.add_library_function(loop_subroutine);

                        triton_asm!(
                            {&ident_addr_code}
                            push {list_metadata_size}
                            add
                            {&index_code}
                            call {loop_label}
                            // _ *vec<T>[index]_size 0

                            pop
                            push 1 add
                            // _ *vec<T>[index]
                        )
                    }
                };

                self.function_state.vstack.pop();
                self.function_state.vstack.pop();

                ValueLocation::DynamicMemoryAddress(element_address)
            }
            ast::Identifier::Field(lhs, field_id, known_type) => {
                let lhs_location = self.locate_identifier(lhs);
                let lhs_type = lhs.get_type();

                let get_ident_code =
                    get_lhs_address_code(self, &lhs_location, &known_type.get_type(), lhs);
                // stack: _ struct/tuple

                fn handle_tuple(
                    tuple: ast_types::Tuple,
                    field_id: &ast_types::FieldId,
                    lhs_location: ValueLocation,
                ) -> ValueLocation {
                    let tuple_index: usize = field_id.try_into().unwrap();
                    let tuple_depth: usize = tuple
                        .into_iter()
                        .enumerate()
                        .filter(|(i, _x)| *i > tuple_index)
                        .map(|(_i, x)| x.stack_size())
                        .sum::<usize>();
                    match lhs_location {
                        ValueLocation::OpStack(n) => ValueLocation::OpStack(n + tuple_depth),
                        ValueLocation::StaticMemoryAddress(p) => {
                            ValueLocation::StaticMemoryAddress(p + tuple_depth as u32)
                        }
                        ValueLocation::DynamicMemoryAddress(code) => {
                            let new_code = [code, triton_asm!(push {tuple_depth} add)].concat();
                            ValueLocation::DynamicMemoryAddress(new_code)
                        }
                    }
                }

                /// Return the position of a field for a struct that lives on the stack,
                /// or has been spilled. This does not work for boxed structs.
                fn handle_named_fields_struct(
                    struct_type: ast_types::NamedFieldsStruct,
                    field_id: &ast_types::FieldId,
                    lhs_location: ValueLocation,
                ) -> ValueLocation {
                    let needle_name: String = field_id.to_string();
                    let mut field_depth: usize = 0;
                    for (haystack_name, haystack_type) in struct_type.fields.iter() {
                        if &needle_name == haystack_name {
                            break;
                        } else {
                            field_depth += haystack_type.stack_size();
                        }
                    }

                    match lhs_location {
                        ValueLocation::OpStack(n) => ValueLocation::OpStack(n + field_depth),
                        ValueLocation::StaticMemoryAddress(p) => {
                            ValueLocation::StaticMemoryAddress(p + field_depth as u32)
                        }
                        ValueLocation::DynamicMemoryAddress(_code) => unreachable!(
                            "named struct was expected to live on stack, or be spilled"
                        ),
                    }
                }

                // limited field support for now
                match lhs.get_type() {
                    ast_types::DataType::Boxed(inner_type) => match *inner_type {
                        ast_types::DataType::Struct(inner_struct) => {
                            let get_field_pointer_from_struct_pointer =
                                inner_struct.get_field_accessor_code_for_reference(field_id);
                            ValueLocation::DynamicMemoryAddress(triton_asm!(
                                {&get_ident_code}
                                {&get_field_pointer_from_struct_pointer}
                            ))
                        }
                        ast_types::DataType::Tuple(tuple) => {
                            let tuple_index: usize = field_id.try_into().unwrap();
                            let tuple_depth: usize = tuple
                                .into_iter()
                                .enumerate()
                                .filter(|(i, _x)| *i > tuple_index)
                                .map(|(_i, x)| x.stack_size())
                                .sum::<usize>();

                            ValueLocation::DynamicMemoryAddress(triton_asm!(
                                {&get_ident_code}
                                push {tuple_depth}
                                add
                            ))
                        }
                        _ => todo!("lhs_type: {lhs_type}"),
                    },
                    ast_types::DataType::Tuple(tuple) => {
                        handle_tuple(tuple, field_id, lhs_location)
                    }
                    ast_types::DataType::Struct(struct_type) => match struct_type.variant {
                        StructVariant::TupleStruct(tuple) => {
                            handle_tuple(tuple, field_id, lhs_location)
                        }
                        StructVariant::NamedFields(named_fields_struct) => {
                            handle_named_fields_struct(named_fields_struct, field_id, lhs_location)
                        }
                    },
                    ast_types::DataType::Reference(inner_type) => match *inner_type {
                        ast_types::DataType::Struct(struct_type) => match struct_type.variant {
                            StructVariant::TupleStruct(tuple) => {
                                handle_tuple(tuple, field_id, lhs_location)
                            }
                            StructVariant::NamedFields(_) => todo!(),
                        },
                        _ => todo!("lhs_type: {lhs_type}"),
                    },
                    _ => todo!("lhs_type: {lhs_type}"),
                }
            }
        }
    }

    /// Return code for a function, without including its external dependencies and without
    /// initialization of the dynamic memory allocator, as these pieces of code should be
    /// included only once, by the outer function.
    fn compose_code_for_inner_function(
        &self,
        fn_name: &str,
        call_depth_zero_code: Vec<LabelledInstruction>,
    ) -> InnerFunctionTasmCode {
        let with_own_label = triton_asm!(
            {fn_name}:
                {&call_depth_zero_code}
                return);
        InnerFunctionTasmCode {
            name: fn_name.to_owned(),
            call_depth_zero_code: with_own_label.try_into().unwrap(),
            sub_routines: self.function_state.subroutines.clone(),
        }
    }

    /// Return code including those compilation steps that should only be performed once per compilation:
    /// - initialization of dynamic memory allocator
    /// - importing of all external dependencies
    fn compose_code_for_outer_function(
        &self,
        compiled: InnerFunctionTasmCode,
        outer_function_signature: &ast::FnSignature,
    ) -> OuterFunctionTasmCode {
        // After the spilling has been done, and after all dependencies have been loaded, the `library` field
        // in the `state` contains info about how much static memory has been allocated and which
        // code snippets that have been importer.
        let final_snippet_state = self.global_compiler_state.snippet_state.clone();

        let mut methods_sorted = self
            .global_compiler_state
            .compiled_methods_and_afs
            .clone()
            .into_iter()
            .collect_vec();
        methods_sorted.sort_by_key(|x| x.0.clone());
        let compiled_method_calls = methods_sorted
            .into_iter()
            .map(|(_name, code)| code)
            .collect_vec();

        // Compile all associated functions
        // self.associated_functions.values().map(|x| x.values().map(|y| compi))

        OuterFunctionTasmCode {
            function_data: compiled,
            compiled_method_calls,
            snippet_state: final_snippet_state,
            outer_function_signature: outer_function_signature.to_owned(),
            library_snippets: self.global_compiler_state.library_snippets.clone(),
            static_allocations: self.global_compiler_state.static_allocations.clone(),
        }
    }
}

pub const SIZE_OF_ACCESSIBLE_STACK: usize = triton_vm::op_stack::NUM_OP_STACK_REGISTERS;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValueIdentifier {
    pub name: String,
}

impl From<String> for ValueIdentifier {
    fn from(value: String) -> Self {
        Self { name: value }
    }
}

impl std::fmt::Display for ValueIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'a> CompilerState<'a> {
    /// Change the compiler's view of a value, without changing anything on the stack. E.g.:
    /// `(XFE, BField)` -> `XFE, BFE`. `new_types` must be provided such that its last
    /// element will be closest to the top of the stack. New value IDs are returned in the same
    /// order.
    pub(crate) fn split_value<const N: usize>(
        &mut self,
        seek_addr: &ValueIdentifier,
        new_types: [ast_types::DataType; N],
    ) -> [ValueIdentifier; N] {
        let (_, old_type, spilled) = self.function_state.vstack.find_stack_value(seek_addr);
        assert_eq!(
            old_type.stack_size(),
            new_types.iter().map(|x| x.stack_size()).sum::<usize>(),
            "splitting values must keep size unchanged. New types were:\n{}\n",
            new_types.iter().join(", ")
        );

        let index_removed = self.function_state.vstack.remove_by_id(seek_addr);
        let mut size_acc: usize = 0;
        let mut new_labels = vec![];
        for (i, new_type) in new_types.into_iter().enumerate() {
            let new_label: ValueIdentifier =
                self.unique_label("split_value", Some(&new_type)).into();
            new_labels.push(new_label.clone());
            self.function_state.vstack.insert_at(
                index_removed + i,
                (
                    new_label,
                    (new_type.clone(), spilled.map(|x| x + size_acc as u32)),
                ),
            );
            size_acc += new_type.stack_size();
        }

        new_labels.try_into().unwrap()
    }

    fn get_binding_name(&self, value_identifier: &ValueIdentifier) -> String {
        match self
            .function_state
            .var_addr
            .iter()
            .find(|x| x.1 == value_identifier)
        {
            Some(binding) => binding.0.to_owned(),
            None => "Unbound value".to_string(),
        }
    }

    /// Return a new, guaranteed unique label that can be used anywhere in the code
    pub fn unique_label(
        &mut self,
        prefix: &str,
        data_type: Option<&ast_types::DataType>,
    ) -> String {
        let name = match data_type {
            Some(ty) => format!(
                "_{}_{}_{}",
                prefix,
                ty.label_friendly_name(),
                self.global_compiler_state.counter
            ),
            None => format!("_{}_{}", prefix, self.global_compiler_state.counter),
        };
        self.global_compiler_state.counter += 1;

        name
    }

    /// Get a new, guaranteed unique, identifier for a value. Returns an address to
    /// spill the value to, iff spilling of this value is required. A previous run
    /// of the compiler will have determined whether spilling is required.
    pub fn new_value_identifier(
        &mut self,
        prefix: &str,
        data_type: &ast_types::DataType,
    ) -> (ValueIdentifier, Option<u32>) {
        let name = self.unique_label(prefix, Some(data_type));
        let address = ValueIdentifier { name };

        // Get a statically known memory address if value needs to be spilled to
        // memory.
        let spilled = if self.function_state.spill_required.contains(&address) {
            let spill_address = self
                .global_compiler_state
                .statically_allocate(&address, data_type)
                .try_into()
                .unwrap();
            eprintln!("Warning: spill required of {address}. Spilling to address: {spill_address}");
            Some(spill_address)
        } else {
            None
        };

        self.function_state
            .vstack
            .push((address.clone(), (data_type.clone(), spilled)));

        (address, spilled)
    }

    pub fn import_snippet(&mut self, snippet: Box<dyn BasicSnippet>) -> String {
        self.global_compiler_state.snippet_state.import(snippet)
    }

    pub fn mark_as_spilled(&mut self, value_identifier: &ValueIdentifier) {
        eprintln!(
            "Warning: Marking {value_identifier} as spilled. Binding: {}",
            self.get_binding_name(value_identifier)
        );
        self.function_state
            .spill_required
            .insert(value_identifier.to_owned());
    }

    fn verify_same_ordering_of_bindings(
        &self,
        previous_stack: &VStack,
        previous_var_addr: &VarAddr,
    ) {
        // make a list of tuples (binding name, stack position, spilled_value) as the stack looked at the beginning of the loop
        let bindings_start: HashMap<String, (usize, Option<u32>)> = previous_var_addr
            .iter()
            .map(|(k, v)| {
                let binding_name = k.to_string();
                let (previous_stack_depth, _data_type, spilled) =
                    previous_stack.find_stack_value(v);
                (binding_name, (previous_stack_depth, spilled))
            })
            .collect();

        // make a list of tuples (binding name, stack position, spilled_value) as the stack looks now
        let bindings_end: HashMap<String, (usize, Option<u32>)> = self
            .function_state
            .var_addr
            .iter()
            .map(|(k, v)| {
                let binding_name = k.to_string();
                let (previous_stack_depth, _data_type, spilled) =
                    self.function_state.vstack.find_stack_value(v);
                (binding_name, (previous_stack_depth, spilled))
            })
            .collect();

        // Sanity check that all bindings at the start of the code block still exists.
        // I think that should be the case.
        for (var_name_start, _) in bindings_start.iter() {
            assert!(
                bindings_end.contains_key(var_name_start),
                "Bindings at end of block must contain all at start of block"
            );
        }

        // Generate a flip/flop list of tuple values (var_name, old_position, new_position)
        // If assignments overwrite existing stack space, that means all values declared in
        // the block will be on top of the stack, and that the flip_flop list will always
        // be empty.
        let mut flip_flop_list = vec![];
        for (start_binding, (start_stack_pos, _spilled)) in bindings_start.iter() {
            let (end_stack_pos, _end_data_type) = bindings_end[start_binding];
            if end_stack_pos != *start_stack_pos {
                flip_flop_list.push((start_binding, end_stack_pos, start_stack_pos));
            }
        }

        if !flip_flop_list.is_empty() {
            panic!("non-empty flip-flop list is not yet supported!\n\n\nflip-flop list: {flip_flop_list:#?}");
        }

        // TODO: If we want to, we could change this function to return the code to recover from this
        // re-ordering of value bindings. We can also just accept that we must generate code that does
        // not change the stack-ordering of bindings when a value is re-assigned.
    }

    /// Restore the vstack to a previous state, representing the state the stack had
    /// at the beginning of a codeblock. Also returns the code to achieve this.
    fn restore_stack_code(
        &mut self,
        previous_stack: &VStack,
        previous_var_addr: &VarAddr,
    ) -> Vec<LabelledInstruction> {
        // Clear stack, vstack, and var_addr of locally declared values for those that are on top of the stack
        let mut code = vec![];
        loop {
            let (addr, (dt, spilled)) = self.function_state.vstack.peek().unwrap().to_owned();
            let binding_name = self
                .function_state
                .var_addr
                .iter()
                .find(|(_var_name, ident)| **ident == addr)
                //.unwrap_or_else(|| panic!("Cannot handle stack cleanup of unbound values"))
                .map(|x| x.0.clone());
            // .0
            // .clone();
            if binding_name.is_some()
                && previous_var_addr.contains_key(&binding_name.clone().unwrap())
                || binding_name.is_none()
                    && previous_stack
                        .inner
                        .contains(&(addr, (dt.clone(), spilled)))
            {
                break;
            } else {
                code.append(&mut triton_asm![pop; dt.stack_size()]);
                self.function_state.vstack.pop();
                if let Some(binding) = binding_name {
                    let removed = self.function_state.var_addr.remove(&binding);
                    assert!(removed.is_some());
                }
            }
        }

        // Verify that ordering of previously declared bindings did not change from the execution of
        // this code block.
        self.verify_same_ordering_of_bindings(previous_stack, previous_var_addr);

        code
    }

    /// Return code that clears the stack above a certain height but leaves the value
    /// that's on the top of the stack when this function is called.
    /// Also updates vstack to reflect this.
    fn clear_all_but_top_stack_value_above_height(
        &mut self,
        height: usize,
    ) -> Vec<LabelledInstruction> {
        /// Code generation for removing elements from the bottom of the stack while preserving
        /// the order of the values above.
        fn clear_bottom_of_stack(
            top_value_size: usize,
            words_to_remove: usize,
        ) -> Vec<LabelledInstruction> {
            match (top_value_size, words_to_remove) {
                (3, 2) => triton_asm!(swap 2 swap 4 pop swap 2 pop),
                (4, 2) => triton_asm!(swap 1 swap 3 swap 5 pop swap 1 swap 3 pop),
                (6, 2) => triton_asm!(swap 2 swap 4 swap 6 pop swap 2 swap 4 swap 6 pop),
                (4, 3) => triton_asm!(swap 3 swap 6 pop swap 3 pop swap 3 pop),
                (5, 3) => triton_asm!(swap 4 swap 7 pop swap 2 swap 5 pop swap 3 pop swap 1),
                (6, 4) => triton_asm!(swap 4 swap 8 pop swap 4 swap 8 pop swap 4 pop swap 4 pop),
                (8, 4) => triton_asm!(swap 4 swap 8 pop swap 4 swap 8 pop swap 4 swap 8 pop swap 4 swap 8 pop),
                (n, 1) => {
                    let mut swaps = vec![];
                    for i in 1..=n {
                        swaps.append(&mut triton_asm!(swap {i}));
                    }

                    triton_asm!({&swaps} pop)
                }
                _ => panic!("Unsupported. Please cover more special cases. Got: {top_value_size}, {words_to_remove}"),
            }
        }

        let height_of_affected_stack: usize =
            self.function_state.vstack.get_stack_height() - height;

        let top_element = self
            .function_state
            .vstack
            .pop()
            .expect("Cannot remove all but top element from an empty stack");
        let (_top_elem_ident, (top_element_type, top_element_spilled)) = top_element.clone();

        // Verify that top element is not spilled -- as we haven't covered that case yet
        // TODO: Fix this case. That should be quite easy, I think. Not sure this can ever
        // happen though... Please write a test demonstrating this case, before fixing it.
        assert!(
            top_element_spilled.is_none(),
            "Cannot handle spilled value as top element in stack clearing yet"
        );

        let top_value_size = top_element_type.stack_size();

        // Generate code to move value to the bottom of the requested stack range
        let words_to_remove = height_of_affected_stack - top_value_size;
        let code = if words_to_remove != 0
            && top_value_size <= words_to_remove
            && words_to_remove < SIZE_OF_ACCESSIBLE_STACK
        {
            // If we can handle the stack clearing by just swapping the top value
            // lower onto the stack and removing everything above, we do that.
            let mut code = vec![triton_asm!(swap {words_to_remove} pop); top_value_size].concat();

            // Generate code to remove any remaining values from the requested stack range
            let remaining_pops = if words_to_remove > top_value_size {
                words_to_remove - top_value_size
            } else {
                0
            };

            code.append(&mut triton_asm![pop; remaining_pops]);

            code
        } else if words_to_remove >= SIZE_OF_ACCESSIBLE_STACK {
            // In this case, we have to clear more elements from the stack than we can
            // access with dup15/swap15. Our solution is to store the return value in
            // memory, clear the stack, and read it back from memory.
            let (value_identifier_for_spill_value, _spill) =
                self.new_value_identifier("memory_return_spilling", &top_element_type);
            assert!(_spill.is_none(), "Cannot spill while spilling");
            let memory_location: u32 = self
                .global_compiler_state
                .statically_allocate(&value_identifier_for_spill_value, &top_element_type)
                .try_into()
                .unwrap();
            let mut code = copy_top_stack_value_to_memory(memory_location, top_value_size);
            code.append(&mut triton_asm![pop; height_of_affected_stack]);
            code.append(&mut load_from_memory(Some(memory_location), top_value_size));

            code
        } else if words_to_remove != 0 {
            // Here, the number of words under the top element is less than the size of
            // the top element. We special-case this as the order on the stack
            // must be preserved, which is non-trivial.
            clear_bottom_of_stack(top_value_size, words_to_remove)
        } else {
            // The case where nothing has to be done since there is nothing below the
            // top value that needs to be removed
            vec![]
        };

        // Clean up vstack
        while self.function_state.vstack.get_stack_height() > height {
            self.function_state.vstack.pop();
        }

        // Sanity check that input argument was aligned with a value
        assert_eq!(
            height,
            self.function_state.vstack.get_stack_height(),
            "Cannot clear stack to position that is not alligned with a value"
        );

        self.function_state.vstack.push(top_element);

        code
    }

    /// Helper function for debugging
    #[allow(dead_code)]
    fn show_vstack_values(&self) {
        println!("vstack: ");
        for (addr, (data_type, spilled)) in self.function_state.vstack.inner.iter().rev() {
            let var_names = self
                .function_state
                .var_addr
                .iter()
                .filter(|(_k, v)| **v == *addr)
                .map(|(k, _v)| k)
                .collect_vec();
            let var_name = match var_names.len() {
                0 => "unnamed",
                _ => var_names[0],
            };
            match spilled {
                Some(spilled_addr) => {
                    println!(
                        "{var_name: <10} <{data_type: <10}> -- ({addr: <30}); spilled to: {spilled_addr}"
                    )
                }
                None => println!("{var_name: <10} <{data_type: <10}> -- ({addr: <30})"),
            }
        }
        println!();
    }
}

/// Compile a function, returning the code for the function body. Inherits the `global_compiler_state`
/// from the caller but starts with an empty virtual stack and an empty variable mapping.
fn compile_function_inner(
    function: &ast::Fn<type_checker::Typing>,
    global_compiler_state: &mut GlobalCodeGeneratorState,
    libraries: &[Box<dyn libraries::Library>],
    declared_methods: &[ast::Method<type_checker::Typing>],
    associated_functions: &HashMap<String, HashMap<String, ast::Fn<type_checker::Typing>>>,
    custom_types: &HashMap<String, ast_types::CustomTypeOil>,
) -> InnerFunctionTasmCode {
    let fn_name = &function.signature.name;
    let _fn_stack_output_sig = format!("{}", function.signature.output);

    // Run the compilation 1st time to learn which values need to be spilled to memory
    let spills = {
        let mut temporary_fn_state = CompilerState::new(
            global_compiler_state.to_owned(),
            libraries,
            declared_methods,
            associated_functions,
            custom_types,
        );
        let fn_arg_spilling = temporary_fn_state
            .add_input_arguments_to_vstack_and_return_spilled_fn_args(&function.signature.args);
        assert!(
            fn_arg_spilling.is_empty(),
            "Cannot memory-spill function arguments first time the code generator runs"
        );

        // Compiling the function body allows us to learn which values need to be spilled to memory
        let _fn_body_code = function
            .body
            .iter()
            .map(|stmt| compile_stmt(stmt, &mut temporary_fn_state))
            .concat();
        temporary_fn_state.get_required_spills()
    };

    // Run the compilation again now that we know which values to spill
    println!("\n\n\nRunning compiler again\n\n\n");
    let mut state = CompilerState::with_known_spills(
        global_compiler_state.to_owned(),
        spills,
        libraries,
        declared_methods,
        associated_functions,
        custom_types,
    );

    // Add function arguments to the compiler's view of the stack.
    let fn_arg_spilling =
        state.add_input_arguments_to_vstack_and_return_spilled_fn_args(&function.signature.args);

    // Create code to spill required function arguments to memory
    let mut fn_body_code = state
        .function_state
        .vstack
        .get_code_to_spill_to_memory(&fn_arg_spilling);

    // Append the code for the function body
    fn_body_code.append(
        &mut function
            .body
            .iter()
            .map(|stmt| compile_stmt(stmt, &mut state))
            .concat(),
    );

    // Update global compiler state with imported snippets, and label counter
    *global_compiler_state = state.global_compiler_state.clone();

    state.compose_code_for_inner_function(fn_name, fn_body_code)
}

// TODO: Remove this attribute once we have a sane `main` function that uses this step
#[allow(dead_code)]
pub(crate) fn compile_function(
    function: &ast::Fn<type_checker::Typing>,
    libraries: &[Box<dyn libraries::Library>],
    declared_methods: Vec<ast::Method<type_checker::Typing>>,
    associated_functions: &HashMap<String, HashMap<String, ast::Fn<type_checker::Typing>>>,
    custom_types: &HashMap<String, ast_types::CustomTypeOil>,
) -> OuterFunctionTasmCode {
    let mut state = CompilerState::new(
        GlobalCodeGeneratorState::default(),
        libraries,
        &declared_methods,
        associated_functions,
        custom_types,
    );
    let compiled_function = compile_function_inner(
        function,
        &mut state.global_compiler_state,
        libraries,
        &declared_methods,
        associated_functions,
        custom_types,
    );

    state.compose_code_for_outer_function(compiled_function, &function.signature)
}

/// Produce the code and handle the `vstack` for a statement. `env_fn_signature` is the
/// function signature in which the statement is enclosed.
fn compile_stmt(
    stmt: &ast::Stmt<type_checker::Typing>,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    /// Local function to handle a block statement. Returns code to execute
    /// all statements, and to clean up the stack after the exit of a block.
    fn compile_block_stmt(
        block: &ast::BlockStmt<type_checker::Typing>,
        state: &mut CompilerState,
    ) -> Vec<LabelledInstruction> {
        let vstack_init = state.function_state.vstack.clone();
        let var_addr_init = state.function_state.var_addr.clone();
        let block_body_code = block
            .stmts
            .iter()
            .map(|stmt| compile_stmt(stmt, state))
            .collect_vec()
            .concat();

        let restore_stack_code = state.restore_stack_code(&vstack_init, &var_addr_init);

        [block_body_code, restore_stack_code].concat()
    }

    match stmt {
        ast::Stmt::Let(ast::LetStmt {
            var_name,
            data_type: _,
            expr,
            mutable: _,
        }) => {
            let (expr_addr, expr_code) = compile_expr(expr, var_name, state);
            state
                .function_state
                .var_addr
                .insert(var_name.clone(), expr_addr);
            expr_code
        }

        ast::Stmt::Assign(ast::AssignStmt { identifier, expr }) => {
            // When overwriting a value, we ignore the value identifier of the new expression as
            // it's simply popped from the stack and the old value identifier is used.
            let (_expr_addr, expr_code) = compile_expr(expr, "assign", state);
            let location = state.locate_identifier(identifier);

            let ident_type = identifier.get_type();
            let overwrite_code = if !location.is_accessible(&ident_type) {
                // Compiler must run again. Mark binding as spilled and produce unusable code
                let binding_name = identifier.binding_name();
                let value_ident_of_binding = state
                    .function_state
                    .var_addr
                    .get(&binding_name)
                    .unwrap_or_else(|| {
                        panic!("Could not locate value identifier for binding {binding_name}")
                    })
                    .to_owned();
                state.mark_as_spilled(&value_ident_of_binding);
                triton_asm!(push 0 assert)
            } else {
                match location {
                    ValueLocation::OpStack(top_value_position) => {
                        let swap_pop_instructions = format!("swap {top_value_position} pop\n")
                            .repeat(ident_type.stack_size());
                        triton_asm!({ swap_pop_instructions })
                    }
                    ValueLocation::StaticMemoryAddress(ram_pointer) => {
                        move_top_stack_value_to_memory(Some(ram_pointer), ident_type.stack_size())
                    }
                    ValueLocation::DynamicMemoryAddress(code) => triton_asm!(
                        // Goal: Move 2nd to top stack value to memory where memory is indicated
                        // by the address on top of the stack.
                        {&code}
                        {&move_top_stack_value_to_memory(None, ident_type.stack_size())}
                    ),
                }
            };

            // remove new value from stack, as it replaces old value and doesn't take up room on the stack
            // or creates new values in memory.
            state.function_state.vstack.pop();

            triton_asm!(
                {&expr_code}
                {&overwrite_code}
            )
        }

        // 'return;': Clean stack
        ast::Stmt::Return(None) => {
            let mut code = vec![];
            while let Some((_addr, (data_type, _spilled))) = state.function_state.vstack.pop() {
                code.push(triton_asm![pop; data_type.stack_size()])
            }

            code.concat()
        }

        ast::Stmt::Return(Some(ret_expr)) => {
            // special-case on returning variable, without unnecessary dup-instructions
            let expr_code = if let ast::Expr::Var(ast::Identifier::String(var_name, _known_type)) =
                ret_expr
            {
                // Case: Returning a bound variable

                // Remove everything above returned value
                let needle = state
                    .function_state
                    .var_addr
                    .get(var_name)
                    .expect("Returned value must exist in value/addr map");
                let mut code = vec![];

                // If the returned value is a spilled value, we pop everything and
                // load the value from memory.
                // If the returned value is not spilled, we pop until the sought
                // value is on top of the stack and then call a helper function
                // to remove everything below it.
                loop {
                    // This loop pops values from the stack until the value we want to return.
                    // Then the below if-expression takes over when the value is found.
                    let (haystack, (dt, spilled)) = state.function_state.vstack.peek().unwrap();
                    if *haystack == *needle {
                        match spilled {
                            Some(spill_addr) => {
                                // Value is spilled, so we load it from memory and clear that stack.
                                code.append(&mut triton_asm![pop; state.function_state.vstack.get_stack_height()]);
                                code.append(&mut load_from_memory(
                                    Some(*spill_addr),
                                    dt.stack_size(),
                                ))
                            }

                            None => {
                                // Value is not spilled and is on top of the stack now. Remove everything
                                // below it.
                                code.append(
                                    &mut state.clear_all_but_top_stack_value_above_height(0),
                                );
                            }
                        }
                        break;
                    }

                    code.append(&mut triton_asm![pop; dt.stack_size()]);
                    state.function_state.vstack.pop();
                }

                code
            } else {
                // Case: Returning an expression that must be computed
                let code = compile_expr(ret_expr, "ret_expr", state).1;

                // Remove all but top value from stack
                let cleanup_code = state.clear_all_but_top_stack_value_above_height(0);

                [code, cleanup_code].concat()
            };

            // We don't need to clear `var_addr` here since we are either done with the code generation
            // (if this is an outer function return) or we're returning from a local function that does
            // not share `vstack` or `var_addr` with the caller.

            expr_code
        }

        ast::Stmt::FnCall(fn_call) => compile_fn_call(fn_call, state),

        ast::Stmt::MethodCall(method_call) => compile_method_call(method_call, state),

        ast::Stmt::While(ast::WhileStmt { condition, block }) => {
            // The code generated here is a subroutine that contains the while loop code
            // and then just a call to this subroutine.
            let (cond_addr, cond_evaluation_code) =
                compile_expr(condition, "while_condition", state);

            let while_loop_subroutine_name = format!("{cond_addr}_while_loop");

            // condition evaluation is not visible to loop body, so pop this from vstack
            state.function_state.vstack.pop();

            // Compiling the while body as a block means we also get the code to
            // cleanup the stack from local declarations.
            let loop_body_code = compile_block_stmt(block, state);
            let while_loop_code = triton_asm!(
                    {while_loop_subroutine_name}:
                        {&cond_evaluation_code}
                        push 0 eq skiz return
                        {&loop_body_code}
                        recurse
            );

            state
                .function_state
                .subroutines
                .push(while_loop_code.try_into().unwrap());

            triton_asm!(call {
                while_loop_subroutine_name
            })
        }

        ast::Stmt::If(ast::IfStmt {
            condition,
            then_branch,
            else_branch,
        }) => {
            let (cond_addr, cond_code) = compile_expr(condition, "if_condition", state);

            // Pop condition result from vstack as it's not on the stack inside the branches
            let _condition_addr = state.function_state.vstack.pop();

            let then_body_code = compile_block_stmt(then_branch, state);

            let else_body_code = compile_block_stmt(else_branch, state);

            let then_subroutine_name = format!("{cond_addr}_then");
            let else_subroutine_name = format!("{cond_addr}_else");
            let if_code = triton_asm!(
                {&cond_code}
                push 1
                swap 1
                skiz
                call {then_subroutine_name}
                skiz
                call {else_subroutine_name}
            );

            let then_code = triton_asm!(
                {then_subroutine_name}:
                    pop
                    {&then_body_code}
                    push 0
                    return
            );

            let else_code = triton_asm!(
                {else_subroutine_name}:
                    {&else_body_code}
                    return
            );

            state
                .function_state
                .subroutines
                .push(then_code.try_into().unwrap());
            state
                .function_state
                .subroutines
                .push(else_code.try_into().unwrap());

            if_code
        }

        ast::Stmt::Block(block_stmt) => compile_block_stmt(block_stmt, state),
        ast::Stmt::Assert(ast::AssertStmt { expression }) => {
            let (_addr, assert_expr_code) = compile_expr(expression, "assert-expr", state);

            // evaluated expression value is not visible after `assert` instruction has been executed
            state.function_state.vstack.pop();

            triton_asm!(
                {&assert_expr_code}
                assert
            )
        }
        ast::Stmt::FnDeclaration(function) => {
            let compiled_fn = compile_function_inner(
                function,
                &mut state.global_compiler_state,
                state.libraries,
                state.declared_methods,
                state.associated_functions,
                state.custom_types,
            );
            state
                .function_state
                .add_compiled_fn_to_subroutines(compiled_fn);

            vec![]
        }
        ast::Stmt::Match(ast::MatchStmt {
            arms,
            match_expression,
        }) => {
            let vstack_init = state.function_state.vstack.clone();
            let var_addr_init = state.function_state.var_addr.clone();

            // Evaluate match expression
            let (match_expr_id, match_expr_evaluation) =
                compile_expr(match_expression, "match-expr", state);
            assert!(
                !state.function_state.spill_required.contains(&match_expr_id),
                "Cannot handle memory-spill of evaluated match expressions. But {match_expr_id} required memory spilling"
            );

            let mut match_code = triton_asm!({ &match_expr_evaluation });
            let contains_wildcard = arms
                .iter()
                .any(|x| matches!(x.match_condition, ast::MatchCondition::CatchAll));

            if contains_wildcard {
                // Indicate that no arm body has been executed yet. For wildcard arm-conditions.
                match_code.push(triton_instr!(push 1));
            }

            let match_expression_enum_type = match_expression.get_type().as_enum_type();

            let outer_vstack = state.function_state.vstack.clone();
            let outer_bindings = state.function_state.var_addr.clone();
            for (arm_counter, arm) in arms.iter().enumerate() {
                // At start of each loop-iternation, stack is:
                // stack: _ [expression_variant_data] expression_variant_discriminant <no_arm_taken>

                let arm_subroutine_label = format!("{match_expr_id}_body_{arm_counter}");

                match &arm.match_condition {
                    ast::MatchCondition::EnumVariant(enum_variant) => {
                        // We know that variant discriminant is on top
                        let arm_variant_discriminant = match_expression_enum_type
                            .variant_discriminant(&enum_variant.variant_name);
                        match_code.append(&mut triton_asm!(
                            dup {contains_wildcard as u32}
                            // _ match_expr <no_arm_taken> match_expr_discriminant
                            push {arm_variant_discriminant}
                            // _ match_expr <no_arm_taken> match_expr_discriminant needle_discriminant

                            eq
                            skiz
                            call {arm_subroutine_label}
                        ));

                        let remove_old_any_arm_taken_indicator = if contains_wildcard {
                            triton_asm!(pop)
                        } else {
                            triton_asm!()
                        };
                        let set_new_no_arm_taken_indicator = if contains_wildcard {
                            triton_asm!(push 0)
                        } else {
                            triton_asm!()
                        };

                        // Split compiler's view of evaluated expression from
                        // _ [enum_value]
                        // into
                        // _ [enum_data] [padding] discriminant
                        let [_padding_id, data_id, _discriminant_id] = state.split_value(
                            &match_expr_id,
                            match_expression_enum_type
                                .decompose_variant(&enum_variant.variant_name),
                        );

                        // Insert binding from pattern-match into stack view for arm-body
                        enum_variant.data_binding.as_ref().map(|x| {
                            state
                                .function_state
                                .var_addr
                                .insert(x.name.to_owned(), data_id)
                        });

                        let body_code = compile_block_stmt(&arm.body, state);

                        // This arm-body changes the `arm_taken` bool but otherwise leaves the stack unchanged
                        let subroutine_code = triton_asm!(
                            {arm_subroutine_label}:
                                {&remove_old_any_arm_taken_indicator}
                                // stack: _ [expression_variant_data] [padding] expression_variant_discriminant

                                {&body_code}

                                {&set_new_no_arm_taken_indicator}
                                return
                        );

                        state
                            .function_state
                            .subroutines
                            .push(subroutine_code.try_into().unwrap());
                    }
                    ast::MatchCondition::CatchAll => {
                        // CatchAll (`_`) is guaranteed to be the last arm. So we only have to check if any
                        // previous arm was taken
                        match_code.append(&mut triton_asm!(
                            skiz
                            call {arm_subroutine_label}
                            push 0 // push 0 to make stack-cleanup code-path independent
                        ));

                        let body_code = compile_block_stmt(&arm.body, state);
                        let subroutine_code = triton_asm!(
                            {arm_subroutine_label}:
                                {&body_code}
                                return
                        );
                        state
                            .function_state
                            .subroutines
                            .push(subroutine_code.try_into().unwrap());
                        // stack: _  [expression_variant_data] expression_variant_discriminant <no_arm_taken>
                    }
                }

                // Restore stack view and bindings view for next loop-iteration
                state
                    .function_state
                    .restore_stack_and_bindings(&outer_vstack, &outer_bindings);
            }

            // Cleanup stack by removing evaluated expresison and `any_arm_taken_bool` indicator
            if contains_wildcard {
                match_code.push(triton_instr!(pop));
            }

            // Remove match-expression from stack
            let restore_stack_code = state.restore_stack_code(&vstack_init, &var_addr_init);

            triton_asm!(
                {&match_code}
                {&restore_stack_code}
            )
        }
    }
}

fn compile_fn_call(
    fn_call: &ast::FnCall<type_checker::Typing>,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    let ast::FnCall {
        name,
        args,
        annot: _return_type, // unit for statement-level fn calls
        type_parameter,
        arg_evaluation_order,
    } = fn_call.clone();

    // Compile function arguments either left-to-right or right-to-left depending
    // on definition in function.
    let args_iter = if arg_evaluation_order == ast::ArgEvaluationOrder::LeftToRight {
        Either::Left(args.iter().enumerate())
    } else {
        Either::Right(args.iter().enumerate().rev())
    };

    let (_args_idents, args_code): (Vec<ValueIdentifier>, Vec<Vec<LabelledInstruction>>) =
        args_iter
            .map(|(arg_pos, arg_expr)| {
                let context = format!("_{name}_arg_{arg_pos}");
                compile_expr(arg_expr, &context, state)
            })
            .unzip();

    let mut call_fn_code = vec![];
    for lib in state.libraries.iter() {
        if let Some(fn_name) = lib.get_function_name(&name) {
            call_fn_code.append(&mut lib.call_function(&fn_name, type_parameter, &args, state));
            break;
        }
    }

    // Associated functions are in scope in `ftable`, they must be called with `<Type>::<function_name>`,
    // where function_name must be lower-cased.
    if call_fn_code.is_empty() {
        let split_name = name.split("::").collect_vec();
        if split_name.len() > 1 && split_name[1].chars().next().unwrap().is_lowercase() {
            let associated_type_name = split_name[0];
            let fn_name = split_name[1];

            let assoc_fun = match state.associated_functions.get(associated_type_name) {
                Some(entry) => {
                    match entry.get(fn_name) {
                        Some(function) => function.to_owned(),
                        None => panic!("Don't know associated function '{fn_name}' for type {associated_type_name}"),
                    }},
                    None => panic!("Don't know type {associated_type_name} for which an associated function {fn_name} is made"),
                };

            let function_label: String = assoc_fun.get_tasm_label();
            call_fn_code.append(&mut triton_asm!(call { function_label }));

            if !state
                .global_compiler_state
                .compiled_methods_and_afs
                .contains_key(&function_label)
            {
                // Insert something with the right label *before* compiling,
                // otherwise the methods cannot handle recursion.
                state.global_compiler_state.compiled_methods_and_afs.insert(
                    function_label.clone(),
                    InnerFunctionTasmCode::dummy_value(&function_label),
                );

                // Compile the method as a function and add it to compiled methods
                let compiled_function = compile_function_inner(
                    &assoc_fun,
                    &mut state.global_compiler_state,
                    state.libraries,
                    state.declared_methods,
                    state.associated_functions,
                    state.custom_types,
                );

                state
                    .global_compiler_state
                    .compiled_methods_and_afs
                    .insert(function_label.clone(), compiled_function);
            }
        }
    }

    if call_fn_code.is_empty() {
        // Is the function a enum-variant constructor? `enum Foo { Bar(u32) }; let a = Foo::Bar(42);`
        let split_name = name.split("::").collect_vec();
        if split_name.len() > 1
            && split_name[1].chars().next().unwrap().is_uppercase()
            && state.custom_types.contains_key(split_name[0])
        {
            let custom_type = state
                .custom_types
                .get(split_name[0])
                .unwrap_or_else(|| panic!("Unknown enum-variant constructor: {name}"));
            let enum_type = if let CustomTypeOil::Enum(enum_type) = custom_type {
                enum_type
            } else {
                panic!(
                    "Can only call enum-variant constructor on enum type. Problem was with {name}"
                );
            };
            call_fn_code.append(
                &mut enum_type
                    .variant_constructor(split_name[1], state.custom_types)
                    .body
                    .clone(),
            );
        } else {
            // Is the function a tuple constuctor? `struct Foo(u32); let a = Foo(200);`
            if let Some(custom_type) = state.custom_types.get(&name) {
                if let CustomTypeOil::Struct(struct_type) = custom_type {
                    assert!(
                        matches!(struct_type.variant, StructVariant::TupleStruct(_)),
                        "Can only call tuple constructor of tuple struct. Got: {struct_type}"
                    );
                    call_fn_code
                        .append(&mut struct_type.constructor(state.custom_types).body.clone());
                } else {
                    panic!("Can only call tuple constructor of tuple struct. Got: {custom_type:?}");
                }
            } else {
                // Function is not a library function, but type checker has guaranteed that it is in
                // scope. So we just call it.
                call_fn_code.append(&mut triton_asm!(call { name }));
            }
        }
    }

    // Remove function arguments from vstack since they're not visible after the function call
    for _ in 0..args.len() {
        state.function_state.vstack.pop();
    }

    [args_code.concat(), call_fn_code].concat()
}

fn compile_method_call(
    method_call: &ast::MethodCall<type_checker::Typing>,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    let method_name = method_call.method_name.clone();

    // The type checker might have forced the `receiver_type` here, so
    // we're not allowed to change it again.
    let receiver_type = method_call.args[0].get_type();

    // Compile arguments, including receiver, left-to-right
    let (_args_idents, args_code): (Vec<ValueIdentifier>, Vec<Vec<LabelledInstruction>>) =
        method_call
            .args
            .iter()
            .enumerate()
            .map(|(arg_pos, arg_expr)| {
                let context = format!("_{method_name}_arg_{arg_pos}");
                compile_expr(arg_expr, &context, state)
            })
            .unzip();

    // First check if this is a declared method
    let mut call_code = vec![];
    for declared_method in state.declared_methods.iter() {
        if method_call.method_name == declared_method.signature.name
            && (receiver_type == declared_method.receiver_type() ||
            // TODO: Type checker should handle this. Remove this extra condition!
            ast_types::DataType::Boxed(Box::new(receiver_type.clone())) == declared_method.receiver_type())
        {
            let method_label = declared_method.get_tasm_label();
            if !state
                .global_compiler_state
                .compiled_methods_and_afs
                .contains_key(&method_label)
            {
                // Insert something with the right label *before* compiling,
                // otherwise the methods cannot handle recursion.
                state.global_compiler_state.compiled_methods_and_afs.insert(
                    method_label.clone(),
                    InnerFunctionTasmCode::dummy_value(&method_label),
                );

                // Compile the method as a function and add it to compiled methods
                let compiled_method = compile_function_inner(
                    &declared_method.clone().to_ast_function(&method_label),
                    &mut state.global_compiler_state,
                    state.libraries,
                    state.declared_methods,
                    state.associated_functions,
                    state.custom_types,
                );

                state
                    .global_compiler_state
                    .compiled_methods_and_afs
                    .insert(method_label.clone(), compiled_method);
            }

            call_code.append(&mut triton_asm!(call { method_label }));
        }
    }

    // Then check if it is a method from a library
    if call_code.is_empty() {
        for lib in state.libraries.iter() {
            if let Some(fn_name) = lib.get_method_name(&method_name, &receiver_type) {
                let mut call_method_code =
                    lib.call_method(&fn_name, &receiver_type, &method_call.args, state);
                call_code.append(&mut call_method_code);
                break;
            }
        }
    }

    assert!(
        !call_code.is_empty(),
        "Unknown method: \"{method_name}\" for receiver type \"{receiver_type}\""
    );

    // Update vstack to reflect that all input arguments, including receiver
    // were consumed.
    for _ in 0..method_call.args.len() {
        state.function_state.vstack.pop();
    }

    [args_code.concat(), call_code].concat()
}

fn compile_expr(
    expr: &ast::Expr<type_checker::Typing>,
    _context: &str,
    state: &mut CompilerState,
) -> (ValueIdentifier, Vec<LabelledInstruction>) {
    let result_type = expr.get_type();
    let code = match expr {
        ast::Expr::Lit(expr_lit) => match expr_lit {
            ast::ExprLit::Bool(value) => {
                triton_asm!(push {*value as u64})
            }

            ast::ExprLit::U32(value) => {
                triton_asm!(push {*value as u64})
            }

            ast::ExprLit::BFE(value) => triton_asm!(push {*value }),

            ast::ExprLit::U64(value) => {
                let as_u32s = U32s::<2>::try_from(*value).unwrap().encode();
                let stack_serialized: Vec<_> = as_u32s.iter().rev().collect();

                let code = stack_serialized
                    .iter()
                    .flat_map(|bfe| triton_asm!(push {**bfe}))
                    .collect_vec();

                code
            }

            ast::ExprLit::U128(value) => {
                let mut stack_serialized = value.encode();
                stack_serialized.reverse();
                let code = stack_serialized
                    .iter()
                    .flat_map(|bfe| triton_asm!(push {*bfe}))
                    .collect_vec();

                code
            }

            ast::ExprLit::XFE(value) => {
                // In the VM, the 1st element of the array is expected to be on top of the stack.
                // So the elements must be pushed onto the stack in reversed order.
                triton_asm!(
                    push {value.coefficients[2]}
                    push {value.coefficients[1]}
                    push {value.coefficients[0]}
                )
            }
            ast::ExprLit::Digest(digest) => {
                let digest_values = digest.values();
                triton_asm!(
                    push {digest_values[4]}
                    push {digest_values[3]}
                    push {digest_values[2]}
                    push {digest_values[1]}
                    push {digest_values[0]}
                )
            }
            ast::ExprLit::GenericNum(n, _) => {
                panic!("Type of number literal {n} not resolved")
            }
            ast::ExprLit::MemPointer(ast::MemPointerLiteral {
                mem_pointer_address,
                mem_pointer_declared_type: _,
                resolved_type: _,
            }) => triton_asm!(push {
                mem_pointer_address
            }),
        },

        ast::Expr::Var(identifier) => {
            // TODO: We probably need to use `known_type` here!
            let var_type = identifier.get_type();
            if matches!(var_type, ast_types::DataType::Function(_)) {
                // Identifier is a function and must be in scope since typechecker was passed
                triton_asm!()
            } else {
                let location = state.locate_identifier(identifier);
                if !location.is_accessible(&var_type) {
                    // Compiler must run again. Mark binding as spilled and produce unusable code
                    let binding_name = identifier.binding_name();
                    let value_ident_of_binding = state
                        .function_state
                        .var_addr
                        .get(&binding_name)
                        .unwrap_or_else(|| {
                            panic!("Could not locate value identifier for binding {binding_name}")
                        })
                        .to_owned();
                    state.mark_as_spilled(&value_ident_of_binding);
                    triton_asm!(push 0 assert)
                } else {
                    match location {
                        ValueLocation::OpStack(position) => {
                            dup_value_from_stack_code(position.try_into().unwrap(), &var_type)
                        }
                        ValueLocation::StaticMemoryAddress(pointer) => {
                            load_from_memory(Some(pointer), var_type.stack_size())
                        }
                        ValueLocation::DynamicMemoryAddress(code) => {
                            triton_asm!(
                                {&code}
                                {&dereference(&var_type)}
                            )
                            // if let ast_types::DataType::MemPointer(_) = var_type {
                            //     triton_asm!(
                            //         {&code}
                            //         {&dereference(&var_type)}
                            //     )
                            // } else {
                            //     code
                            // }
                        }
                    }
                }
            }
        }

        ast::Expr::Tuple(exprs) => {
            // Compile arguments left-to-right
            let (idents, code): (Vec<ValueIdentifier>, Vec<Vec<LabelledInstruction>>) = exprs
                .iter()
                .enumerate()
                .map(|(arg_pos, arg_expr)| {
                    let context = format!("_tuple_{arg_pos}");
                    compile_expr(arg_expr, &context, state)
                })
                .unzip();

            // Combine vstack entries into one Tuple entry
            for _ in idents {
                state.function_state.vstack.pop();
            }

            code.concat()
        }

        ast::Expr::Struct(struct_expr) => {
            // It is assumed that the struct fields in the expression are sorted as that in the
            // type declaration, after the type checker has run.
            let (idents, code): (Vec<ValueIdentifier>, Vec<Vec<LabelledInstruction>>) = struct_expr
                .field_names_and_values
                .iter()
                .map(|(field_name, arg_expr)| {
                    let context = format!("_struct_{field_name}");
                    compile_expr(arg_expr, &context, state)
                })
                .unzip();

            // Combine vstack entries into one Struct entry
            for _ in idents {
                state.function_state.vstack.pop();
            }

            code.concat()
        }

        ast::Expr::EnumDeclaration(enum_decl) => {
            // Compile an enum-declaration without associated data
            // Padding is done to ensure that all instances of this enum have
            // the same size on the stack.
            let padding = enum_decl
                .get_type()
                .as_enum_type()
                .padding_size(&enum_decl.variant_name);
            let padding = vec![triton_instr!(push 0); padding];
            let discriminant = enum_decl
                .enum_type
                .as_enum_type()
                .variant_discriminant(&enum_decl.variant_name);
            let discriminant = triton_asm!(push { discriminant });

            [padding, discriminant].concat()
        }

        ast::Expr::FnCall(fn_call) => compile_fn_call(fn_call, state),

        ast::Expr::MethodCall(method_call) => compile_method_call(method_call, state),

        ast::Expr::Unary(unaryop, rhs_expr, _known_type) => {
            let rhs_type = rhs_expr.get_type();
            let (_inner_expr_addr, inner_expr_code) = compile_expr(rhs_expr, "unop_operand", state);
            let code = match unaryop {
                ast::UnaryOp::Neg => match rhs_type {
                    ast_types::DataType::BFE => triton_asm!(push -1 mul),
                    ast_types::DataType::XFE => triton_asm!(push -1 xbmul),
                    _ => panic!("Unsupported negation of type {rhs_type}"),
                },
                ast::UnaryOp::Not => match rhs_type {
                    ast_types::DataType::Bool => triton_asm!(push 0 eq),
                    ast_types::DataType::U32 => triton_asm!(push {u32::MAX as u64} xor),
                    ast_types::DataType::U64 => triton_asm!(
                        swap 1
                        push {u32::MAX as u64}
                        xor
                        swap 1
                        push {u32::MAX as u64}
                        xor
                    ),
                    _ => panic!("Unsupported not of type {rhs_type}"),
                },
                ast::UnaryOp::Deref => {
                    if let ast_types::DataType::Boxed(inner_type) = rhs_type {
                        println!("inner type for deref is: {inner_type}");
                        println!("result_type is {result_type}");
                        println!("expression is: {expr}");
                        dereference(&inner_type)
                    } else {
                        panic!("Cannot dereference non-pointers. Type was: {rhs_type}")
                    }
                }

                // This reference only works for `Vec<T>` and `MemPointer<U>`, for these data types `S` it
                // holds that `eval(MemPointer<S>) == eval(S)`, so we don't need to do anything here. For
                // types that are copy the reference operator is simply ignored.
                ast::UnaryOp::Ref(_mutable) => vec![],
            };

            // Pops the operand from the compiler's view of the stack since
            // the above code consumes that.
            state.function_state.vstack.pop();

            [inner_expr_code, code].concat()
        }

        ast::Expr::Binop(lhs_expr, binop, rhs_expr, _known_type) => {
            let lhs_type = lhs_expr.get_type();

            match binop {
                ast::BinOp::Add => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", state);

                    let add_code = match result_type {
                        ast_types::DataType::U32 => {
                            // We use the safe, overflow-checking, add code as default
                            let safe_add_u32 =
                                state.import_snippet(Box::new(arithmetic::u32::safeadd::Safeadd));
                            triton_asm!(call { safe_add_u32 })
                        }
                        ast_types::DataType::U64 => {
                            // We use the safe, overflow-checking, add code as default
                            let add_u64 =
                                state.import_snippet(Box::new(arithmetic::u64::add_u64::AddU64));

                            triton_asm!(call { add_u64 })
                        }
                        ast_types::DataType::U128 => {
                            // We use the safe, overflow-checking, add code as default
                            let add_u128 =
                                state.import_snippet(Box::new(arithmetic::u128::add_u128::AddU128));

                            triton_asm!(call { add_u128 })
                        }
                        ast_types::DataType::BFE => triton_asm!(add),
                        ast_types::DataType::XFE => {
                            triton_asm!(xxadd swap 3 pop swap 3 pop swap 3 pop)
                        }
                        _ => panic!("Operator add is not supported for type {result_type}"),
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    [lhs_expr_code, rhs_expr_code, add_code].concat()
                }
                ast::BinOp::And => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", state);

                    let and_code = match result_type {
                        ast_types::DataType::Bool => triton_asm!(add push 2 eq),
                        _ => panic!("Logical AND operator is not supported for {result_type}"),
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    [lhs_expr_code, rhs_expr_code, and_code].concat()
                }

                ast::BinOp::BitAnd => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", state);

                    let bitwise_and_code = match result_type {
                        ast_types::DataType::U32 => triton_asm!(and),
                        ast_types::DataType::U64 => {
                            let and_u64 =
                                state.import_snippet(Box::new(arithmetic::u64::and_u64::AndU64));
                            triton_asm!(call { and_u64 })
                        }
                        _ => panic!("Logical AND operator is not supported for {result_type}"),
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    [lhs_expr_code, rhs_expr_code, bitwise_and_code].concat()
                }

                ast::BinOp::BitXor => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", state);

                    use ast_types::DataType::*;
                    let xor_code = match result_type {
                        U32 => triton_asm!(xor),
                        U64 => triton_asm!(
                            swap 3
                            xor
                            swap 2
                            xor
                        ),
                        _ => panic!("xor on {result_type} is not supported"),
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    [lhs_expr_code, rhs_expr_code, xor_code].concat()
                }
                ast::BinOp::BitOr => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", state);

                    use ast_types::DataType::*;

                    let bitwise_or_code = match result_type {
                        U32 => {
                            let or_u32 = state.import_snippet(Box::new(arithmetic::u32::or::Or));
                            triton_asm!(call { or_u32 })
                        }
                        U64 => {
                            let or_u64 =
                                state.import_snippet(Box::new(arithmetic::u64::or_u64::OrU64));
                            triton_asm!(call { or_u64 })
                        }
                        _ => panic!("bitwise `or` on {result_type} is not supported"),
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    [lhs_expr_code, rhs_expr_code, bitwise_or_code].concat()
                }

                ast::BinOp::Div => {
                    use ast_types::DataType::*;
                    match result_type {
                        U32 => {
                            // TODO: Consider evaluating in opposite order to save a clock-cycle by removing `swap1`
                            // below. This would change the "left-to-right" convention though.
                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", state);
                            let (_rhs_expr_addr, rhs_expr_code) =
                                compile_expr(rhs_expr, "_binop_rhs", state);

                            // Pop numerator and denominator
                            state.function_state.vstack.pop();
                            state.function_state.vstack.pop();

                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                swap 1
                                div_mod
                                pop
                            )
                        }
                        U64 => {
                            // Division is very expensive in the general case!
                            // Try to do right shifting instead, if you can.
                            let rhs_expr_owned = *rhs_expr.to_owned();
                            if matches!(rhs_expr_owned, ast::Expr::Lit(ast::ExprLit::U64(2))) {
                                let (_lhs_expr_addr, lhs_expr_code) =
                                    compile_expr(lhs_expr, "_binop_lhs", state);
                                let div2 = state
                                    .import_snippet(Box::new(arithmetic::u64::div2_u64::Div2U64));

                                // Pop the numerator that was divided by two
                                state.function_state.vstack.pop();

                                triton_asm!(
                                    {&lhs_expr_code}
                                    call {div2}
                                )
                            } else {
                                let (_lhs_expr_addr, lhs_expr_code) =
                                    compile_expr(lhs_expr, "_binop_lhs", state);
                                let (_rhs_expr_addr, rhs_expr_code) =
                                    compile_expr(rhs_expr, "_binop_rhs", state);

                                let div_mod_u64 = state.import_snippet(Box::new(
                                    arithmetic::u64::div_mod_u64::DivModU64,
                                ));

                                state.function_state.vstack.pop();
                                state.function_state.vstack.pop();

                                // Call the div-mod function and throw away the remainder
                                triton_asm!(
                                    {&lhs_expr_code}
                                    {&rhs_expr_code}
                                    call {div_mod_u64}
                                    pop
                                    pop
                                )
                            }
                        }
                        BFE => {
                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", state);
                            let (_rhs_expr_addr, rhs_expr_code) =
                                compile_expr(rhs_expr, "_binop_rhs", state);

                            // Pop numerator and denominator
                            state.function_state.vstack.pop();
                            state.function_state.vstack.pop();

                            // vec![lhs_expr_code, rhs_expr_code, bfe_div_code].concat()
                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                invert
                                mul
                            )
                        }
                        XFE => {
                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", state);
                            let (_rhs_expr_addr, rhs_expr_code) =
                                compile_expr(rhs_expr, "_binop_rhs", state);

                            // Pop numerator and denominator
                            state.function_state.vstack.pop();
                            state.function_state.vstack.pop();

                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                xinvert
                                xxmul
                                swap 3
                                pop
                                swap 3
                                pop
                                swap 3
                                pop
                            )
                        }
                        _ => panic!("Unsupported div for type {result_type}"),
                    }
                }

                ast::BinOp::Rem => {
                    use ast_types::DataType::*;
                    match result_type {
                        U32 => {
                            // TODO: Consider evaluating in opposite order to save a clock-cycle by removing `swap1`
                            // below. This would change the "left-to-right" convention though.
                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", state);
                            let (_rhs_expr_addr, rhs_expr_code) =
                                compile_expr(rhs_expr, "_binop_rhs", state);

                            // Pop numerator and denominator
                            state.function_state.vstack.pop();
                            state.function_state.vstack.pop();

                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                swap 1
                                div_mod
                                swap 1
                                pop
                            )
                        }
                        U64 => {
                            // divsion and remainder are very expensive in the general case!
                            // Try to use a bitmask instead.
                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", state);
                            let (_rhs_expr_addr, rhs_expr_code) =
                                compile_expr(rhs_expr, "_binop_rhs", state);

                            let div_mod_u64 = state
                                .import_snippet(Box::new(arithmetic::u64::div_mod_u64::DivModU64));

                            state.function_state.vstack.pop();
                            state.function_state.vstack.pop();

                            // Call the div-mod function and throw away the quotient
                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                call {div_mod_u64}
                                swap 2
                                pop
                                swap 2
                                pop
                            )
                        }
                        _ => panic!("Unsupported remainder of type {lhs_type}"),
                    }
                }

                ast::BinOp::Eq => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", state);

                    let eq_code = compile_eq_code(&lhs_type, state);

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    [lhs_expr_code, rhs_expr_code, eq_code].concat()
                }

                ast::BinOp::Lt => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", state);

                    use ast_types::DataType::*;

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    match lhs_type {
                        U32 => triton_asm!(
                            {&lhs_expr_code}
                            {&rhs_expr_code}
                            swap 1
                            lt
                        ),

                        U64 => {
                            let lt_u64 = state
                                .import_snippet(Box::new(arithmetic::u64::lt_u64::LtStandardU64));
                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                // _ lhs_hi lhs_lo rhs_hi rhs_lo
                                swap 3
                                swap 1
                                swap 3
                                swap 2
                                call {lt_u64}
                            )
                        }
                        _ => panic!("Unsupported < for type {lhs_type}"),
                    }
                }
                // ast::Expr::Binop(lhs_expr, binop, rhs_expr, _known_type)
                ast::BinOp::Gt => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", state);

                    use ast_types::DataType::*;
                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    match lhs_type {
                        U32 => triton_asm!(
                            {&lhs_expr_code}
                            {&rhs_expr_code}
                            lt
                        ),
                        U64 => {
                            let lt_u64 = state
                                .import_snippet(Box::new(arithmetic::u64::lt_u64::LtStandardU64));

                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                // _ lhs_hi lhs_lo rhs_hi rhs_lo
                                call {lt_u64}
                            )
                        }
                        _ => panic!("Unsupported < for type {lhs_type}"),
                    }
                }
                ast::BinOp::Mul => {
                    let rhs_type = rhs_expr.get_type();
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", state);

                    use ast_types::DataType::*;

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    match (&lhs_type, &rhs_type) {
                        (U32, U32) => {
                            let fn_name =
                                state.import_snippet(Box::new(arithmetic::u32::safemul::Safemul));

                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                call {fn_name}
                            )
                        }
                        (U64, U64) => {
                            let fn_name = state.import_snippet(Box::new(
                                arithmetic::u64::safe_mul_u64::SafeMulU64,
                            ));

                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                call {fn_name}
                            )
                        }
                        (U128, U128) => {
                            let fn_name = state.import_snippet(Box::new(
                                arithmetic::u128::safe_mul_u128::SafeMulU128,
                            ));

                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                call {fn_name}
                            )
                        }
                        (BFE, BFE) => triton_asm!(
                            {&lhs_expr_code}
                            {&rhs_expr_code}
                            mul
                        ),
                        (XFE, XFE) => triton_asm!(
                            {&lhs_expr_code}
                            {&rhs_expr_code}
                            xxmul
                            swap 3
                            pop
                            swap 3
                            pop
                            swap 3
                            pop
                        ),
                        (XFE, BFE) => triton_asm!(
                            {&lhs_expr_code}
                            {&rhs_expr_code}
                            xbmul
                        ),
                        _ => panic!("Unsupported MUL for types LHS: {lhs_type}, RHS: {rhs_type}"),
                    }
                }
                ast::BinOp::Neq => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", state);

                    let eq_code = compile_eq_code(&lhs_type, state);

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    triton_asm!(
                        {&lhs_expr_code}
                        {&rhs_expr_code}
                        {&eq_code}
                        push 0
                        eq
                    )
                }

                ast::BinOp::Or => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", state);

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    triton_asm!(
                        {&lhs_expr_code}
                        {&rhs_expr_code}
                        add
                        push 0
                        eq
                        push 0
                        eq
                    )
                }

                ast::BinOp::Shl => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", state);

                    let lhs_type = lhs_expr.get_type();
                    let shl = match lhs_type {
                        ast_types::DataType::U32 => state.import_snippet(Box::new(arithmetic::u32::shiftleft::Shiftleft)),
                        ast_types::DataType::U64 => state.import_snippet(Box::new(
                                    arithmetic::u64::shift_left_u64::ShiftLeftU64,
                                )),
                        ast_types::DataType::U128 => state.import_snippet(Box::new(
                            arithmetic::u128::shift_left_u128::ShiftLeftU128,
                        )),
                        _ => panic!("Unsupported SHL of type {lhs_type}. Expression was `{lhs_expr}: >> {rhs_expr}`; types: `{lhs_type}`, {}.", rhs_expr.get_type()),
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    triton_asm!(
                        {&lhs_expr_code}
                        {&rhs_expr_code}
                        call {shl}
                    )
                }

                ast::BinOp::Shr => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", state);

                    let lhs_type = lhs_expr.get_type();
                    // TODO: add optimization where RHS is a literal. Also applies for `Binop::Shl`. We have code snippets
                    // for left/right shifting u128s with statically known bits.
                    let shr = match lhs_type {
                        ast_types::DataType::U32 => state.import_snippet(Box::new(arithmetic::u32::shiftright::Shiftright)),
                        ast_types::DataType::U64 => state.import_snippet(Box::new(
                                    arithmetic::u64::shift_right_u64::ShiftRightU64,
                                )),
                        ast_types::DataType::U128 => state.import_snippet(Box::new(
                            arithmetic::u128::shift_right_u128::ShiftRightU128,
                        )),
                        _ => panic!("Unsupported SHR of type {lhs_type}. Expression was `{lhs_expr}: >> {rhs_expr}`; types: `{lhs_type}`, {}.", rhs_expr.get_type()),
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    triton_asm!(
                        {&lhs_expr_code}
                        {&rhs_expr_code}
                        call {shr}
                    )
                }

                ast::BinOp::Sub => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", state);

                    let sub_code: Vec<LabelledInstruction> = match result_type {
                        ast_types::DataType::U32 => {
                            // As standard, we use safe arithmetic that crashes on overflow
                            let safe_sub_u32 =
                                state.import_snippet(Box::new(arithmetic::u32::safesub::Safesub));
                            triton_asm!(
                                swap 1
                                call {safe_sub_u32}
                            )
                        }
                        ast_types::DataType::U64 => {
                            // As standard, we use safe arithmetic that crashes on overflow
                            let sub_u64 =
                                state.import_snippet(Box::new(arithmetic::u64::sub_u64::SubU64));
                            triton_asm!(
                                //     // _ lhs_hi lhs_lo rhs_hi rhs_lo
                                swap 3
                                swap 1
                                swap 3
                                swap 2
                                call {sub_u64}
                            )
                        }
                        ast_types::DataType::U128 => {
                            // As standard, we use safe arithmetic that crashes on overflow
                            let sub_u128 =
                                state.import_snippet(Box::new(arithmetic::u128::sub_u128::SubU128));
                            triton_asm!(
                                // _ lhs_3 lhs_2 lhs_1 lhs_0 rhs_3 rhs_2 rhs_1 rhs_0
                                swap 7
                                // _ rhs_0 lhs_2 lhs_1 lhs_0 rhs_3 rhs_2 rhs_1 lhs_3
                                swap 6
                                // _ rhs_0 lhs_3 lhs_1 lhs_0 rhs_3 rhs_2 rhs_1 lhs_2
                                swap 2
                                // _ rhs_0 lhs_3 lhs_1 lhs_0 rhs_3 lhs_2 rhs_1 rhs_2
                                swap 6
                                // _ rhs_0 rhs_2 lhs_1 lhs_0 rhs_3 lhs_2 rhs_1 lhs_3
                                swap 3
                                // _ rhs_0 rhs_2 lhs_1 lhs_0 lhs_3 lhs_2 rhs_1 rhs_3
                                swap 7
                                // _ rhs_3 rhs_2 lhs_1 lhs_0 lhs_3 lhs_2 rhs_1 rhs_0
                                swap 1
                                // _ rhs_3 rhs_2 lhs_1 lhs_0 lhs_3 lhs_2 rhs_0 rhs_1
                                swap 5
                                // _ rhs_3 rhs_2 rhs_1 lhs_0 lhs_3 lhs_2 rhs_0 lhs_1
                                swap 1
                                // _ rhs_3 rhs_2 rhs_1 lhs_0 lhs_3 lhs_2 lhs_1 rhs_0
                                swap 4
                                // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0
                                call {sub_u128}
                            )
                        }
                        ast_types::DataType::BFE => {
                            triton_asm!(
                                push -1
                                mul
                                add
                            )
                        }
                        ast_types::DataType::XFE => {
                            triton_asm!(
                                  // multiply top element with -1
                                push -1
                                xbmul
                                // Perform (lhs - rhs)
                                xxadd
                                // Get rid of the lhs, only leaving the result
                                swap 3
                                pop
                                swap 3
                                pop
                                swap 3
                                pop
                            )
                        }
                        _ => panic!("subtraction operator is not supported for {result_type}"),
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    triton_asm!(
                        {&lhs_expr_code}
                        {&rhs_expr_code}
                        {&sub_code}
                    )
                }
            }
        }

        ast::Expr::If(ast::ExprIf {
            condition,
            then_branch,
            else_branch,
        }) => {
            let (_cond_addr, cond_code) = compile_expr(condition, "if_cond", state);

            // Condition is handled immediately and it is not on the stack when
            // the `then` or `else` branches are entered.
            state.function_state.vstack.pop();

            let (then_addr, then_code) = compile_returning_block_expr(_context, state, then_branch);
            let (else_addr, else_code) = compile_returning_block_expr(_context, state, else_branch);

            // Both branches are compiled as subroutines which are called depending on what `cond`
            // evaluates to.
            let then_subroutine_name = format!("{then_addr}_then");
            let else_subroutine_name = format!("{else_addr}_else");

            let code = triton_asm!(
                {&cond_code}
                push 1
                swap 1
                skiz
                call {then_subroutine_name}
                skiz
                call {else_subroutine_name}
            );

            let then_code = triton_asm!(
                {then_subroutine_name}:
                    pop
                    {&then_code}
                    push 0
                    return
            );

            let else_code = triton_asm!(
                {else_subroutine_name}:
                    {&else_code}
                    return
            );

            state
                .function_state
                .subroutines
                .push(then_code.try_into().unwrap());
            state
                .function_state
                .subroutines
                .push(else_code.try_into().unwrap());

            code
        }

        ast::Expr::ReturningBlock(ret_block) => {
            let (_, code) = compile_returning_block_expr(_context, state, ret_block);

            code
        }

        ast::Expr::Cast(expr, _as_type) => {
            let previous_type = expr.get_type();
            let (_expr_addr, expr_code) = compile_expr(expr, "as", state);
            let (_, (_old_data_type, spilled)) = state.function_state.vstack.pop().unwrap();

            // I don't think this value *can* be spilled unless maybe if you make a tuple
            // that's longer than 16 words which we probably can't handle anyway
            assert!(
                spilled.is_none(),
                "Can't handle spilled values in casting yet"
            );

            match (&previous_type, &result_type) {
                (ast_types::DataType::U64, ast_types::DataType::U32) => {
                    triton_asm!(
                        {&expr_code}
                        swap 1
                        pop
                    )
                }
                (ast_types::DataType::U32, ast_types::DataType::U64) => {
                    triton_asm!({ &expr_code } push 0 swap 1)
                }
                (ast_types::DataType::U32, ast_types::DataType::U128) => {
                    triton_asm!(
                        {&expr_code}
                        push 0
                        push 0
                        push 0
                        swap 3
                    )
                }
                (ast_types::DataType::U64, ast_types::DataType::U128) => {
                    triton_asm!(
                        {&expr_code}
                        push 0
                        push 0
                        swap 3
                        swap 1
                        swap 2
                    )
                }
                // Allow identity-casting since we might need this to make the types
                // agree with code compiled by rustc.
                (ast_types::DataType::U32, ast_types::DataType::U32) => expr_code,
                (ast_types::DataType::U64, ast_types::DataType::U64) => expr_code,
                (ast_types::DataType::Bool, ast_types::DataType::U64) => {
                    triton_asm!(
                        {&expr_code}
                        push 0
                        swap 1
                    )
                }
                (ast_types::DataType::Bool, ast_types::DataType::U32) => expr_code,
                (ast_types::DataType::Bool, ast_types::DataType::BFE) => expr_code,
                (ast_types::DataType::U128, ast_types::DataType::U64) => {
                    triton_asm!(
                    {&expr_code}
                    swap 2
                    pop
                    swap 2
                    pop
                    )
                }
                _ => todo!("previous_type: {previous_type}; result_type: {result_type}"),
            }
        }
    };

    // Update compiler's view of the stack with the new value. Check if value needs to
    // be spilled to memory.
    let binding_description = format!(
        "{}__L{}R",
        expr.label_friendly_name(),
        result_type.label_friendly_name()
    );
    let (addr, spill) = state.new_value_identifier(&binding_description, &result_type);
    let spill_code = spill
        .map(|x| copy_top_stack_value_to_memory(x, result_type.stack_size()))
        .unwrap_or_default();

    (addr, [code, spill_code].concat())
}

fn compile_returning_block_expr(
    context: &str,
    state: &mut CompilerState,
    ret_block: &ast::ReturningBlock<type_checker::Typing>,
) -> (ValueIdentifier, Vec<LabelledInstruction>) {
    let start_vstack = state.function_state.vstack.clone();
    let start_var_addr = state.function_state.var_addr.clone();
    let statement_code = ret_block
        .stmts
        .iter()
        .map(|stmt| compile_stmt(stmt, state))
        .collect_vec()
        .concat();
    let (expr_add, expr_code) = compile_expr(
        &ret_block.return_expr,
        &format!("{context}_return_expression"),
        state,
    );

    // Cleanup stack and variable name mapping after `then` body. Preserve the return
    // value from the `then` branch on the stack, but not on vstack as this value is
    // not visible to the `else` branch.
    let cleanup_code =
        state.clear_all_but_top_stack_value_above_height(start_vstack.get_stack_height());

    state.function_state.vstack = start_vstack.clone();
    state.function_state.var_addr = start_var_addr.clone();
    state.verify_same_ordering_of_bindings(&start_vstack, &start_var_addr);

    (expr_add, [statement_code, expr_code, cleanup_code].concat())
}

/// Return the code to move the top stack element to a
/// specific memory address. Pops top stack value.
/// If no static memory pointer is provided, pointer is assumed to be on
/// top of the stack, above the value that is moved to memory, in
/// which case this address is also popped from the stack.
pub fn move_top_stack_value_to_memory(
    static_memory_location: Option<u32>,
    top_value_size: usize,
) -> Vec<LabelledInstruction> {
    // A stack value of the form `_ val2 val1 val0`, with `val0` being on the top of the stack
    // is stored in memory as: `val0 val1 val2`, where `val0` is stored on the `memory_location`
    // address.
    let mut ret = match static_memory_location {
        Some(static_mem_addr) => triton_asm!(push {static_mem_addr as u64}),
        None => triton_asm!(),
    };

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

    // remove memory address from top of stack
    ret.push(triton_instr!(pop));

    ret
}

/// Return the code to store the top stack element at a
/// specific memory address. Leaves the stack unchanged.
/// Limitation: Can only copy values smaller than 16 for now
fn copy_top_stack_value_to_memory(
    memory_location: u32,
    top_value_size: usize,
) -> Vec<LabelledInstruction> {
    // A stack value of the form `_ val2 val1 val0`, with `val0` being on the top of the stack
    // is stored in memory as: `val0 val1 val2`, where `val0` is stored on the `memory_location`
    // address.
    let mut ret = triton_asm!(push {memory_location as u64});

    assert!(
        top_value_size < SIZE_OF_ACCESSIBLE_STACK,
        "Can only copy values of size less than 16 for now"
    );
    for i in 0..top_value_size {
        ret.append(&mut triton_asm!(dup {1 + i as u64}));
        // _ [elements] mem_address element

        ret.push(triton_instr!(write_mem));
        // _ [elements] mem_address

        if i != top_value_size - 1 {
            ret.append(&mut triton_asm!(push 1 add));
            // _ (mem_address + 1)
        }
    }

    // remove memory address from top of stack
    ret.push(triton_instr!(pop));

    ret
}

/// Returns the code to convert a `MemPointer<data_type>` into a `data_type` on the stack. Consumes the
/// address (`MemPointer<data_type>`) that is on top of the stack.
fn dereference(data_type: &ast_types::DataType) -> Vec<LabelledInstruction> {
    match data_type {
        // From the TASM perspective, a mempointer to a list is the same as a list
        ast_types::DataType::List(_, _) => triton_asm!(),
        ast_types::DataType::Boxed(_) => triton_asm!(),

        // No idea how to handle these yet
        ast_types::DataType::VoidPointer => todo!(),
        ast_types::DataType::Function(_) => todo!(),
        ast_types::DataType::Unresolved(_) => todo!(),

        // Simple data types are simple read from memory and placed on the stack
        ast_types::DataType::Struct(_) => load_from_memory(None, data_type.stack_size()),
        _ => load_from_memory(None, data_type.stack_size()),
    }
}

/// Return the code to load a value from memory. Leaves the stack with the read value on top.
/// If no static memory address is provided, the memory address is read from top of the stack,
/// and this memory address is then consumed.
fn load_from_memory(
    static_memory_address: Option<u32>,
    value_size: usize,
) -> Vec<LabelledInstruction> {
    // A stack value of the form `_ val2 val1 val0`, with `val0` being on the top of the stack
    // is stored in memory as: `val0 val1 val2`, where `val0` is stored on the `memory_location`
    // address. So we read the value at the highest memory location first.
    // TODO: Consider making subroutines out of this in
    // order to get shorter programs.
    let mut ret = match static_memory_address {
        Some(mem_location) => triton_asm!(push {mem_location as u64 + value_size as u64 - 1}),
        None => {
            if value_size.is_one() {
                triton_asm!()
            } else {
                triton_asm!(push {value_size as u64 - 1} add)
            }
        }
    };

    // stack: _ memory_address_of_last_word

    for i in 0..value_size {
        // Stack: _ memory_address

        ret.push(triton_instr!(read_mem));
        // Stack: _ memory_address value

        ret.push(triton_instr!(swap 1));

        // Decrement memory address to prepare for next loop iteration
        if i != value_size - 1 {
            ret.append(&mut triton_asm!(push {BFieldElement::MAX} add))
            // Stack: _ (memory_address - 1)
        }
    }

    // Remove memory address from top of stack
    ret.push(triton_instr!(pop));

    // Stack: _ element_N element_{N - 1} ... element_0

    ret
}

fn compile_eq_code(
    lhs_type: &ast_types::DataType,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    use ast_types::DataType::*;
    match lhs_type {
        Bool | U32 | BFE | VoidPointer => triton_asm!(eq),
        U64 => triton_asm!(
            // _ a_hi a_lo b_hi b_lo
            swap 3
            eq
            swap 2
            eq
            mul
        ),
        U128 => triton_asm!(
            // _ a_3 a_2 a_1 a_0 b_3 b_2 b_1 b_0
            swap 5
            eq
            // _ a_3 a_2 b_0 a_0 b_3 b_2 (b_1 == a_1)
            swap 5
            eq
            // _ a_3 (b_1 == a_1) b_0 a_0 b_3 (b_2 == a_2)
            swap 5
            eq
            // _ (b_2 == a_2) (b_1 == a_1) b_0 a_0 (b_3 == a_3)
            swap 2
            eq
            // _ (b_2 == a_2) (b_1 == a_1) (b_3 == a_3) (b_0 == a_0)
            mul
            mul
            mul
            // _ (b_2 == a_2) * (b_1 == a_1) * (b_3 == a_3) * (b_0 == a_0)
        ),

        XFE => triton_asm!(
             // _ a_2 a_1 a_0 b_2 b_1 b_0
            swap 4 // _ a_2 b_0 a_0 b_2 b_1 a_1
            eq     // _ a_2 b_0 a_0 b_2 (b_1 == a_1)
            swap 4 // _ (b_1 == a_1) b_0 a_0 b_2 a_2
            eq     // _ (b_1 == a_1) b_0 a_0 (b_2 == a_2)
            swap 2 // _ (b_1 == a_1) (b_2 == a_2) a_0 b_0
            eq     // _ (b_1 == a_1) (b_2 == a_2) (a_0 == b_0)
            mul    // _ (b_1 == a_1) ((b_2 == a_2)·(a_0 == b_0))
            mul    // _ ((b_1 == a_1)·(b_2 == a_2)·(a_0 == b_0))
        ),
        Digest => {
            let eq_digest = state.import_snippet(Box::new(hashing::eq_digest::EqDigest));
            triton_asm!(call { eq_digest })
        }
        List(_, _) => todo!(),
        Tuple(_) => todo!(),
        Function(_) => todo!(),
        Struct(_) => todo!(),
        Boxed(_) => todo!("Comparison of MemPointer not supported yet"),
        Unresolved(name) => panic!("Cannot compare unresolved type {name}"),
        Reference(_) => panic!("Cannot compare references. Got {lhs_type}"),
        Enum(_) => todo!("Equality for enums not yet implemented"),
    }
}

/// Copy a value at a position on the stack to the top
fn dup_value_from_stack_code(
    position: OpStackElement,
    data_type: &ast_types::DataType,
) -> Vec<LabelledInstruction> {
    let elem_size = data_type.stack_size();

    // the position of the deepest element of the value.
    let n: usize = Into::<usize>::into(position) + elem_size - 1;

    let instrs_as_str = format!("dup {}\n", n);
    let instrs_as_str = instrs_as_str.repeat(elem_size);

    triton_asm!({ instrs_as_str })
}

impl ast_types::StructType {
    /// Assuming the stack top points to the start of the struct, returns the code
    /// that modifies the top stack value to point to the indicated field. So the top
    /// stack element is consumed and the returned value is a pointer to the requested
    /// field in the struct. Note that the top of the stack is where the field begins,
    /// not the size indication of that field.
    pub fn get_field_accessor_code_for_reference(
        &self,
        field_id: &ast_types::FieldId,
    ) -> Vec<LabelledInstruction> {
        // This implementation must match `BFieldCodec` for the equivalent Rust types
        let mut instructions = vec![];
        let mut static_pointer_addition = 0;
        let needle_id = field_id;
        let mut needle_type: Option<ast_types::DataType> = None;
        for (haystack_field_id, haystack_type) in self.field_ids_and_types_reversed_for_tuples() {
            if haystack_field_id == *needle_id {
                // If we've found the field the accumulators are in the right state.
                // return them.
                needle_type = Some(haystack_type.to_owned());
                break;
            } else {
                // We have not reached the field yet. If the field has a statically
                // known size, we can just add that number to the accumulator. Otherwise,
                // we have to read the size of the field from RAM, and add that value
                // to the pointer
                match haystack_type.bfield_codec_length() {
                    Some(static_length) => static_pointer_addition += static_length,
                    None => {
                        if !static_pointer_addition.is_zero() {
                            instructions
                                .append(&mut triton_asm!(push {static_pointer_addition} add));
                        }
                        instructions.append(&mut triton_asm!(read_mem add push 1 add));
                        static_pointer_addition = 0;
                    }
                }
            }
        }

        // If the requested field is dynamically sized, add one to address, to point to start
        // of the field instead of the size of the field.
        match needle_type.unwrap().bfield_codec_length() {
            Some(_) => (),
            None => static_pointer_addition += 1,
        }

        if !static_pointer_addition.is_zero() {
            instructions.append(&mut triton_asm!(push {static_pointer_addition} add));
        }

        instructions
    }
}
