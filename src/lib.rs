pub mod ast;
pub mod ast_types;
pub mod cfg;
mod compiled_tasm;
#[macro_use]
pub mod graft;
pub mod custom_type_resolver;
pub mod libraries;
pub mod ssa;
mod subroutine;
pub mod tasm_code_generator;
#[cfg(test)]
pub mod tests_and_benchmarks;
pub mod type_checker;
