mod benchmarks;
// Programs defined in the `ozk_programs` directory must be translatable by both Rust and
// tasm-lang, so we need to give tasm-lang some rope here.
#[allow(clippy::needless_return)]
pub mod ozk_programs;
mod ozk_tests;
mod programs;
mod test_helpers;
