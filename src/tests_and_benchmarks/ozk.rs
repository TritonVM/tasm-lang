// Programs defined in the `ozk::programs` directory must be translatable by both Rust and
// tasm-lang, so we need to give tasm-lang some rope here.
pub(crate) mod ozk_parsing;
#[allow(clippy::needless_return)]
mod programs;
#[cfg(test)]
pub(crate) mod rust_shadows;
