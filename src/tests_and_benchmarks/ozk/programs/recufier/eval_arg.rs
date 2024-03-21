use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use num::One;
use tasm_lib::triton_vm::prelude::*;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct EvalArg;

impl EvalArg {
    fn _default_initial() -> XFieldElement {
        return XFieldElement::one();
    }

    /// Compute the evaluation for an evaluation argument as specified by `initial`, `challenge`,
    /// and `symbols`. This amounts to evaluating polynomial
    /// `f(x) = initial·x^n + Σ_i symbols[n-i]·x^i`
    /// at point `challenge`, _i.e._, returns `f(challenge)`.
    /// Consider using `tasm-lib` snippets directly instead of this code. The `tasm-lib` snippets
    /// produce a much shorter execution trace.
    fn compute_terminal(
        symbols: Vec<BFieldElement>,
        initial: XFieldElement,
        challenge: XFieldElement,
    ) -> XFieldElement {
        let mut acc: XFieldElement = initial;

        let symbols_length: usize = symbols.len();
        let mut i: usize = 0;
        while i < symbols_length {
            acc = acc * challenge + symbols[i];
            i += 1;
        }

        return acc;
    }
}

#[cfg(test)]
mod test {
    use self::tasm::wrap_main_with_io;
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
    use rand::thread_rng;
    use rand::Rng;
    use tasm_lib::twenty_first::shared_math::other::random_elements;
    use tasm_lib::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

    fn call_compute_terminal() {
        let symbols_length: usize = tasm::tasm_io_read_stdin___u32() as usize;
        let mut symbols: Vec<BFieldElement> = Vec::<BFieldElement>::default();

        {
            let mut i: usize = 0;
            while i < symbols_length {
                symbols.push(tasm::tasm_io_read_stdin___bfe());
                i += 1;
            }
        }

        let initial: XFieldElement = tasm::tasm_io_read_stdin___xfe();
        let challenge: XFieldElement = tasm::tasm_io_read_stdin___xfe();

        let terminal: XFieldElement = EvalArg::compute_terminal(symbols, initial, challenge);
        tasm::tasm_io_write_to_stdout___xfe(terminal);

        return;
    }

    #[test]
    fn test_eval_arg_compute_terminal() {
        let symbols_length: u32 = thread_rng().gen_range(0..200);
        let std_in = [
            vec![BFieldElement::new(symbols_length as u64)],
            random_elements(symbols_length as usize),
            random_elements(EXTENSION_DEGREE * 2),
        ]
        .concat();
        let native_output =
            wrap_main_with_io(&call_compute_terminal)(std_in.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("recufier", "eval_arg", "test::call_compute_terminal");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
