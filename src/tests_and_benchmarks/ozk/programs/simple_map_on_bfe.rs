use itertools::Itertools;
use triton_vm::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

pub(crate) fn main() {
    fn local_function(input: BFieldElement) -> BFieldElement {
        return input * BFieldElement::new(2);
    }
    let mut input_values: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(200);

    let length_indication: usize = tasm::tasm_io_read_stdin_bfe().value() as usize;
    let mut i: usize = 0;
    while i < length_indication {
        input_values.push(tasm::tasm_io_read_stdin_bfe());
        i += 1;
    }
    let output_values: Vec<BFieldElement> =
        input_values.into_iter().map(local_function).collect_vec();

    let mut j: usize = 0;
    while j < length_indication {
        tasm::tasm_io_write_to_stdout_bfe(output_values[j]);
        j += 1;
    }

    return;
}
