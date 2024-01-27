use super::three_variants_type::*;
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::triton_vm::prelude::*;
use tasm_lib::Digest;

#[allow(clippy::collapsible_else_if)]
fn main() {
    let input: u32 = tasm::tasm_io_read_stdin___u32();
    let discriminant: u32 = input % 3;
    let val: ThreeVariants = if discriminant == 0 {
        ThreeVariants::A
    } else {
        if discriminant == 1 {
            ThreeVariants::B(1u128 << 101)
        } else {
            ThreeVariants::C(Digest::default())
        }
    };

    let next_variant: ThreeVariants = match val {
        ThreeVariants::A => {
            tasm::tasm_io_write_to_stdout___u32(0);
            ThreeVariants::B(303)
        }
        ThreeVariants::B(_) => {
            tasm::tasm_io_write_to_stdout___u32(1);
            ThreeVariants::C(Digest::default())
        }
        ThreeVariants::C(_) => {
            tasm::tasm_io_write_to_stdout___u32(2);
            ThreeVariants::A
        }
    };

    let final_discriminant: u32 = match next_variant {
        ThreeVariants::A => {
            //
            tasm::tasm_io_write_to_stdout___u32(0);
            tasm::tasm_io_write_to_stdout___u32(0);
            0
        }
        ThreeVariants::B(_) => {
            //
            tasm::tasm_io_write_to_stdout___u32(1);
            tasm::tasm_io_write_to_stdout___u32(1);
            1
        }
        ThreeVariants::C(_) => {
            //
            tasm::tasm_io_write_to_stdout___u32(2);
            tasm::tasm_io_write_to_stdout___u32(2);
            2
        }
    };

    tasm::tasm_io_write_to_stdout___u32(final_discriminant);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use rand::random;

    #[test]
    fn three_variants_test() {
        let input: Vec<u32> = vec![random()];
        let input: Vec<BFieldElement> = input
            .into_iter()
            .map(|x| BFieldElement::new(x as u64))
            .collect();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(input.clone(), NonDeterminism::default());
        let entrypoint = EntrypointLocation::disk("match_expr_on_stack", "three_variants", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(input)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
