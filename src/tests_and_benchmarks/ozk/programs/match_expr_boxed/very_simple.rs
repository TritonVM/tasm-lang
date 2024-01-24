use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use triton_vm::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

#[derive(BFieldCodec, Clone, Debug)]
enum SimpleEnum {
    A,
    B(BFieldElement),
}

fn main() {
    let boxed_enum_type: Box<SimpleEnum> =
        SimpleEnum::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();
    let evaluated_discriminant: u32 = match boxed_enum_type.as_ref() {
        SimpleEnum::A => {
            //
            0
        }
        SimpleEnum::B(inner) => {
            //
            tasm::tasm_io_write_to_stdout___bfe(*inner);
            1
        }
    };

    tasm::tasm_io_write_to_stdout___u32(evaluated_discriminant);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    #[test]
    fn very_simple_test() {
        let variant_a = SimpleEnum::A;
        let non_determinism_a = init_memory_from(&variant_a, BFieldElement::new(0));
        let native_output_a =
            rust_shadows::wrap_main_with_io(&main)(Vec::default(), non_determinism_a.clone());
        let entrypoint = EntrypointLocation::disk("match_expr_boxed", "very_simple", "main");
        let vm_output_a = TritonVMTestCase::new(entrypoint.clone())
            .expect_stack_difference(0)
            .with_non_determinism(non_determinism_a)
            .execute()
            .unwrap();
        assert_eq!(native_output_a, vm_output_a.output);

        let variant_b = SimpleEnum::B(BFieldElement::new(12345678901234567890));
        let non_determinism_b = init_memory_from(&variant_b, BFieldElement::new(0));
        let native_output_b =
            rust_shadows::wrap_main_with_io(&main)(Vec::default(), non_determinism_b.clone());
        let vm_output_b = TritonVMTestCase::new(entrypoint)
            .expect_stack_difference(0)
            .with_non_determinism(non_determinism_b)
            .execute()
            .unwrap();
        assert_eq!(native_output_b, vm_output_b.output);
    }
}
