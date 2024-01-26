use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use num::Zero;
use triton_vm::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;
use twenty_first::shared_math::x_field_element::XFieldElement;

#[derive(BFieldCodec, Clone, Debug)]
enum EnumWithArrayData {
    A,
    B([XFieldElement; 4]),
}

impl EnumWithArrayData {
    pub(crate) fn panic_on_a(&self) -> [XFieldElement; 4] {
        #[allow(unused_assignments)]
        let mut ood_quotient_segments: [XFieldElement; 4] = [XFieldElement::zero(); 4];
        match self {
            EnumWithArrayData::B(xs) => {
                ood_quotient_segments = *xs;
            }
            _ => {
                panic!();
            }
        };

        return ood_quotient_segments;
    }
}

fn call_panic_on_a() {
    let boxed_enum_type: Box<EnumWithArrayData> =
        EnumWithArrayData::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();
    let array_value: [XFieldElement; 4] = boxed_enum_type.panic_on_a();
    tasm::tasm_io_write_to_stdout___xfe(array_value[0]);
    tasm::tasm_io_write_to_stdout___xfe(array_value[1]);
    tasm::tasm_io_write_to_stdout___xfe(array_value[2]);
    tasm::tasm_io_write_to_stdout___xfe(array_value[3]);

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    #[test]
    fn call_panic_on_a_variant_b() {
        let bfes = (12..=24).map(BFieldElement::new);
        let variant_b = EnumWithArrayData::B(
            bfes.tuples()
                .map(|(i0, i1, i2)| XFieldElement::new([i0, i1, i2]))
                .collect_vec()
                .try_into()
                .unwrap(),
        );
        let non_determinism_a = init_memory_from(&variant_b, BFieldElement::new(0));
        let native_output_b = rust_shadows::wrap_main_with_io(&call_panic_on_a)(
            Vec::default(),
            non_determinism_a.clone(),
        );
        let entrypoint = EntrypointLocation::disk(
            "match_stmt_boxed",
            "set_array_value_from_memory",
            "call_panic_on_a",
        );
        let vm_output_b = TritonVMTestCase::new(entrypoint.clone())
            .expect_stack_difference(0)
            .with_non_determinism(non_determinism_a)
            .execute()
            .unwrap();
        assert_eq!(native_output_b, vm_output_b.output);
    }
}
