use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

use super::simple_enum_type::*;

fn catch_all_on_b() {
    let boxed_enum_type: Box<SimpleEnum> =
        SimpleEnum::decode(&tasm::load_from_memory(BFieldElement::new(8))).unwrap();
    let evaluated_discriminant: u32 = match boxed_enum_type.as_ref() {
        SimpleEnum::A => {
            //
            49
        }
        _ => {
            //
            100
        }
    };

    tasm::tasm_io_write_to_stdout___u32(evaluated_discriminant);

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    use super::*;

    #[test]
    fn catch_all_on_b_test() {
        println!("a");
        let variant_a = SimpleEnum::A;
        let non_determinism_a = init_memory_from(&variant_a, BFieldElement::new(8));
        let native_output_a = rust_shadows::wrap_main_with_io(&catch_all_on_b)(
            Vec::default(),
            non_determinism_a.clone(),
        );
        let entrypoint =
            EntrypointLocation::disk("match_expr_boxed", "with_catch_all", "catch_all_on_b");
        let code = TritonVMTestCase::new(entrypoint.clone()).compile();
        println!("code:\n{}", code.iter().join("\n"));
        let vm_output_a = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(non_determinism_a)
            .execute()
            .unwrap();
        assert_eq!(native_output_a, vm_output_a.public_output);

        println!("b");
        let variant_b = SimpleEnum::B(BFieldElement::new(12345678901234567890));
        let non_determinism_b = init_memory_from(&variant_b, BFieldElement::new(8));
        let native_output_b = rust_shadows::wrap_main_with_io(&catch_all_on_b)(
            Vec::default(),
            non_determinism_b.clone(),
        );
        let vm_output_b = TritonVMTestCase::new(entrypoint)
            .with_non_determinism(non_determinism_b)
            .execute()
            .unwrap();
        assert_eq!(native_output_b, vm_output_b.public_output);
    }
}
