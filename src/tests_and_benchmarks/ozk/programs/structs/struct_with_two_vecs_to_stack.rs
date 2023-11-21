use arbitrary::Arbitrary;
use triton_vm::Digest;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Arbitrary, BFieldCodec, Clone, Debug)]
struct NotCopyStruct {
    digests: Vec<Digest>,
    xfes: Vec<XFieldElement>,
}

fn main() {
    let boxed_enum_type: Box<NotCopyStruct> =
        NotCopyStruct::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();

    let on_stack: NotCopyStruct = *boxed_enum_type;

    tasm::tasm_io_write_to_stdout___u32(on_stack.digests.len() as u32);
    tasm::tasm_io_write_to_stdout___u32(on_stack.xfes.len() as u32);

    return;
}

mod tests {
    use super::*;
    use arbitrary::Unstructured;
    use itertools::Itertools;
    use rand::random;
    use std::collections::HashMap;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::{
        execute_compiled_with_stack_memory_and_ins_for_test, init_memory_from,
    };

    #[test]
    fn struct_with_two_vecs_to_stack_test() {
        for _ in 0..2 {
            let rand: [u8; 100] = random();
            let sv = NotCopyStruct::arbitrary(&mut Unstructured::new(&rand)).unwrap();
            let non_determinism = init_memory_from(&sv, BFieldElement::new(84));
            let stdin = vec![];

            // Run program on host machine
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

            // Run test on Triton-VM
            let test_program = ozk_parsing::compile_for_test(
                "structs",
                "struct_with_two_vecs_to_stack",
                "main",
                crate::ast_types::ListType::Unsafe,
            );
            let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
                &test_program,
                vec![],
                &mut HashMap::default(),
                stdin,
                non_determinism.clone(),
                0,
            )
            .unwrap();
            if native_output != vm_output.output {
                {
                    let mut ram: Vec<(BFieldElement, BFieldElement)> =
                        non_determinism.ram.clone().into_iter().collect();
                    ram.sort_unstable_by_key(|(p, _v)| p.value());
                    println!(
                        "{}",
                        ram.iter().map(|(p, v)| format!("{p} => {v}")).join(", ")
                    );
                }
                panic!(
                    "native_output:\n{}\nVM output:\n{}. Code was:\n{}\nrand was {}\ninput was: {sv:#?}",
                    native_output.iter().join(", "),
                    vm_output.output.iter().join(", "),
                    test_program.iter().join("\n"),
                    rand.iter().join(",")
                );
            }
        }
    }
}