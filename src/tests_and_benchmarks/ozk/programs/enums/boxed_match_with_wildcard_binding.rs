use triton_vm::twenty_first::shared_math::bfield_codec::BFieldCodec;
use triton_vm::BFieldElement;
use triton_vm::Digest;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(BFieldCodec)]
pub(crate) enum EnumType {
    A(Vec<Digest>),
    B(Vec<Vec<BFieldElement>>),
}

impl EnumType {
    fn discriminant(&self) -> BFieldElement {
        #[allow(unused_assignments)]
        let mut discriminant: BFieldElement = BFieldElement::new(u32::MAX as u64);
        match self {
            EnumType::A(_) => {
                discriminant = BFieldElement::new(0);
            }
            EnumType::B(_) => {
                discriminant = BFieldElement::new(1);
            }
        };

        return discriminant;
    }
}

fn main() {
    let boxed_proof_item: Box<EnumType> =
        EnumType::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    tasm::tasm_io_write_to_stdout___bfe(boxed_proof_item.discriminant());

    return;
}

#[cfg(test)]
mod test {

    use itertools::Itertools;
    use rand::random;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;

    use super::*;

    #[test]
    fn boxed_match_with_wildcard_binding_test() {
        let a_0: Digest = random();
        let proof_item = EnumType::A(vec![a_0]);
        let non_determinism = init_memory_from(&proof_item, BFieldElement::new(84));
        let stdin = vec![];

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        // Run test on Triton-VM
        let entrypoint_location =
            EntrypointLocation::disk("enums", "boxed_match_with_wildcard_binding", "main");
        let test_program =
            ozk_parsing::compile_for_test(&entrypoint_location, crate::ast_types::ListType::Unsafe);
        println!("executing:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
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
                "native_output:\n {}, got:\n{}. Code was:\n{}",
                native_output.iter().join(", "),
                vm_output.output.iter().join(", "),
                test_program.iter().join("\n")
            );
        }
    }
}
