use tasm_lib::structure::tasm_object::TasmObject;
use tasm_lib::triton_vm::prelude::BFieldCodec;
use tasm_lib::triton_vm::prelude::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::triton_vm::proof::Claim;
use crate::triton_vm::proof::Proof;
use crate::twenty_first::prelude::Digest;

#[derive(Clone, Debug, PartialEq, Eq, BFieldCodec, TasmObject)]
pub struct ProofCollection {
    pub removal_records_integrity: Proof,
    pub collect_lock_scripts: Proof,
    pub lock_scripts_halt: Vec<Proof>,
    pub kernel_to_outputs: Proof,
    pub collect_type_scripts: Proof,
    pub type_scripts_halt: Vec<Proof>,
    pub lock_script_hashes: Vec<Digest>,
    pub type_script_hashes: Vec<Digest>,
    pub kernel_mast_hash: Digest,
    pub salted_inputs_hash: Digest,
    pub salted_outputs_hash: Digest,
}

impl ProofCollection {
    fn removal_records_integrity_claim(&self) -> Claim {
        return Claim {
            // program_digest: RemovalRecordsIntegrity.program().hash(),
            program_digest: Digest::new([
                BFieldElement::new(54),
                BFieldElement::new(55),
                BFieldElement::new(56),
                BFieldElement::new(57),
                BFieldElement::new(58),
            ]),
            input: self.kernel_mast_hash.reversed().encode(),
            output: self.salted_inputs_hash.encode(),
        };
    }

    pub fn kernel_to_outputs_claim(&self) -> Claim {
        return Claim {
            program_digest: Digest::new([
                BFieldElement::new(154),
                BFieldElement::new(155),
                BFieldElement::new(156),
                BFieldElement::new(157),
                BFieldElement::new(158),
            ]),
            input: self.kernel_mast_hash.reversed().encode(),
            output: self.salted_outputs_hash.encode(),
        };
    }

    pub fn collect_lock_scripts_claim(&self) -> Claim {
        let mut lock_script_hashes_as_output: Vec<BFieldElement> = Vec::<BFieldElement>::default();
        {
            let mut i: usize = 0;
            while i < self.lock_script_hashes.len() {
                let digest: Digest = self.lock_script_hashes[i];
                let Digest([d0, d1, d2, d3, d4]) = digest;
                lock_script_hashes_as_output.push(d0);
                lock_script_hashes_as_output.push(d1);
                lock_script_hashes_as_output.push(d2);
                lock_script_hashes_as_output.push(d3);
                lock_script_hashes_as_output.push(d4);
                i += 1;
            }
        }
        return Claim {
            program_digest: Digest::new([
                BFieldElement::new(254),
                BFieldElement::new(255),
                BFieldElement::new(256),
                BFieldElement::new(257),
                BFieldElement::new(258),
            ]),
            input: self.salted_inputs_hash.reversed().encode(),
            output: lock_script_hashes_as_output,
        };
    }
}

fn main() {
    // Initialize memory with a ProofCollection
    fn print_claim(claim: &Claim) {
        tasm::tasmlib_io_write_to_stdout___digest(claim.program_digest);

        {
            let mut i: usize = 0;
            while i < claim.input.len() {
                tasm::tasmlib_io_write_to_stdout___bfe(claim.input[i]);
                i += 1;
            }
        }

        {
            let mut i: usize = 0;
            while i < claim.output.len() {
                tasm::tasmlib_io_write_to_stdout___bfe(claim.output[i]);
                i += 1;
            }
        }

        return;
    }

    let proof_collection: Box<ProofCollection> =
        ProofCollection::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();

    tasm::tasmlib_io_write_to_stdout___digest(proof_collection.kernel_mast_hash);

    let rri_claim: Box<Claim> =
        Box::<Claim>::new(proof_collection.removal_records_integrity_claim());
    print_claim(&rri_claim);

    let k2o_claim: Box<Claim> = Box::<Claim>::new(proof_collection.kernel_to_outputs_claim());
    print_claim(&k2o_claim);

    let cls_claim: Box<Claim> = Box::<Claim>::new(proof_collection.collect_lock_scripts_claim());
    print_claim(&cls_claim);

    return;
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use rand::random;
    use tasm_lib::triton_vm::prelude::NonDeterminism;
    use tasm_lib::twenty_first::bfe;
    use tasm_lib::twenty_first::math::other::random_elements;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn construct_single_proof_claims() {
        let a_pc = ProofCollection {
            removal_records_integrity: Proof(random_elements(10)),
            collect_lock_scripts: Proof(random_elements(11)),
            lock_scripts_halt: vec![Proof(random_elements(12)), Proof(random_elements(13))],
            kernel_to_outputs: Proof(random_elements(14)),
            collect_type_scripts: Proof(random_elements(15)),
            type_scripts_halt: vec![Proof(random_elements(16))],
            lock_script_hashes: random_elements(3),
            type_script_hashes: random_elements(4),
            kernel_mast_hash: random(),
            salted_inputs_hash: random(),
            salted_outputs_hash: random(),
        };
        let memory: HashMap<_, _> = (0..).map(|i| bfe!(i)).zip(a_pc.encode()).collect();
        let non_determinism = NonDeterminism::default().with_ram(memory);
        let native_output = rust_shadows::wrap_main_with_io(&main)(vec![], non_determinism.clone());

        let entrypoint =
            EntrypointLocation::disk("neptune_consensus", "claims/single_proof_claims", "main");
        let vm_output = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(non_determinism)
            .execute()
            .unwrap();

        assert_eq!(native_output, vm_output.public_output);
    }
}
