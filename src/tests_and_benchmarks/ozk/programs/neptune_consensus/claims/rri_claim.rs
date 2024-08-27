use tasm_lib::structure::tasm_object::TasmObject;
use tasm_lib::triton_vm::prelude::BFieldCodec;
use tasm_lib::triton_vm::prelude::BFieldElement;
use tasm_lib::Digest;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::triton_vm::proof::Claim;
use crate::triton_vm::proof::Proof;

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
        let claim: Claim = Claim {
            // program_digest: RemovalRecordsIntegrity.program().hash(),
            program_digest: Digest::new([
                BFieldElement::new(0),
                BFieldElement::new(0),
                BFieldElement::new(0),
                BFieldElement::new(0),
                BFieldElement::new(0),
            ]),
            // input: self.kernel_mast_hash.reversed().values().to_vec(),
            input: self.kernel_mast_hash.encode(),
            output: self.salted_inputs_hash.encode(),
        };

        return claim;
    }
}

fn main() {
    // Initialize memory with a ProofCollection
    let proof_collection: Box<ProofCollection> =
        ProofCollection::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();

    tasm::tasmlib_io_write_to_stdout___digest(proof_collection.kernel_mast_hash);

    return;
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::time::SystemTime;
    use std::time::UNIX_EPOCH;

    use itertools::Itertools;
    use tasm_lib::triton_vm::program::NonDeterminism;
    use tasm_lib::twenty_first::bfe;
    use tasm_lib::twenty_first::math::other::random_elements;
    use tasm_lib::twenty_first::util_types::merkle_tree::CpuParallel;
    use tasm_lib::twenty_first::util_types::merkle_tree::MerkleTree;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn construct_rri_claim() {
        let empty_proof = Proof(vec![]);
        let a_pc = ProofCollection {
            removal_records_integrity: empty_proof.clone(),
            collect_lock_scripts: empty_proof.clone(),
            lock_scripts_halt: vec![],
            kernel_to_outputs: empty_proof.clone(),
            collect_type_scripts: empty_proof.clone(),
            type_scripts_halt: vec![],
            lock_script_hashes: vec![],
            type_script_hashes: vec![],
            kernel_mast_hash: Digest::default(),
            salted_inputs_hash: Digest::default(),
            salted_outputs_hash: Digest::default(),
        };
        let memory: HashMap<_, _> = (0..).map(|i| bfe!(i)).zip(a_pc.encode()).collect();
        let non_determinism = NonDeterminism::default().with_ram(memory);
        let native_output = rust_shadows::wrap_main_with_io(&main)(vec![], non_determinism.clone());

        let entrypoint = EntrypointLocation::disk("neptune_consensus", "claims/rri_claim", "main");
        let vm_output = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(non_determinism)
            .execute()
            .unwrap();

        assert_eq!(native_output, vm_output.public_output);
    }
}
