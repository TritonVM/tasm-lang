use tasm_lib::prelude::TasmObject;
use tasm_lib::triton_vm::prelude::BFieldCodec;
use tasm_lib::triton_vm::prelude::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::twenty_first::prelude::*;

#[derive(Debug, Clone, BFieldCodec)]
pub struct Proof(pub Vec<BFieldElement>);

#[derive(Clone, Debug, BFieldCodec, TasmObject)]
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

#[derive(Debug, Clone, BFieldCodec)]
pub enum SingleProofWitness {
    Collection(ProofCollection),
    // Update(Box<SingleProofWitness>),
    // Merger(MergerWitness)

    // Wait for Hard Fork One:
    // IntegralMempool(IntegralMempoolMembershipWitness)
}

#[derive(Debug, Clone)]
pub struct SingleProof;

impl SingleProof {
    fn source(&self) {
        let txk_digest: Digest = tasm::tasmlib_io_read_stdin___digest();
        let start_address: BFieldElement = BFieldElement::new(0);
        let spw: Box<SingleProofWitness> =
            SingleProofWitness::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();

        match spw.as_ref() {
            SingleProofWitness::Collection(pc) => {
                assert_eq!(txk_digest, pc.kernel_mast_hash);

                let removal_records_integrity_claim: Claim = pc.removal_records_integrity_claim();
                tasm::verify_stark(
                    Stark::default(),
                    &removal_records_integrity_claim,
                    &pc.removal_records_integrity,
                );

                let kernel_to_outputs_claim: Claim = pc.kernel_to_outputs_claim();
                tasm::verify_stark(
                    Stark::default(),
                    &kernel_to_outputs_claim,
                    &pc.kernel_to_outputs,
                );

                let collect_lock_scripts_claim: Claim = pc.collect_lock_scripts_claim();
                tasm::verify_stark(
                    Stark::default(),
                    &collect_lock_scripts_claim,
                    &pc.collect_lock_scripts,
                );

                let collect_type_scripts_claim: Claim = pc.collect_type_scripts_claim();
                tasm::verify_stark(
                    Stark::default(),
                    &collect_type_scripts_claim,
                    &pc.collect_type_scripts,
                );

                // let mut i = 0;
                // let lock_script_claims: Vec<Claim> = pc.lock_script_claims();
                // assert_eq!(lock_script_claims.len(), pc.lock_script_hashes.len());
                // while i < pc.lock_script_hashes.len() {
                //     let claim: &Claim = &lock_script_claims[i];
                //     let lock_script_halts_proof: &Proof = &pc.lock_scripts_halt[i];
                //     tasmlib::verify_stark(Stark::default(), claim, lock_script_halts_proof);

                //     i += 1;
                // }

                // i = 0;
                // let type_script_claims = pc.type_script_claims();
                // assert_eq!(type_script_claims.len(), pc.type_script_hashes.len());
                // while i < pc.type_script_hashes.len() {
                //     let claim: &Claim = &type_script_claims[i];
                //     let type_script_halts_proof: &Proof = &pc.type_scripts_halt[i];
                //     tasmlib::verify_stark(Stark::default(), claim, type_script_halts_proof);
                //     i += 1;
                // }
            } // SingleProofWitness::Update(_) => todo!(),
        }
    }
}
