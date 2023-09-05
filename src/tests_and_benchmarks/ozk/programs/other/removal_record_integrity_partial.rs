use triton_vm::Digest;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

// #[derive(
//     Clone,
//     Debug,
//     Serialize,
//     Deserialize,
//     PartialEq,
//     Eq,
//     GetSize,
//     BFieldCodec,
//     FieldCount,
//     TasmObject,
// )]
// pub struct RemovalRecordsIntegrityWitness {
//     pub input_utxos: Vec<Utxo>,
//     pub membership_proofs: Vec<MsMembershipProof<Hash>>,
//     pub aocl: MmrAccumulator<Hash>,
//     pub swbfi: MmrAccumulator<Hash>,
//     pub swbfa_hash: Digest,
//     pub kernel: TransactionKernel,
// }

fn main() {
    // Read transaction kernel hash
    let _s5: Digest = tasm::tasm_io_read_stdin_digest();

    // Calling `decode` could load a struct, but from where?
    // If the struct is already present in memory, we probaly only need a pointer to it.
    // I guess, we can just assume that the struct we are loading is just placed on memory
    // pointer 1. So the `decode` function call just returns the pointer 1.

    // Once we have the struct pointer, we need to map the Rust field getters `obj.field` to
    // the `tasm_lib::field!(<Struct>::<field>)` macro. The `tasm_lib::field!` macro outputs
    // VM instructions, so a field getter can be replaced with the output of that macro.
    // Let's assume that we can get the output of that macro.
    // Then we need to *know* what type the field has, and be able to call methods on that type.
    // Some of these methods will already exist in `tasm-lib`. E.g. witness.kernel.mast_hash()
    // is present in `tasm-lib` through `library.import(Box::new(TransactionKernelMastHash));`
    // and *that* function just takes a pointer to the kernel, so with the above code in place,
    // we have support for that.
    // So to move forward, we only need to write a test that handles the most basic struct
    // and see if we can return (write to stdout) a pointer to the 2nd field of that struct.

    // If we can do that, from code that's valid Rust code, then we can handle structs well enough
    // for our current needs.

    // We should probably start testing this in isolation. So we should create a test with
    // a very simple struct and just output its field values to stdout.
    // Test procedure in VM:
    // - Rust field getters 'obj.field' maps to 'tasm_lib::field!(<Struct>::<field>)'
    // - then how do we read the field value onto the stack? We probably need
    // We can start the VM with this struct in memory but the native execution has to
    // have support for this through some helper functions. Those helper functions then probably
    // have to be called the same as

    // // 1. read and process witness data
    // let witness = *RemovalRecordsIntegrityWitness::decode(
    //     &secret_input.iter().skip(1).copied().collect_vec(),
    // )
    // .unwrap();
    // let hash_of_kernel = *Digest::decode(
    //     &public_input
    //         .individual_tokens
    //         .iter()
    //         .copied()
    //         .take(DIGEST_LENGTH)
    //         .rev()
    //         .collect_vec(),
    // )
    // .expect("Could not decode public input in Removal Records Integrity :: verify_raw");

    // // 1. read and process witness data
    // let memory_length = nondeterminism.ram.len() as u64;
    // let memory_vector = (1u64..memory_length)
    //     .map(BFieldElement::new)
    //     .map(|b| *nondeterminism.ram.get(&b).unwrap_or(&BFieldElement::new(0)))
    //     .collect_vec();
    // let witness = *RemovalRecordsIntegrityWitness::decode(&memory_vector).unwrap();

    // println!("first element of witness: {}", witness.encode()[0]);
    // println!("first element of kernel: {}", witness.kernel.encode()[0]);

    // // 2. assert that the kernel from the witness matches the hash in the public input
    // // now we can trust all data in kernel
    // assert_eq!(
    //     hash_of_kernel,
    //     witness.kernel.mast_hash(),
    //     "hash of kernel ({})\nwitness kernel ({})",
    //     hash_of_kernel,
    //     witness.kernel.mast_hash()
    // );

    // // 3. assert that the mutator set's MMRs in the witness match the kernel
    // // now we can trust all data in these MMRs as well
    // let mutator_set_hash = Hash::hash_pair(
    //     &Hash::hash_pair(&witness.aocl.bag_peaks(), &witness.swbfi.bag_peaks()),
    //     &Hash::hash_pair(&witness.swbfa_hash, &Digest::default()),
    // );
    // assert_eq!(witness.kernel.mutator_set_hash, mutator_set_hash);

    return;
}

mod tests {
    use super::*;
    use crate::tests_and_benchmarks::{
        ozk::{self, ozk_parsing},
        test_helpers::shared_test::*,
    };
    use rand::random;
    use std::collections::HashMap;
    use tasm_lib::Digest;
    use triton_vm::NonDeterminism;
    use twenty_first::shared_math::bfield_codec::BFieldCodec;

    #[test]
    fn removal_record_integrity_partial_test() {
        let input: Digest = random();
        let input = input.encode();
        let non_determinism = NonDeterminism::new(vec![]);
        let expected_output = vec![];
        let native_output =
            ozk::rust_shadows::wrap_main_with_io(&main)(input.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let (parsed, _, _) =
            ozk_parsing::parse_main_and_structs("other", "removal_record_integrity_partial");
        let expected_stack_diff = 0;
        let vm_output = execute_with_stack_memory_and_ins(
            &parsed,
            vec![],
            &mut HashMap::default(),
            input,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.output);
    }
}
