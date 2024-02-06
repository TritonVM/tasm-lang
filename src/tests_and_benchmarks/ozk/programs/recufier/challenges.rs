use tasm_lib::twenty_first::shared_math::x_field_element::XFieldElement;

#[derive(Debug, Clone, Arbitrary)]
pub struct Challenges {
    pub challenges: [XFieldElement; 63],
}

impl Challenges {
    pub fn new(mut challenges: Vec<XFieldElement>) -> Self {
        assert_eq!(63 - 4, challenges.len());

        let compressed_digest = EvalArg::compute_terminal(
            &claim.program_digest.values(),
            EvalArg::default_initial(),
            challenges[CompressProgramDigestIndeterminate.index()],
        );
        let lookup_terminal = EvalArg::compute_terminal(
            &tip5::LOOKUP_TABLE.map(|i| BFieldElement::new(i as u64)),
            EvalArg::default_initial(),
            challenges[LookupTablePublicIndeterminate.index()],
        );

        challenges.insert(StandardInputTerminal.index(), input_terminal);
        challenges.insert(StandardOutputTerminal.index(), output_terminal);
        challenges.insert(LookupTablePublicTerminal.index(), lookup_terminal);
        challenges.insert(CompressedProgramDigest.index(), compressed_digest);
        assert_eq!(Self::count(), challenges.len());
        let challenges = challenges.try_into().unwrap();

        Self { challenges }
    }
}
