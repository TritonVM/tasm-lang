use arbitrary::Arbitrary;
use tasm_lib::twenty_first::math::x_field_element::XFieldElement;

#[derive(Debug, Clone, Arbitrary)]
pub(crate) struct Challenges {
    #[allow(dead_code)]
    // Value is never read, since the position of `Challenges` in memory is statically known.
    //  Therefore, we need to ignore the linter warning here.
    pub challenges: [XFieldElement; 63],
}

impl Challenges {
    pub(crate) const fn count() -> usize {
        return 63;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::triton_vm;

    #[test]
    fn local_challenges_count_agrees_with_tvm() {
        assert_eq!(
            triton_vm::table::challenges::Challenges::COUNT,
            Challenges::count()
        );
    }
}
