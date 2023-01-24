use twenty_first::{
    shared_math::b_field_element::BFieldElement, util_types::algebraic_hasher::Hashable,
};

#[derive(Debug, Clone)]
pub struct Stack<T> {
    pub inner: Vec<T>,
}

impl<T> Default for Stack<T> {
    fn default() -> Self {
        let inner = Vec::default();
        Self { inner }
    }
}

impl<T> Stack<T> {
    pub fn push(&mut self, elem: T) {
        self.inner.push(elem);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.inner.pop()
    }

    pub fn peek(&self) -> Option<&T> {
        self.inner.last()
    }
}
