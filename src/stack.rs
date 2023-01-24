use itertools::Itertools;

#[derive(Debug, Clone)]
pub struct Stack<T: Eq> {
    pub inner: Vec<T>,
}

impl<T: Eq> Default for Stack<T> {
    fn default() -> Self {
        let inner = Vec::default();
        Self { inner }
    }
}

impl<T: Eq> Stack<T> {
    pub fn push(&mut self, elem: T) {
        self.inner.push(elem);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.inner.pop()
    }

    pub fn peek(&self) -> Option<&T> {
        self.inner.last()
    }

    pub fn remove_value(&mut self, seek_value: &T) {
        let index_to_remove = self
            .inner
            .iter()
            .find_position(|found_value| seek_value == *found_value)
            .expect("Value must be present when removing from stack")
            .0;

        self.inner.remove(index_to_remove);
    }
}
