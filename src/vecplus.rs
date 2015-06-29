use std::ops::Deref;

/// A `VecPlus` is a subset of `Vec` that's guaranteed never to be empty.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct VecPlus<T> {
    value: Vec<T>
}

impl<T> Into<Vec<T>> for VecPlus<T> {
    fn into(self) -> Vec<T> {
        self.value
    }
}

impl<T> Deref for VecPlus<T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        &self.value
    }
}

impl<T> VecPlus<T> {
    pub fn from_vec(value: Vec<T>) -> Option<VecPlus<T>> {
        if value.is_empty() {
            None
        } else {
            Some(VecPlus {
                value: value
            })
        }
    }
}
