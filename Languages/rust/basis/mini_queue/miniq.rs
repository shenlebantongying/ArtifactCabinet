#[derive(Debug, Clone)]
pub struct Miniq<T> {
    _v: Vec<T>,
}

impl<T: Clone + PartialOrd> Miniq<T> {
    pub fn new() -> Self {
        Self { _v: Vec::new() }
    }

    pub fn insert(&mut self, value: T) {
        if self._v.is_empty() {
            self._v.push(value);
        } else {
            let mut inserted = false;
            for i in 0..self._v.len() {
                if self._v[i] > value {
                    self._v.insert(i, value.clone());
                    inserted = true;
                    break;
                }
            }
            if !inserted {
                self._v.push(value.clone());
            }
        }
    }
}
