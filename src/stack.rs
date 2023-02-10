use std::collections::HashMap;

pub struct Frame<T>(Vec<HashMap<String, T>>);
pub type Stack<T> = Vec<Frame<T>>;

impl<T> Frame<T> {
    pub fn new() -> Self {
        Frame::<T>(vec![])
    }

    pub fn get(&self, key: &str) -> Option<&T> {
        for scope in self.0.iter().rev() {
            if let Some(t) = scope.get(key) {
                return Some(t);
            }
        }
        None
    }

    pub fn contains(&self, key: &str) -> bool {
        for scope in &self.0 {
            if scope.contains_key(key) {
                return true;
            }
        }
        false
    }

    pub fn insert(&mut self, key: String, value: T) {
        self.0.last_mut().unwrap().insert(key, value);
    }

    pub fn push_scope(&mut self) {
        self.0.push(HashMap::default());
    }

    pub fn pop_scope(&mut self) {
        self.0.pop();
    }
}
