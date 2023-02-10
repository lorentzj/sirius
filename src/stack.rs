use std::collections::HashMap;

pub trait FrameTrait<T> {
    fn new() -> Self;
    fn get(&self, key: &str) -> Option<&T>;
    fn contains(&self, key: &str) -> bool;
    fn insert(&mut self, key: String, value: T);
    fn push_scope(&mut self);
    fn pop_scope(&mut self);
}

pub struct Frame<T>(Vec<HashMap<String, T>>);
pub type Stack<T> = Vec<Frame<T>>;

impl<T: Clone> FrameTrait<T> for Frame<T> {
    fn new() -> Self {
        Frame::<T>(vec![])
    }

    fn get(&self, key: &str) -> Option<&T> {
        for scope in self.0.iter().rev() {
            if let Some(t) = scope.get(key) {
                return Some(t);
            }
        }
        None
    }

    fn contains(&self, key: &str) -> bool {
        for scope in &self.0 {
            if scope.contains_key(key) {
                return true;
            }
        }
        false
    }

    fn insert(&mut self, key: String, value: T) {
        self.0.last_mut().unwrap().insert(key, value);
    }

    fn push_scope(&mut self) {
        self.0.push(HashMap::default());
    }

    fn pop_scope(&mut self) {
        self.0.pop();
    }
}
