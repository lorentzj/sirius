use std::collections::HashMap;

pub struct Scope<T>(Vec<HashMap<String, T>>);

impl<T> std::default::Default for Scope<T> {
    fn default() -> Self {
        Scope::<T>(vec![])
    }
}

impl<T> Scope<T> {
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

    pub fn assign(&mut self, key: &str, value: T) {
        for scope in self.0.iter_mut().rev() {
            if let Some(t) = scope.get_mut(key) {
                *t = value;
                break;
            }
        }
    }

    pub fn push(&mut self) {
        self.0.push(HashMap::default());
    }

    pub fn pop(&mut self) {
        self.0.pop();
    }

    pub fn is_global(&self, key: &str) -> bool {
        for (i, scope) in self.0.iter().enumerate().rev() {
            if scope.contains_key(key) {
                return i == 0;
            }
        }
        false
    }
}
