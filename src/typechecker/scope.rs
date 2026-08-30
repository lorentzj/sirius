use super::Type;
use crate::solver::poly::{Poly, Var};
use std::collections::HashMap;

pub struct BlockScope {
    pub yield_count: Poly,
    pub can_return: bool,
    pub always_returns: bool,
    vars: HashMap<String, BlockScopeEntry>,
}

pub struct BlockScopeEntry {
    pub t: Type,
    pub mutable: bool,
}

pub struct Scopes {
    stack: Vec<BlockScope>,
    curr_fresh_typevar: Var,
}

impl Scopes {
    pub fn new() -> Self {
        Scopes {
            stack: vec![],
            curr_fresh_typevar: 0,
        }
    }

    pub fn get(&self, key: &str) -> Option<&BlockScopeEntry> {
        for block in self.stack.iter().rev() {
            if let Some(t) = block.vars.get(key) {
                return Some(t);
            }
        }
        None
    }

    pub fn _get_mut(&mut self, key: &str) -> Option<&mut BlockScopeEntry> {
        for block in self.stack.iter_mut().rev() {
            if let Some(t) = block.vars.get_mut(key) {
                return Some(t);
            }
        }
        None
    }

    pub fn _contains(&self, key: &str) -> bool {
        for block in &self.stack {
            if block.vars.contains_key(key) {
                return true;
            }
        }
        false
    }

    pub fn insert<S: ToString>(&mut self, key: S, t: Type, mutable: bool) {
        if let Some(block) = self.stack.last_mut() {
            block
                .vars
                .insert(key.to_string(), BlockScopeEntry { t, mutable });
        }
    }

    pub fn push(&mut self) {
        self.stack.push(BlockScope {
            yield_count: Poly::zero(),
            can_return: false,
            always_returns: false,
            vars: HashMap::default(),
        });
    }

    pub fn peek(&self) -> Option<&'_ BlockScope> {
        if !self.stack.is_empty() {
            Some(&self.stack[self.stack.len() - 1])
        } else {
            None
        }
    }

    pub fn pop(&mut self) -> Option<BlockScope> {
        if self.stack.len() > 1 {
            self.stack.pop()
        } else {
            None
        }
    }

    pub fn add_yields(&mut self, p: &Poly) {
        if let Some(block) = self.stack.last_mut() {
            block.yield_count = block.yield_count.add(p);
        }
    }

    pub fn set_can_return(&mut self) {
        if let Some(block) = self.stack.last_mut() {
            block.can_return = true;
        }
    }

    pub fn set_always_returns(&mut self) {
        if let Some(block) = self.stack.last_mut() {
            block.can_return = true;
            block.always_returns = true;
        }
    }

    pub fn n_typevars(&mut self, n: u32) {
        self.curr_fresh_typevar = n;
    }

    pub fn _get_fresh_typevar(&mut self) -> Var {
        self.curr_fresh_typevar += 1;
        self.curr_fresh_typevar - 1
    }

    pub fn _is_global(&self, key: &str) -> bool {
        // check in reverse order to see if later element shadowed global
        for (i, block) in self.stack.iter().enumerate().rev() {
            if block.vars.contains_key(key) {
                return i == 0;
            }
        }
        false
    }
}
