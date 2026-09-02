//! Block-level scope stack. Walks alongside the AST, recording bindings and facts
//! into the [`TypedBlock`]s that outlive the traversal.

use crate::solver::count::Count;
use crate::solver::z3::Constraint;

use super::ty::Type;
use super::typed::{Binding, BlockId, BlockKind, TypedBlock};

pub struct Frame {
    pub block: BlockId,
    pub yields: Count,
    pub can_return: bool,
    pub always_returns: bool,
}

pub struct Scope {
    blocks: Vec<TypedBlock>,
    stack: Vec<Frame>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            blocks: vec![],
            stack: vec![],
        }
    }

    pub fn push(&mut self, kind: BlockKind) -> BlockId {
        let block = self.blocks.len();
        self.blocks.push(TypedBlock {
            kind,
            parent: self.stack.last().map(|f| f.block),
            bindings: vec![],
            facts: vec![],
        });
        self.stack.push(Frame {
            block,
            yields: Count::zero(),
            can_return: false,
            always_returns: false,
        });
        block
    }

    pub fn pop(&mut self) -> Frame {
        self.stack.pop().expect("popped empty scope")
    }

    pub fn current(&self) -> BlockId {
        self.stack.last().expect("no open block").block
    }

    pub fn insert(&mut self, name: &str, ty: Type, mutable: bool) {
        let block = self.current();
        self.blocks[block].bindings.push(Binding {
            name: name.to_string(),
            ty,
            mutable,
        });
    }

    pub fn get(&self, name: &str) -> Option<&Binding> {
        self.stack
            .iter()
            .rev()
            .find_map(|f| self.blocks[f.block].binding(name))
    }

    pub fn add_fact(&mut self, fact: Constraint) {
        let block = self.current();
        self.blocks[block].facts.push(fact);
    }

    // outermost first
    pub fn facts(&self) -> Vec<Constraint> {
        self.stack
            .iter()
            .flat_map(|f| self.blocks[f.block].facts.iter().cloned())
            .collect()
    }

    pub fn add_yields(&mut self, count: &Count) {
        if let Some(frame) = self.stack.last_mut() {
            frame.yields = frame.yields.add(count);
        }
    }

    pub fn set_can_return(&mut self) {
        if let Some(frame) = self.stack.last_mut() {
            frame.can_return = true;
        }
    }

    pub fn set_always_returns(&mut self) {
        if let Some(frame) = self.stack.last_mut() {
            frame.can_return = true;
            frame.always_returns = true;
        }
    }

    pub fn always_returns(&self) -> bool {
        self.stack.last().is_some_and(|f| f.always_returns)
    }

    pub fn into_blocks(self) -> Vec<TypedBlock> {
        self.blocks
    }
}
