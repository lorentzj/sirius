//! Block-level scope stack. Walks alongside the AST, recording bindings and facts
//! into the [`TypedBlock`]s that outlive the traversal.

use super::yields::Yields;
use crate::solver::Constraint;

use super::ty::Type;
use super::typed::{Binding, BindingId, BlockId, BlockKind, Fact, TypedBlock};

pub struct Frame {
    pub block: BlockId,
    pub yields: Yields,
    pub can_return: bool,
    pub always_returns: bool,
}

pub struct Scope {
    blocks: Vec<TypedBlock>,
    stack: Vec<Frame>,
    binding_id: BindingId,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            blocks: vec![],
            stack: vec![],
            binding_id: 0,
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
            yields: Yields::zero(),
            can_return: false,
            always_returns: false,
        });
        block
    }

    pub fn frame(&self) -> &Frame {
        self.stack.last().expect("no open block")
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
            id: self.binding_id,
            name: name.to_string(),
            ty,
            mutable,
        });
        self.binding_id += 1;
    }

    pub fn get(&self, name: &str) -> Option<&Binding> {
        self.stack
            .iter()
            .rev()
            .find_map(|f| self.blocks[f.block].binding(name))
    }

    pub fn add_fact(&mut self, fact: Fact) {
        let block = self.current();
        self.blocks[block].facts.push(fact);
    }

    pub fn add_constraint(&mut self, c: Constraint) {
        self.add_fact(Fact::Constraint(c));
    }

    // outermost first
    pub fn constraints(&self) -> Vec<Constraint> {
        self.stack
            .iter()
            .flat_map(|f| self.blocks[f.block].facts.iter().cloned())
            .filter_map(|fact| match fact {
                Fact::Constraint(c) => Some(c),
                _ => None,
            })
            .collect()
    }

    // outermost first
    pub fn not_nulls(&self) -> Vec<BindingId> {
        self.stack
            .iter()
            .flat_map(|f| self.blocks[f.block].facts.iter().cloned())
            .filter_map(|fact| match fact {
                Fact::NotNull(b) => Some(b),
                _ => None,
            })
            .collect()
    }

    // outermost first
    pub fn is_nulls(&self) -> Vec<BindingId> {
        self.stack
            .iter()
            .flat_map(|f| self.blocks[f.block].facts.iter().cloned())
            .filter_map(|fact| match fact {
                Fact::IsNull(b) => Some(b),
                _ => None,
            })
            .collect()
    }

    pub fn add_yields(&mut self, count: &Yields) {
        if let Some(frame) = self.stack.last_mut() {
            frame.yields = frame.yields.then(count);
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
