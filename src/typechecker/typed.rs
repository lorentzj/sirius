//! For tools/testing

#![allow(dead_code)]

use std::collections::HashMap;

use crate::solver::count::Count;
use crate::solver::z3::Constraint;

use super::sig::FnSig;
use super::ty::Type;

pub type BlockId = usize;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BlockKind {
    /// A function body; always block 0 of a [`TypedFn`].
    Body,
    If,
    Else,
    For,
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub name: String,
    pub ty: Type,
    pub mutable: bool,
}

#[derive(Debug)]
pub struct TypedBlock {
    pub kind: BlockKind,
    pub parent: Option<BlockId>,
    /// In source order; a name may appear twice if it is shadowed.
    pub bindings: Vec<Binding>,
    /// Facts the block adds to its enclosing scope (loop bounds, `if` conditions).
    pub facts: Vec<Constraint>,
}

impl TypedBlock {
    pub fn binding(&self, name: &str) -> Option<&Binding> {
        self.bindings.iter().rev().find(|b| b.name == name)
    }
}

#[derive(Debug)]
pub struct TypedFn {
    pub name: String,
    pub sig: FnSig,
    /// [`Var`](crate::solver::poly::Var) index to display name: typevars, then loop iterators.
    pub vars: Vec<String>,
    pub blocks: Vec<TypedBlock>,
    /// Total `yield`s over the whole body.
    pub yields: Count,
    /// Token span to type, for every expression the checker visited.
    pub exprs: HashMap<(usize, usize), Type>,
}

impl TypedFn {
    pub fn names(&self) -> Vec<&str> {
        self.vars.iter().map(String::as_str).collect()
    }

    pub fn block(&self, block: BlockId) -> &TypedBlock {
        &self.blocks[block]
    }

    /// Resolve `name` as seen from `block`, walking out through enclosing blocks.
    pub fn binding(&self, block: BlockId, name: &str) -> Option<&Binding> {
        let mut curr = Some(block);
        while let Some(id) = curr {
            if let Some(b) = self.blocks[id].binding(name) {
                return Some(b);
            }
            curr = self.blocks[id].parent;
        }
        None
    }

    pub fn var(&self, block: BlockId, name: &str) -> Option<&Type> {
        self.binding(block, name).map(|b| &b.ty)
    }

    pub fn render_var(&self, block: BlockId, name: &str) -> Option<String> {
        self.var(block, name).map(|t| t.render(&self.names()))
    }

    pub fn expr(&self, start: usize, end: usize) -> Option<&Type> {
        self.exprs.get(&(start, end))
    }

    pub fn children(&self, block: BlockId) -> Vec<BlockId> {
        (0..self.blocks.len())
            .filter(|&i| self.blocks[i].parent == Some(block))
            .collect()
    }

    /// Blocks of a given kind, in source order.
    pub fn blocks_of(&self, kind: BlockKind) -> Vec<BlockId> {
        (0..self.blocks.len())
            .filter(|&i| self.blocks[i].kind == kind)
            .collect()
    }

    /// Facts in scope at `block`, innermost last.
    pub fn facts(&self, block: BlockId) -> Vec<&Constraint> {
        let mut chain = vec![];
        let mut curr = Some(block);
        while let Some(id) = curr {
            chain.push(id);
            curr = self.blocks[id].parent;
        }
        chain
            .into_iter()
            .rev()
            .flat_map(|id| self.blocks[id].facts.iter())
            .collect()
    }
}

#[derive(Debug, Default)]
pub struct TypedAst {
    pub fns: Vec<TypedFn>,
}

impl TypedAst {
    pub fn get(&self, name: &str) -> Option<&TypedFn> {
        self.fns.iter().find(|f| f.name == name)
    }

    /// Type of `name` in block `block` of function `f`, rendered with that function's var names.
    pub fn render_var(&self, f: &str, block: BlockId, name: &str) -> Option<String> {
        self.get(f)?.render_var(block, name)
    }
}
