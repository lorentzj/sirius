#![feature(let_chains)]
#![feature(hash_drain_filter)]
#![feature(box_patterns)]
#![feature(trait_alias)]

#[macro_use]
extern crate lalrpop_util;

pub mod error;
pub mod scope;

pub mod lexer;
pub mod parser;
pub mod solver;
pub mod typechecker;

pub mod interpreter;
pub mod stdlib;
pub mod wasm;
