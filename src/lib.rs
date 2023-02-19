#![feature(let_chains)]
#![feature(hash_drain_filter)]
#![feature(box_patterns)]

#[macro_use]
extern crate lalrpop_util;

pub mod error;
pub mod flow;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod stack;
pub mod stdlib;
pub mod typechecker;
pub mod wasm;
