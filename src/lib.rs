#![feature(let_chains)]
#![feature(hash_drain_filter)]
#![feature(box_patterns)]

#[macro_use]
extern crate lalrpop_util;

mod error;
mod lexer;
mod scope;

pub mod stdlib;

pub mod interpreter;
pub mod parser;
pub mod typechecker;

pub mod wasm;
