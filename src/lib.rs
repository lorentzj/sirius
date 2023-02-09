#![feature(let_chains)]

#[macro_use]
extern crate lalrpop_util;

pub mod error;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod typechecker;
pub mod wasm;
