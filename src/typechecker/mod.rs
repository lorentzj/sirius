mod annotation;
mod check;
mod coerce;
mod expr;
mod scope;
mod sig;
mod t0;
mod t1;
mod ty;
mod typed;

#[cfg(test)]
mod tests;

use std::path::PathBuf;

use crate::error::{Error, error_at};
use crate::parser::{ParserOutput, Pos};
use crate::solver::z3::Solver;
pub use ty::Type;

pub fn check(parse: &ParserOutput, cache_dir: Option<PathBuf>) -> Vec<Error> {
    match Solver::new(cache_dir) {
        Some(mut solver) => check::check_file(parse.tree.as_ref().unwrap_or(&vec![]), &mut solver),
        None => vec![error_at!(Type, &Pos::span(0, 0), "failed to initialize z3")],
    }
}
