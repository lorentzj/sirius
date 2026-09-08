mod annotation;
mod check;
mod coerce;
mod expr;
mod scope;
mod sig;
mod ty;
mod typed;
mod yields;

#[cfg(test)]
mod tests;

use crate::error::Errors;
use crate::parser::ParserOutput;
use crate::solver::Solver;
pub use ty::Type;

pub fn check(parse: &ParserOutput, solver: &mut Solver) -> Errors {
    check::check_file(parse.tree.as_ref().unwrap_or(&vec![]), solver)
}
