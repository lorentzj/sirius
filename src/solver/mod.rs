//! Some symbolic computation utilities based on the [`Poly`](crate::solver::poly::Poly) abstraction.
//!
//! All operations are exact. Coefficients are stored as `i128`s and panic on overflow.

pub mod count;
pub mod poly;
pub mod z3;
