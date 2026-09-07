//! Some symbolic computation utilities based on the [`Poly`] abstraction.
//!
//! All operations are exact. Coefficients are stored as `i128`s and panic on overflow.

pub mod count;
pub mod poly;

pub mod t0;
pub mod t1;
pub mod t2;

use js_sys::Function;
use std::fmt;
use std::path::PathBuf;

use crate::parser::lexer::ArithCmpOp;
use poly::{Poly, Var};

use t1::{Linearized, Z3};

/// Obligation vocabulary.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Cmp {
    Eq,
    Ne,
    Le,
    Lt,
    Ge,
    Gt,
}

impl Cmp {
    pub fn negate(self) -> Cmp {
        match self {
            Cmp::Eq => Cmp::Ne,
            Cmp::Ne => Cmp::Eq,
            Cmp::Le => Cmp::Gt,
            Cmp::Lt => Cmp::Ge,
            Cmp::Ge => Cmp::Lt,
            Cmp::Gt => Cmp::Le,
        }
    }

    fn smt(self) -> &'static str {
        match self {
            Cmp::Eq => "=",
            Cmp::Ne => "distinct",
            Cmp::Le => "<=",
            Cmp::Lt => "<",
            Cmp::Ge => ">=",
            Cmp::Gt => ">",
        }
    }

    pub fn from_lex(op: &ArithCmpOp) -> Self {
        match op {
            ArithCmpOp::Greater => Cmp::Gt,
            ArithCmpOp::GreaterOrEq => Cmp::Ge,
            ArithCmpOp::Less => Cmp::Lt,
            ArithCmpOp::LessOrEq => Cmp::Le,
            ArithCmpOp::Eq => Cmp::Eq,
            ArithCmpOp::NotEq => Cmp::Ne,
        }
    }
}

impl fmt::Display for Cmp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Cmp::Eq => "==",
            Cmp::Ne => "!=",
            Cmp::Le => "<=",
            Cmp::Lt => "<",
            Cmp::Ge => ">=",
            Cmp::Gt => ">",
        })
    }
}

/// Format for facts and goals, e.g. `X > 2*Y`.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Constraint {
    pub lhs: Poly,
    pub cmp: Cmp,
    pub rhs: Poly,
}

impl Constraint {
    pub fn new(lhs: Poly, cmp: Cmp, rhs: Poly) -> Self {
        Self { lhs, cmp, rhs }
    }

    pub fn display_with(&self, names: &[&str]) -> String {
        format!(
            "{} {} {}",
            self.lhs.display_with(Some(names)),
            self.cmp,
            self.rhs.display_with(Some(names))
        )
    }

    pub fn negate(&self) -> Self {
        Self {
            lhs: self.lhs.clone(),
            cmp: self.cmp.negate(),
            rhs: self.rhs.clone(),
        }
    }
}

pub type Point = Vec<(Var, i128)>;

/// Solver response.
/// [`t0`] refutations provide a trivial `!goal` given non-negative variables;
/// [`t1`] refutations provide a counterexample point;
/// [`t2`] is best-effort and cannot refute.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Verdict {
    Proved,
    RefutedBy(Constraint),
    RefutedAt(Point),
    Unknown,
}

/// A consolidated [`t0`] -> [`t1`] -> [`t2`] solving pipeline.
pub struct Solver {
    pub z3: Z3,
}

impl Solver {
    /// Expect the z3 command to be available in the environment.
    /// Optionally provide a filesystem cache for model output.
    pub fn new_cli(cache_dir: Option<PathBuf>) -> Option<Self> {
        Some(Self {
            z3: Z3::new_cli(cache_dir)?,
        })
    }

    /// Pass a javascript callback string->string for smt2 input.
    pub fn new_wasm(callback: Function) -> Self {
        Self {
            z3: Z3::new_wasm(callback),
        }
    }

    pub fn prove(
        &mut self,
        facts: &[Constraint],
        goal: &Constraint,
        names: &[String],
        nonneg: &[Var],
    ) -> Verdict {
        let lin = Linearized::new(facts, goal, names, &|v| nonneg.contains(&v));

        let verdict = {
            if facts.contains(goal)
                || t0::prove(goal.cmp, &goal.lhs, &goal.rhs, &|v| nonneg.contains(&v))
            {
                Verdict::Proved
            } else {
                if facts.contains(&goal.negate())
                    || t0::prove(goal.cmp.negate(), &goal.lhs, &goal.rhs, &|v| {
                        nonneg.contains(&v)
                    })
                {
                    return Verdict::RefutedBy(goal.negate());
                }

                let lin_names: Vec<&str> = lin.names.iter().map(String::as_str).collect();

                let z3_lia_verdict =
                    self.z3
                        .entails_lia(&lin.facts, &lin.goal, &lin.nonneg, &lin_names);

                match (z3_lia_verdict, lin.pure_linear) {
                    (Verdict::Proved, _) => Verdict::Proved,
                    (Verdict::RefutedAt(p), true) => Verdict::RefutedAt(p),
                    _ => Verdict::Unknown,
                }
            }
        };

        // atom abstraction discards how the atoms relate to their factors, so a nonlinear goal it
        // could not settle gets one more chance at a positivity certificate
        if verdict != Verdict::Proved
            && !lin.pure_linear
            && t2::prove(&mut self.z3, facts, goal, nonneg)
        {
            return Verdict::Proved;
        }

        verdict
    }
}
