use serde::Serialize;

use crate::error::{Error, ErrorType};
use rational::Rat;

pub mod field;
pub mod interval;
pub mod poly;
pub mod rational;
pub mod system;
pub mod univariate;

#[derive(Serialize, Clone, PartialEq, Eq, Debug)]
pub enum Truth {
    True,
    False,
    Undetermined,
}

impl Truth {
    pub fn or(&self, other: Truth) -> Truth {
        match self {
            Truth::True => Truth::True,
            Truth::Undetermined => match other {
                Truth::True => Truth::True,
                _ => Truth::Undetermined,
            },
            Truth::False => other,
        }
    }

    pub fn and(&self, other: Truth) -> Truth {
        match self {
            Truth::True => other,
            Truth::Undetermined => match other {
                Truth::False => Truth::False,
                _ => Truth::Undetermined,
            },
            Truth::False => Truth::False,
        }
    }

    pub fn is_true(&self) -> bool {
        matches! {self, Truth::True}
    }

    pub fn is_false(&self) -> bool {
        matches! {self, Truth::False}
    }

    pub fn is_undetermined(&self) -> bool {
        matches! {self, Truth::Undetermined}
    }
}

pub(crate) type Poly = poly::Poly<Rat>;

#[allow(clippy::enum_variant_names)]
#[derive(Serialize, Clone, PartialEq, Eq)]
enum C {
    Eq(Poly, Poly),
    NEq(Poly, Poly),
    GtEq(Poly, Poly),
    Gt(Poly, Poly),
}

impl std::fmt::Debug for C {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            C::Eq(a, b) => write!(f, "{a:?} = {b:?}"),
            C::NEq(a, b) => write!(f, "{a:?} != {b:?}"),
            C::GtEq(a, b) => write!(f, "{a:?} >= {b:?}"),
            C::Gt(a, b) => write!(f, "{a:?} > {b:?}"),
        }
    }
}

#[derive(Serialize, Clone, PartialEq)]
pub struct Constraint {
    data: C,
    pub start: usize,
    pub end: usize,
}

impl std::fmt::Debug for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.data)
    }
}

impl Constraint {
    pub fn new_neq(a: Poly, b: Poly) -> Self {
        Constraint {
            start: 0,
            data: C::NEq(a, b),
            end: 0,
        }
    }

    pub fn new_eq(a: Poly, b: Poly) -> Self {
        Constraint {
            start: 0,
            data: C::Eq(a, b),
            end: 0,
        }
    }

    pub fn new_gt_eq(a: Poly, b: Poly) -> Self {
        Constraint {
            start: 0,
            data: C::GtEq(a, b),
            end: 0,
        }
    }

    pub fn new_gt(a: Poly, b: Poly) -> Self {
        Constraint {
            start: 0,
            data: C::Gt(a, b),
            end: 0,
        }
    }

    pub fn apply_pos(cs: &mut Vec<Self>, start: usize, end: usize) {
        for c in cs {
            c.start = start;
            c.end = end;
        }
    }

    pub fn negate(&self) -> Constraint {
        Constraint {
            start: self.start,
            data: match &self.data {
                C::Eq(a, b) => C::NEq(a.clone(), b.clone()),
                C::NEq(a, b) => C::Eq(a.clone(), b.clone()),
                C::Gt(a, b) => C::GtEq(b.clone(), a.clone()),
                C::GtEq(a, b) => C::Gt(b.clone(), a.clone()),
            },
            end: self.end,
        }
    }

    pub fn get_rel_zero(&self) -> Poly {
        match &self.data {
            C::Eq(a, b) => a.clone() - b.clone(),
            C::NEq(a, b) => a.clone() - b.clone(),
            C::Gt(a, b) => a.clone() - b.clone(),
            C::GtEq(a, b) => a.clone() - b.clone(),
        }
    }
}

pub fn solve(preconditions: &[Constraint], postconditions: &[Constraint]) -> Vec<Error> {
    let mut postconditions = system::System::new(preconditions.iter().chain(postconditions.iter()));
    let mut preconditions = system::System::new(preconditions.iter());

    let pre_errors = preconditions.check();

    if !pre_errors.is_empty() {
        return pre_errors;
    }

    let post_errors = postconditions.check();

    if !post_errors.is_empty() {
        return post_errors;
    }

    preconditions.groebner_basis();
    postconditions.groebner_basis();

    let preconditions_dim = preconditions.groebner_basis();
    let postconditions_dim = postconditions.groebner_basis();

    let additional_degs_of_freedom = postconditions
        .free_eq_vars()
        .difference(&preconditions.free_eq_vars())
        .count();

    if postconditions_dim - preconditions_dim > additional_degs_of_freedom {
        let preconditions_eqs = preconditions.gen_eq_zs();
        let overdetermined_eqs_errs: Vec<_> = postconditions
            .0
            .iter()
            .filter(|r| r.data.1 == system::ToZero::Eq && !preconditions_eqs.contains(&r.data.0))
            .map(|r| {
                if r.check() == Truth::False {
                    Error::new(
                        ErrorType::Constraint,
                        format!(
                            "cannot satisfy constraint \"{:?} {:?}\"",
                            r.data, r.provenance
                        ),
                        r.provenance[0].start,
                        r.provenance[0].end,
                    )
                } else {
                    Error::new(
                        ErrorType::Constraint,
                        format!(
                            "overdetermined system: cannot satisfy constraint \"{:?} {:?}\"",
                            r.data, r.provenance
                        ),
                        r.provenance[0].start,
                        r.provenance[0].end,
                    )
                }
            })
            .collect();

        overdetermined_eqs_errs
    } else {
        vec![]
    }
}
