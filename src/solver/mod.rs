// with thanks to Prof. Emre Sertöz for the great lectures on Computational Algebraic Geometry on YouTube.

use serde::Serialize;

use crate::error::{Error, ErrorType};
use rational::Rat;

pub mod field;
pub mod poly;
pub mod rational;
pub mod univariate;

type Poly = poly::Poly<Rat>;

#[allow(clippy::enum_variant_names)]
#[derive(Serialize, Clone, PartialEq, Eq, Debug)]
enum C {
    EqZero(Poly),
    NEqZero(Poly),
    GtEqZero(Poly),
    GtZero(Poly),
}

#[derive(Serialize, Clone, Debug)]
pub struct Constraint {
    data: C,
    pub start: usize,
    pub end: usize,
}

impl PartialEq for Constraint {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl Constraint {
    pub fn new_neq_z(v: Poly) -> Self {
        Constraint {
            start: 0,
            data: C::NEqZero(v),
            end: 0,
        }
    }

    pub fn new_eq_z(v: Poly) -> Self {
        Constraint {
            start: 0,
            data: C::EqZero(v),
            end: 0,
        }
    }

    pub fn new_gt_eq_z(v: Poly) -> Self {
        Constraint {
            start: 0,
            data: C::GtEqZero(v),
            end: 0,
        }
    }

    pub fn new_gt_z(v: Poly) -> Self {
        Constraint {
            start: 0,
            data: C::GtZero(v),
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
                C::EqZero(p) => C::NEqZero(p.clone()),
                C::NEqZero(p) => C::EqZero(p.clone()),
                C::GtZero(p) => C::GtEqZero(p.clone() * Poly::constant(Rat::from(-1))),
                C::GtEqZero(p) => C::GtZero(p.clone() * Poly::constant(Rat::from(-1))),
            },
            end: self.end,
        }
    }
}

pub fn solve(preconditions: &[Constraint], postconditions: &[Constraint]) -> Vec<Error> {
    let mut errors = vec![];

    let mut preconditions = preconditions.to_vec();
    let mut postconditions = postconditions.to_vec();

    errors.extend(filter_constants(&mut preconditions));
    errors.extend(filter_constants(&mut postconditions));

    errors
}

fn filter_constants(lst: &mut Vec<Constraint>) -> Vec<Error> {
    let mut errors = vec![];

    lst.retain(|c| match &c.data {
        C::EqZero(p) => {
            if let Some(v) = p.get_constant_val() {
                if v != 0 {
                    errors.push(Error::new(
                        ErrorType::Constraint,
                        format!("cannot satisfy constraint {v} == 0"),
                        c.start,
                        c.end,
                    ))
                }
                false
            } else {
                true
            }
        }
        C::NEqZero(p) => {
            if let Some(v) = p.get_constant_val() {
                if v == 0 {
                    errors.push(Error::new(
                        ErrorType::Constraint,
                        format!("cannot satisfy constraint {v} != 0"),
                        c.start,
                        c.end,
                    ))
                }
                false
            } else {
                true
            }
        }
        C::GtEqZero(p) => {
            if let Some(v) = p.get_constant_val() {
                if v < 0 {
                    errors.push(Error::new(
                        ErrorType::Constraint,
                        format!("cannot satisfy constraint {v} >= 0"),
                        c.start,
                        c.end,
                    ))
                }
                false
            } else {
                true
            }
        }
        C::GtZero(p) => {
            if let Some(v) = p.get_constant_val() {
                if v <= 0 {
                    errors.push(Error::new(
                        ErrorType::Constraint,
                        format!("cannot satisfy constraint {v} > 0"),
                        c.start,
                        c.end,
                    ))
                }
                false
            } else {
                true
            }
        }
    });

    errors
}
