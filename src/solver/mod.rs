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
    Eq(Poly, Poly),
    NEq(Poly, Poly),
    GtEq(Poly, Poly),
    Gt(Poly, Poly),
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
    let mut errors = vec![];

    let mut preconditions = preconditions.to_vec();
    let mut postconditions = postconditions.to_vec();

    errors.extend(filter_constants(&mut preconditions));
    errors.extend(filter_constants(&mut postconditions));

    errors
}

fn filter_constants(lst: &mut Vec<Constraint>) -> Vec<Error> {
    let mut errors = vec![];

    lst.retain(|c| {
        let rel_zero = c.get_rel_zero();
        if let Some(v) = rel_zero.get_constant_val() {
            if let C::Eq(a, b) = &c.data && v != 0 {
                errors.push(Error::new(
                    ErrorType::Constraint,
                    format!("cannot satisfy constraint \"{a:?} == {b:?}\""),
                    c.start,
                    c.end,
                ));
            }

            if let C::NEq(a, b) = &c.data && v == 0 {
                errors.push(Error::new(
                    ErrorType::Constraint,
                    format!("cannot satisfy constraint \"{a:?} != {b:?}\""),
                    c.start,
                    c.end,
                ));
            }

            if let C::Gt(a, b) = &c.data && v <= 0 {
                errors.push(Error::new(
                    ErrorType::Constraint,
                    format!("cannot satisfy constraint \"{a:?} > {b:?}\""),
                    c.start,
                    c.end,
                ));
            }

            if let C::GtEq(a, b) = &c.data && v < 0 {
                errors.push(Error::new(
                    ErrorType::Constraint,
                    format!("cannot satisfy constraint \"{a:?} >= {b:?}\""),
                    c.start,
                    c.end,
                ));
            }

            false
        } else {
            true
        }
    });

    errors
}
