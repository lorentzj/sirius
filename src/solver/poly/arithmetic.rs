use super::{Coef, Mono, Poly};
use std::cmp::Ordering;

/// Exact monomial division with coefficients.
pub fn monomial_div(lhs: &(Coef, Mono), rhs: &(Coef, Mono)) -> Option<(Coef, Mono)> {
    if rhs.0.is_zero() {
        None
    } else if lhs.0.is_zero() {
        Some((Coef::new(0), Mono::unit()))
    } else if let Some(quot) = lhs.1.div(&rhs.1) {
        let const_quot = Coef::new(lhs.0.get() / rhs.0.get());
        if rhs.0 * const_quot == lhs.0 {
            Some((const_quot, quot))
        } else {
            None
        }
    } else {
        None
    }
}

impl super::Poly {
    pub fn add(&self, rhs: &Self) -> Self {
        let mut terms = Vec::with_capacity(self.terms.len() + rhs.terms.len());
        let (mut a, mut b) = (self.terms.iter().peekable(), rhs.terms.iter().peekable());
        loop {
            match (a.peek(), b.peek()) {
                (Some(&(ac, am)), Some(&(bc, bm))) => match am.cmp(bm) {
                    Ordering::Greater => {
                        terms.push((*ac, am.clone()));
                        a.next();
                    }
                    Ordering::Less => {
                        terms.push((*bc, bm.clone()));
                        b.next();
                    }
                    Ordering::Equal => {
                        let c = ac + bc;
                        if c != 0.into() {
                            terms.push((c, am.clone()));
                        }
                        a.next();
                        b.next();
                    }
                },
                (Some(_), None) => terms.extend(a.by_ref().cloned()),
                (None, Some(_)) => terms.extend(b.by_ref().cloned()),
                (None, None) => break,
            }
        }
        Self { terms }.debug_checked()
    }

    pub fn mul(&self, rhs: &Self) -> Self {
        let mut buf = Vec::with_capacity(self.terms.len() * rhs.terms.len());
        for (ac, am) in &self.terms {
            for (bc, bm) in &rhs.terms {
                buf.push((ac * bc, (am.mul(bm))));
            }
        }
        Self::from_terms(buf)
    }

    pub fn mul_scalar<T: Into<Coef>>(&self, c: T) -> Self {
        let c = c.into();
        if c.is_zero() {
            Self::zero()
        } else {
            Self {
                terms: self.terms.iter().map(|t| (t.0 * c, t.1.clone())).collect(),
            }
        }
    }

    pub fn pow(&self, mut e: u32) -> Self {
        let mut acc = Self::constant(1);
        let mut base = self.clone();
        while e > 0 {
            if e & 1 == 1 {
                acc = acc.mul(&base);
            }
            e >>= 1;
            if e > 0 {
                base = base.mul(&base);
            }
        }
        acc
    }

    pub fn neg(&self) -> Self {
        self.mul_scalar(-1)
    }

    pub fn sub(&self, rhs: &Self) -> Self {
        self.add(&rhs.neg())
    }

    /// Via [polynomial long division](https://en.wikipedia.org/wiki/Polynomial_long_division).
    pub fn compound_divide(&self, divisors: &[Self]) -> (Vec<Self>, Self) {
        if divisors.is_empty() {
            return (vec![], self.clone());
        }

        let mut dividend = self.clone();

        let mut rem = Poly::constant(0);
        let mut quotients: Vec<Vec<(Coef, Mono)>> =
            std::iter::repeat_n(Vec::default(), divisors.len()).collect();

        let mut curr_term = 0;
        let mut curr_divisor = 0;

        while dividend.terms.len() > curr_term {
            let self_lt = dividend.terms[curr_term].clone();
            if !divisors[curr_divisor].is_zero() {
                let div_lt = &divisors[curr_divisor].leading_term();
                let self_over_div_lt = monomial_div(&self_lt, div_lt);
                if let Some(self_over_div_lt) = self_over_div_lt {
                    quotients[curr_divisor].push(self_over_div_lt.clone());

                    let self_over_div_lt = Poly {
                        terms: vec![self_over_div_lt],
                    };

                    dividend = dividend.sub(&self_over_div_lt.mul(&divisors[curr_divisor]));

                    curr_divisor = 0;
                } else {
                    curr_divisor += 1;
                }
            } else {
                curr_divisor += 1;
            }

            if curr_divisor == divisors.len() {
                let self_lt = Self {
                    terms: vec![self_lt.clone()],
                };
                curr_term += 1;
                rem = rem.add(&self_lt);
                curr_divisor = 0;
            }
        }

        let quotients = quotients
            .into_iter()
            .map(|v| Self::debug_checked(Self { terms: v }))
            .collect();

        (quotients, rem)
    }

    pub fn try_divide(&self, divisor: &Self) -> Option<Self> {
        let (mut quots, rem) = self.compound_divide(std::slice::from_ref(divisor));

        if rem.is_zero() {
            Some(quots.pop().unwrap())
        } else {
            None
        }
    }
}
