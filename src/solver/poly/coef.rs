//! Coefficient, the constant part of a [`Poly`](super::Poly) term.

use std::cmp::{Ord, PartialOrd};
use std::ops::{Add, Div, Mul, Neg};

/// Coefficient, represented by `i128`. The constant part of a [`Poly`](super::Poly) term.
/// All arithmetic is checked and panics on overflow.
#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct Coef(i128);

impl Coef {
    pub fn new(v: i128) -> Self {
        Self(v)
    }

    pub fn get(&self) -> i128 {
        self.0
    }

    pub fn is_zero(&self) -> bool {
        self.0 == 0
    }

    pub fn is_positive(&self) -> bool {
        self.0 > 0
    }

    /// Stein's binary GCD algorithm. Always returns non-negative.
    /// `gcd(x, 0) = x` and `gcd(0, y) = y`.
    pub fn gcd(&self, other: &Self) -> Self {
        let (mut u, mut v) = (self.0.unsigned_abs(), other.0.unsigned_abs());

        if u == 0 {
            return Self(v.cast_signed());
        }
        if v == 0 {
            return Self(u.cast_signed());
        }

        // find the common power of 2 factor using trailing zeros
        let i = u.trailing_zeros();
        let j = v.trailing_zeros();
        let k = i.min(j);
        u >>= i;
        v >>= j;

        loop {
            // both u and v are guaranteed to be odd
            if u > v {
                std::mem::swap(&mut u, &mut v);
            }

            v -= u;

            if v == 0 {
                break;
            }

            // remove all trailing factors of 2 from v to make it odd again
            v >>= v.trailing_zeros();
        }

        // multiply back the shared power of 2 factor
        Self(i128::try_from(u << k).unwrap())
    }
}

impl std::fmt::Debug for Coef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<i128> for Coef {
    fn from(val: i128) -> Self {
        Coef(val)
    }
}

impl Add for &Coef {
    type Output = Coef;

    fn add(self, b: Self) -> Coef {
        Coef(self.0.checked_add(b.0).unwrap())
    }
}

impl Mul for &Coef {
    type Output = Coef;

    fn mul(self, b: Self) -> Coef {
        Coef(self.0.checked_mul(b.0).unwrap())
    }
}

impl Div for &Coef {
    type Output = Coef;

    fn div(self, b: Self) -> Coef {
        Coef(self.0.checked_div(b.0).unwrap())
    }
}

impl Neg for &Coef {
    type Output = Coef;

    fn neg(self) -> Coef {
        Coef(self.0.checked_neg().unwrap())
    }
}

#[cfg(test)]
mod tests {
    use super::Coef;
    #[test]
    fn gcd() {
        let a = Coef(156);
        let b = Coef(36);
        assert_eq!(a.gcd(&b), Coef(12));
    }
}
