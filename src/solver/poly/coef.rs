//! Coefficient, the constant part of a [`Poly`](super::Poly) term.

use std::cmp::{Ord, PartialOrd};
use std::ops::Neg;

/// Coefficient, represented by `i128`. The constant part of a [`Poly`](super::Poly) term.
/// All arithmetic is checked and panics on overflow.
#[derive(Clone, Copy, PartialOrd, Ord, Hash, PartialEq, Eq)]
pub struct Coef(i128);

impl Coef {
    pub fn new(v: i128) -> Self {
        Self(v)
    }

    pub fn get(&self) -> i128 {
        self.0
    }

    pub fn vec(arr: &[i128]) -> Vec<Self> {
        arr.iter().map(|a| Coef::new(*a)).collect()
    }

    pub fn is_zero(&self) -> bool {
        self.0 == 0
    }

    pub fn is_positive(&self) -> bool {
        self.0 > 0
    }

    pub fn gcd(&self, other: &Self) -> Self {
        let (mut a, mut b) = (self.0, other.0);
        while b != 0 {
            (a, b) = (b, a % b);
        }
        Self(a)
    }

    pub fn lcm(&self, other: &Self) -> Self {
        self * other / self.gcd(other)
    }

    pub fn abs(&self) -> Self {
        Coef(self.0.checked_abs().unwrap())
    }

    pub fn divrem(&self, other: Coef) -> (Self, Self) {
        (
            Coef(self.0.checked_div(other.0).unwrap()),
            Coef(self.0.checked_rem(other.0).unwrap()),
        )
    }

    fn add_impl(&self, b: &Self) -> Coef {
        Coef(self.0.checked_add(b.0).unwrap())
    }

    fn sub_impl(&self, b: &Self) -> Coef {
        Coef(self.0.checked_sub(b.0).unwrap())
    }

    fn mul_impl(&self, b: &Self) -> Coef {
        Coef(self.0.checked_mul(b.0).unwrap())
    }

    fn div_impl(&self, b: &Self) -> Coef {
        Coef(self.0.checked_div(b.0).unwrap())
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

impl Neg for Coef {
    type Output = Coef;

    fn neg(self) -> Coef {
        Coef(self.0.checked_neg().unwrap())
    }
}

macro_rules! impl_binop {
    ($T:ident, $Trait:ident, $method:ident, $impl_fn:ident) => {
        impl std::ops::$Trait<&$T> for &$T {
            type Output = $T;
            fn $method(self, rhs: &$T) -> $T {
                self.$impl_fn(rhs)
            }
        }
        impl std::ops::$Trait<$T> for &$T {
            type Output = $T;
            fn $method(self, rhs: $T) -> $T {
                self.$impl_fn(&rhs)
            }
        }
        impl std::ops::$Trait<&$T> for $T {
            type Output = $T;
            fn $method(self, rhs: &$T) -> $T {
                self.$impl_fn(rhs)
            }
        }
        impl std::ops::$Trait<$T> for $T {
            type Output = $T;
            fn $method(self, rhs: $T) -> $T {
                self.$impl_fn(&rhs)
            }
        }
    };
}

impl_binop!(Coef, Add, add, add_impl);
impl_binop!(Coef, Sub, sub, sub_impl);
impl_binop!(Coef, Mul, mul, mul_impl);
impl_binop!(Coef, Div, div, div_impl);

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
