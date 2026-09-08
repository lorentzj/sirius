//! How many times a block `yield`s.
//!
//! A straight-line block yields an exact [`Count`]. A conditional one only yields *at most* as
//! many as its longer branch, and that upper bound is what an existential size gets bound to --
//! `filter_lt` yielding at most `A` times is what makes `{ex B st B <= A}` provable.
//!
//! An upper bound alone is not enough for a signature like `-> f32[2*B]`, which claims the length
//! is *even*. So a bound also carries a [`stride`](Yields::stride): an integer dividing every
//! count the block can actually produce. A body that yields twice per matching element has stride
//! 2, which is what lets the checker witness `B`.

use crate::solver::count::Count;
use crate::solver::poly::coef::Coef;
use crate::solver::poly::{Poly, Var};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Yields {
    Exact(Count),
    /// At most `bound`, and always a multiple of `stride`.
    AtMost {
        bound: Count,
        stride: Coef,
    },
    /// Not countable at all. Only reached after another error has already been reported, so the
    /// count is not worth complaining about a second time.
    Unknown,
}

impl Yields {
    pub fn zero() -> Self {
        Self::Exact(Count::zero())
    }

    pub fn once() -> Self {
        Self::Exact(Count::constant(1))
    }

    /// The exact count, or an upper bound on it.
    pub fn bound(&self) -> Option<&Count> {
        match self {
            Self::Exact(c) | Self::AtMost { bound: c, .. } => Some(c),
            Self::Unknown => None,
        }
    }

    pub fn is_exact(&self) -> bool {
        matches!(self, Self::Exact(_))
    }

    pub fn is_zero(&self) -> bool {
        matches!(self, Self::Exact(c) if c.is_zero())
    }

    /// An integer dividing every count this block can produce. `0` means only zero is reachable,
    /// which every integer divides -- the identity for [`Coef::gcd`].
    pub fn stride(&self) -> Coef {
        match self {
            // an exact polynomial count is divisible by the gcd of its coefficients
            Self::Exact(c) => match c.as_poly() {
                Some(p) => p.coef_gcd(),
                None => Coef::new(1),
            },
            Self::AtMost { stride, .. } => *stride,
            Self::Unknown => Coef::new(1),
        }
    }

    /// One block after another.
    pub fn then(&self, next: &Self) -> Self {
        match (self, next) {
            (Self::Unknown, _) | (_, Self::Unknown) => Self::Unknown,
            (Self::Exact(a), Self::Exact(b)) => Self::Exact(a.add(b)),
            (a, b) => Self::AtMost {
                bound: a.bound().unwrap().add(b.bound().unwrap()),
                stride: a.stride().gcd(&b.stride()),
            },
        }
    }

    /// A block repeated for `var` over `[lo, hi)`. Summation is monotonic over non-negative
    /// summands, so a per-iteration upper bound sums to a total upper bound; and a sum of
    /// multiples of `stride` is itself a multiple of `stride`.
    pub fn repeated(&self, var: Var, lo: &Poly, hi: &Poly) -> Self {
        match self {
            Self::Exact(c) => Self::Exact(c.sum_range(var, lo, hi)),
            Self::AtMost { bound, stride } => Self::AtMost {
                bound: bound.sum_range(var, lo, hi),
                stride: *stride,
            },
            Self::Unknown => Self::Unknown,
        }
    }

    /// The two arms of an `if`. Equal exact counts stay exact; otherwise the result is bounded by
    /// whichever arm is provably larger, falling back to their sum when neither is.
    pub fn branch(&self, other: &Self, provably_le: impl Fn(&Count, &Count) -> bool) -> Self {
        let (Some(a), Some(b)) = (self.bound(), other.bound()) else {
            return Self::Unknown;
        };
        if self.is_exact() && other.is_exact() && a == b {
            return self.clone();
        }
        Self::AtMost {
            bound: if provably_le(a, b) {
                b.clone()
            } else if provably_le(b, a) {
                a.clone()
            } else {
                a.add(b)
            },
            // the count is one arm or the other, so it is a multiple of their common divisor
            stride: self.stride().gcd(&other.stride()),
        }
    }
}
