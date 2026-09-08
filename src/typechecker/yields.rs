//! How many times a block `yield`s.
//!
//! A straight-line block yields an exact [`Count`]. A conditional one only yields *at most* as
//! many as its longer branch, and that upper bound is what an existential size gets bound to --
//! `filter_lt` yielding at most `A` times is what makes `{ex B st B <= A}` provable.

use crate::solver::count::Count;
use crate::solver::poly::{Poly, Var};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Yields {
    Exact(Count),
    AtMost(Count),
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
            Self::Exact(c) | Self::AtMost(c) => Some(c),
            Self::Unknown => None,
        }
    }

    pub fn is_exact(&self) -> bool {
        matches!(self, Self::Exact(_))
    }

    pub fn is_zero(&self) -> bool {
        matches!(self, Self::Exact(c) if c.is_zero())
    }

    /// One block after another.
    pub fn then(&self, next: &Self) -> Self {
        match (self, next) {
            (Self::Unknown, _) | (_, Self::Unknown) => Self::Unknown,
            (Self::Exact(a), Self::Exact(b)) => Self::Exact(a.add(b)),
            (a, b) => Self::AtMost(a.bound().unwrap().add(b.bound().unwrap())),
        }
    }

    /// A block repeated for `var` over `[lo, hi)`. Summation is monotonic over non-negative
    /// summands, so a per-iteration upper bound sums to a total upper bound.
    pub fn repeated(&self, var: Var, lo: &Poly, hi: &Poly) -> Self {
        match self {
            Self::Exact(c) => Self::Exact(c.sum_range(var, lo, hi)),
            Self::AtMost(c) => Self::AtMost(c.sum_range(var, lo, hi)),
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
        Self::AtMost(if provably_le(a, b) {
            b.clone()
        } else if provably_le(b, a) {
            a.clone()
        } else {
            a.add(b)
        })
    }
}
