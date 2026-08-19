//! A multivariable [polynomial ring](https://en.wikipedia.org/wiki/Polynomial_ring) with integer coefficients.

#[macro_use]
pub mod mono;
pub mod coef;

use std::cmp::Ordering;

use coef::Coef;
use mono::{Mono, Pow, Var};

/// A multivariable polynomial.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Poly {
    terms: Vec<(Coef, Mono)>,
}

impl Poly {
    pub fn zero() -> Self {
        Self { terms: vec![] }
    }

    pub fn constant<T: Into<Coef>>(c: T) -> Self {
        let c = c.into();
        if c == 0.into() {
            Poly::zero()
        } else {
            Poly {
                terms: vec![(c, Mono::unit())],
            }
        }
    }

    pub fn var(v: Var) -> Poly {
        Self::term(1, Mono::new(vec![(v, 1)]))
    }

    pub fn term<T: Into<Coef>>(c: T, m: Mono) -> Self {
        let c = c.into();
        if c == 0.into() {
            Poly::zero()
        } else {
            Poly {
                terms: vec![(c, m)],
            }
        }
    }

    pub fn from_terms(terms: impl IntoIterator<Item = (Coef, Mono)>) -> Self {
        let mut raw: Vec<_> = terms.into_iter().collect();
        raw.sort_unstable_by(|(_, m1), (_, m2)| m2.cmp(m1));
        let mut terms = Vec::with_capacity(raw.len());
        for (c, m) in raw {
            match terms.last_mut() {
                Some((lc, lm)) if *lm == m => {
                    *lc = &*lc + &c;
                    if *lc == 0.into() {
                        terms.pop();
                    }
                }
                _ => {
                    if c != 0.into() {
                        terms.push((c, m));
                    }
                }
            }
        }
        Poly { terms }.debug_checked()
    }

    pub fn total_degree(&self) -> Pow {
        self.terms
            .first()
            .map(|(_, m)| m.total_degree())
            .unwrap_or(0)
    }

    pub fn degree_in(&self, v: Var) -> Pow {
        let mut deg = 0;
        for (_, mono) in &self.terms {
            let mut term_deg = 0;
            for (var, pow) in mono.exps() {
                term_deg += pow;
                match var.cmp(&v) {
                    Ordering::Greater => continue,
                    Ordering::Equal => {
                        deg = deg.min(*pow);
                        break;
                    }
                    Ordering::Less => break,
                }
            }
            if term_deg < deg {
                break;
            }
        }

        deg
    }

    pub fn mul_scalar(&self, c: Coef) -> Self {
        if c == 0.into() {
            Self::zero()
        } else {
            Self {
                terms: self
                    .terms
                    .iter()
                    .map(|t| (&t.0 * &c, t.1.clone()))
                    .collect(),
            }
        }
    }

    pub fn vars(&self) -> Vec<Var> {
        let mut vs = vec![];

        for term in &self.terms {
            for (var, _) in term.1.exps() {
                if !vs.contains(var) {
                    vs.push(*var);
                }
            }
        }

        vs.sort_unstable();
        vs
    }

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

    pub fn neg(&self) -> Self {
        Self {
            terms: self.terms.iter().map(|(c, m)| (-c, m.clone())).collect(),
        }
    }

    pub fn sub(&self, rhs: &Self) -> Self {
        self.add(&rhs.neg())
    }

    /// Panic unless canonical: strictly descending monomials, no zero
    /// coefficients, inner monomial invariants. Test and debug aid.
    #[doc(hidden)]
    pub fn assert_canonical(&self) {
        for w in self.terms.windows(2) {
            assert!(
                w[0].1 > w[1].1,
                "terms not strictly descending: {:?} then {:?}",
                w[0],
                w[1]
            );
        }
        for (c, m) in &self.terms {
            assert!(*c != 0.into(), "zero coefficient on {m:?}");
            m.assert_canonical();
        }
    }

    fn debug_checked(self) -> Poly {
        #[cfg(debug_assertions)]
        self.assert_canonical();
        self
    }

    pub fn get_constant(&self) -> Option<i128> {
        if self.terms.len() == 1 && self.terms[0].1.exps().len() == 1 {
            return Some(self.terms[0].0.get());
        }

        None
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __poly_term {
    () => {{ std::collections::VecDeque::new() }};
    ($var:ident ^ $pow:literal $(* $($rest:tt)*)?) => {{
        use $crate::solver::poly::mono::{Var, __read_var_name};
        const VAR_NAME: Var = __read_var_name(stringify!($var));

        let mut rest = $crate::__poly_term!($($($rest)*)?);
        if rest.is_empty() {
            rest.push_front((1, vec![(VAR_NAME, $pow)]));
        } else {
            rest[0].1.push((VAR_NAME, $pow));
        }
        rest
    }};

    ($var:ident ^ $pow:literal $(+ $($rest:tt)*)?) => {{
        use $crate::solver::poly::mono::{Var, __read_var_name};
        const VAR_NAME: Var = __read_var_name(stringify!($var));

        let mut rest = $crate::__poly_sum!($($($rest)*)?);
        rest.push_front((1, vec![(VAR_NAME, $pow)]));
        rest
    }};

    ($var:ident ^ $pow:literal $(- $($rest:tt)*)?) => {{
        use $crate::solver::poly::mono::{Var, __read_var_name};
        const VAR_NAME: Var = __read_var_name(stringify!($var));

        let mut rest = $crate::__poly_sum!($($($rest)*)?);
        match rest.get_mut(0) {
            Some(term) => term.0 *= -1,
            None => {}
        }
        rest.push_front((1, vec![(VAR_NAME, $pow)]));
        rest
    }};

    ($var:ident $(* $($rest:tt)*)?) => {{
        $crate::__poly_term!($var ^ 1 * $($($rest)*)?)
    }};

    ($var:ident $(+ $($rest:tt)*)?) => {{
        $crate::__poly_term!($var ^ 1 + $($($rest)*)?)
    }};

    ($var:ident $(- $($rest:tt)*)?) => {{
        $crate::__poly_term!($var ^ 1 - $($($rest)*)?)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! __poly_sum {
    ()             => {{ std::collections::VecDeque::new() }};
    ($c:literal)   => {{ std::collections::VecDeque::from([($c, vec![])]) }};
    ($c:literal+$($rest:tt)+) => {{
        let mut rest = $crate::__poly_sum!($($rest)*);
        rest.push_front(($c, vec![]));
        rest
    }};
    ($c:literal-$($rest:tt)+) => {{
        let mut rest = $crate::__poly_sum!($($rest)*);
        rest[0].0 *= -1;
        rest.push_front(($c, vec![]));
        rest
    }};
    ($c:literal*$($rest:tt)+) => {{
        let mut v = $crate::__poly_term!($($rest)*);
        v[0].0 *= $c;
        v
    }};
    ($($rest:tt)+) => {{ $crate::__poly_term!($($rest)*) }};
}

/// Create a [`Poly`]. Accepts 1-character ASCII variable names; <nobr>`poly!(2*x*y^5 + 8*z)`</nobr>.
#[doc(hidden)]
#[macro_export]
macro_rules! __poly {
    ($($body:tt)*) => {{
        use $crate::solver::poly::{Poly, mono::{Var, Pow, Mono}};
        let v: Vec<(i128, Vec<(Var, Pow)>)> = $crate::__poly_sum!($($body)*).into();
        Poly::from_terms(v.into_iter().map(|(c, exps)| (c.into(), Mono::new(exps))))
    }};
}

#[doc(inline)]
pub use crate::__poly as poly;

#[cfg(test)]
mod test {
    use super::poly;
    #[test]
    fn constants() {
        let x = poly!(3 * x ^ 2 + 5 * z - 2 + 1);
        let y = poly!(4 * y * z);
        assert_eq!(
            x.mul(&y),
            poly!(12 * x ^ 2 * y * z + 20 * y * z ^ 2 - 4 * y * z)
        );
        assert_eq!(poly!(), poly!(x - x));
    }
}
