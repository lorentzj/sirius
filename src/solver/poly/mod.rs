//! A multivariable [polynomial ring](https://en.wikipedia.org/wiki/Polynomial_ring) with integer coefficients.

#[macro_use]
pub mod mono;
pub mod coef;

use coef::Coef;
use mono::{Mono, Pow, Var};
use std::cmp::Ordering;

fn monomial_div(lhs: &(Coef, Mono), rhs: &(Coef, Mono)) -> Option<(Coef, Mono)> {
    if rhs.0.is_zero() {
        None
    } else if lhs.0.is_zero() {
        Some((Coef::new(0), Mono::unit()))
    } else if let Some(quot) = lhs.1.div(&rhs.1) {
        let const_quot = rhs.0 / lhs.0;
        if lhs.0 * const_quot == rhs.0 {
            Some((const_quot, quot))
        } else {
            None
        }
    } else {
        None
    }
}

/// A multivariable polynomial.
#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Poly {
    terms: Vec<(Coef, Mono)>,
}

impl Poly {
    pub fn zero() -> Self {
        Self { terms: vec![] }
    }

    pub fn is_zero(&self) -> bool {
        self.terms.is_empty()
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

    pub fn var(v: Var, p: Pow) -> Self {
        if p == 0 {
            Self::constant(1)
        } else {
            Self::term(1, Mono::new(vec![(v, p)]))
        }
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

    pub fn terms(&self) -> Vec<(Coef, Mono)> {
        self.terms.clone()
    }

    pub fn from_terms(terms: impl IntoIterator<Item = (Coef, Mono)>) -> Self {
        let mut raw: Vec<_> = terms.into_iter().collect();
        raw.sort_unstable_by(|(_, m1), (_, m2)| m2.cmp(m1));
        let mut terms = Vec::with_capacity(raw.len());
        for (c, m) in raw {
            match terms.last_mut() {
                Some((lc, lm)) if *lm == m => {
                    *lc = *lc + c;
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
        Self { terms }.debug_checked()
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
                        deg = deg.max(*pow);
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

    pub fn eval(&self, mut point: impl FnMut(Var) -> i128) -> Coef {
        let mut acc = Coef::new(0);
        for (c, m) in &self.terms {
            let mut t = *c;
            for &(v, e) in m.exps() {
                t = t * Coef::new(point(v).checked_pow(e).unwrap());
            }
            acc = acc + t;
        }
        acc
    }

    pub fn map_vars(&self, mut f: impl FnMut(Var) -> Self) -> Self {
        let mut acc = Poly::zero();
        for (c, m) in &self.terms {
            let mut t = Poly::constant(*c);
            for &(v, e) in m.exps() {
                t = t.mul(&f(v).pow(e));
            }
            acc = acc.add(&t);
        }
        acc
    }

    pub fn substitute(&self, v: Var, q: &Self) -> Self {
        self.map_vars(|w| if w == v { q.clone() } else { Self::var(w, 1) })
    }

    pub fn coef_gcd(&self) -> Coef {
        self.terms
            .iter()
            .fold(Coef::from(0), |g, &(c, _)| g.gcd(&c))
            .abs()
    }

    pub fn divide_coefs<T: Into<Coef>>(&self, d: T) -> Option<Self> {
        let d = d.into();
        if d.is_zero() {
            return None;
        }
        let mut terms = Vec::with_capacity(self.terms.len());
        for (c, m) in &self.terms {
            let (quot, rem) = c.divrem(d);
            if rem.is_zero() {
                terms.push((quot, m.clone()));
            } else {
                return None;
            }
        }
        Some(Poly { terms }.debug_checked())
    }

    pub fn as_constant(&self) -> Option<Coef> {
        match self.terms.as_slice() {
            [] => Some(Coef::new(0)),
            [(c, m)] if m.is_unit() => Some(*c),
            _ => None,
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
        self.mul_scalar(-1)
    }

    pub fn sub(&self, rhs: &Self) -> Self {
        self.add(&rhs.neg())
    }

    pub fn leading_term(&self) -> (Coef, Mono) {
        match self.terms.first() {
            Some(m) => m.clone(),
            None => (Coef::new(0), Mono::unit()),
        }
    }

    pub fn compound_divide(&self, divisors: &[Self]) -> (Vec<Self>, Self) {
        println!("divide {self:?} by {divisors:?}");

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
            println!("-----------");
            println!("leading term: {self_lt:?}");
            if !divisors[curr_divisor].is_zero() {
                let div_lt = &divisors[curr_divisor].leading_term();
                let self_over_div_lt = monomial_div(&self_lt, div_lt);

                println!("curr_divisor: {:?}", divisors[curr_divisor]);
                println!("curr_divisor leading term: {div_lt:?}");
                println!("self_over_div_lt: {self_over_div_lt:?}");

                if let Some(self_over_div_lt) = self_over_div_lt {
                    quotients[curr_divisor].push(self_over_div_lt.clone());

                    let self_over_div_lt = Poly {
                        terms: vec![self_over_div_lt],
                    };

                    println!(
                        "subtracting {dividend:?} by ({self_over_div_lt:?}) * ({:?})",
                        divisors[curr_divisor]
                    );
                    println!(
                        "subtracting {dividend:?} by {:?}",
                        self_over_div_lt.mul(&divisors[curr_divisor])
                    );

                    dividend = dividend.sub(&self_over_div_lt.mul(&divisors[curr_divisor]));
                    println!("new dividend: {dividend:?}");

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
                println!("hit end of divisors; adding {self_lt:?} to rem {rem:?}");
                curr_term += 1;

                rem = rem.add(&self_lt);
                println!("new rem: {rem:?}");
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

    fn debug_checked(self) -> Self {
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

impl std::fmt::Debug for Poly {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.terms.first() {
            None => write!(f, "0")?,
            Some((coef, mono)) => {
                write!(f, "{:?}{:?}", coef, mono)?;
            }
        }

        for (coef, mono) in self.terms.iter().skip(1) {
            if coef.is_positive() {
                write!(f, " + {:?}{:?}", coef, mono)?;
            } else {
                write!(f, " - {:?}{:?}", -*coef, mono)?;
            }
        }

        Ok(())
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

    #[test]
    fn arith_sanity() {
        let x = poly!(a ^ 2 + 2 * b + c);
        let y = poly!(2 * a ^ 2 - c ^ 3 + d);
        assert!(x.add(&y) == poly!(-1 * c ^ 3 + 3 * a ^ 2 + 2 * b + c + d));

        let x = poly!(a ^ 4 - b ^ 4);
        let y = poly!(a ^ 2 + b ^ 2);
        assert_eq!(x.try_divide(&y), Some(poly!(a ^ 2 - b ^ 2)));

        let x = poly!(a ^ 2 - 2 * a * b + b ^ 2);
        let y = poly!(a - b);
        assert_eq!(x.try_divide(&y), Some(poly!(a - b)));

        let x = poly!(-4 * b);
        let y = poly!(a);
        assert_eq!(x.try_divide(&y), None);
    }

    #[test]
    fn arith_fuzz() {
        use rand::prelude::*;

        use super::Poly;

        let mut rng = SmallRng::seed_from_u64(1);

        fn create_random_poly(rng: &mut SmallRng, term_max: i32) -> Poly {
            let mut p = Poly::zero();

            for _ in 0..rng.gen_range(0..term_max + 1) {
                let coef = rng.gen_range(-7..7);
                let xpow = rng.gen_range(0..2);
                let ypow = rng.gen_range(0..2);
                let zpow = rng.gen_range(0..4);
                let coef = Poly::constant(coef);
                let xpow = Poly::var('x' as u64, xpow);
                let ypow = Poly::var('y' as u64, ypow);
                let zpow = Poly::var('z' as u64, zpow);

                p = p.add(&coef.mul(&xpow).mul(&ypow).mul(&zpow));
            }

            p
        }

        for _ in 0..10_000 {
            let dividend = create_random_poly(&mut rng, 10);
            let n_divs = rng.gen_range(0..4);
            let mut divisors: Vec<_> = std::iter::repeat_with(|| create_random_poly(&mut rng, 6))
                .take(n_divs)
                .collect();

            let (quotients, rem) = dividend.compound_divide(&mut divisors);

            println!("-------------------------");
            println!("calculated {:?} / {:?}", dividend, divisors);
            println!("got {:?} rem {:?}", quotients, rem);
            println!("-------------------------");

            let calculated_dividend = quotients
                .into_iter()
                .zip(divisors.clone())
                .fold(Poly::zero(), |acc, (x, y)| acc.add(&mut x.mul(&y)))
                .add(&mut rem.clone());

            assert_eq!(calculated_dividend, dividend);
        }
    }
}
