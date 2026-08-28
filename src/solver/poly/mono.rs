//! Monomial without coefficient, the variable part of a [`Poly`](super::Poly) term.

use std::cmp::Ordering;

/// A variable, represented by `u32`.
pub type Var = u32;
/// An exponent, represented by `u32`.
pub type Pow = u32;

/// A power product $\prod_i x_i^{e_i}$ (for example, $x^2y^5$). The variable part of a [`Poly`](super::Poly) term.
/// [`Mono::cmp`] implements [graded-lex ordering](https://en.wikipedia.org/wiki/Monomial_order#Graded_lexicographic_order).
/// ```
/// # use sirius::solver::poly::mono::mono;
/// let a = mono!(x^2);
/// let b = mono!(x*y);
/// let c = mono!(x*z^2);
/// assert_eq!(a.mul(&b).mul(&c), mono!(x^4*y*z^2));
/// ```
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Mono {
    exps: Vec<(Var, Pow)>,
}

impl Mono {
    pub fn new(exps: impl IntoIterator<Item = (Var, Pow)>) -> Self {
        // Normalize an exp list: sort, combine like vars, and remove zero powers.
        let mut pairs: Vec<_> = exps.into_iter().filter(|&(_, e)| e > 0).collect();
        pairs.sort_unstable_by_key(|&(v, _)| v);
        let mut exps = Vec::<(Var, Pow)>::with_capacity(pairs.len());
        for (v, e) in pairs {
            match exps.last_mut() {
                Some((lv, le)) if *lv == v => *le += e,
                _ => exps.push((v, e)),
            }
        }
        Self { exps }
    }

    /// Unit monomial, 1.
    pub fn unit() -> Self {
        Self::new(vec![])
    }

    pub fn exps(&self) -> &[(Var, Pow)] {
        &self.exps
    }

    pub fn is_unit(&self) -> bool {
        self.exps.is_empty()
    }

    pub fn total_degree(&self) -> Pow {
        self.exps.iter().map(|&(_, e)| e).sum()
    }

    pub fn mul(&self, other: &Self) -> Self {
        // Merge sorted `exps` and combine like vars to maintain canonical form.
        let mut exps = Vec::with_capacity(self.exps.len() + other.exps.len());
        let (mut a, mut b) = (self.exps.iter().peekable(), other.exps.iter().peekable());
        loop {
            match (a.peek(), b.peek()) {
                (Some(&&(va, ea)), Some(&&(vb, eb))) => match va.cmp(&vb) {
                    Ordering::Less => {
                        exps.push((va, ea));
                        a.next();
                    }
                    Ordering::Greater => {
                        exps.push((vb, eb));
                        b.next();
                    }
                    Ordering::Equal => {
                        exps.push((va, ea + eb));
                        a.next();
                        b.next();
                    }
                },
                (Some(_), None) => {
                    exps.extend(a);
                    break;
                }
                (None, Some(_)) => {
                    exps.extend(b);
                    break;
                }
                (None, None) => break,
            }
        }
        Self { exps }.debug_checked()
    }

    pub fn div(&self, other: &Self) -> Option<Self> {
        let mut lhs_var_iter = self.exps.iter().peekable();
        let mut rhs_var_iter = other.exps.iter().peekable();
        let mut vars = vec![];
        while let Some((rhs_var, rhs_pow)) = rhs_var_iter.peek() {
            if let Some((lhs_var, lhs_pow)) = lhs_var_iter.peek() {
                match lhs_var.cmp(rhs_var) {
                    Ordering::Equal => match lhs_pow.cmp(rhs_pow) {
                        Ordering::Greater => {
                            vars.push((*lhs_var, lhs_pow - rhs_pow));
                            lhs_var_iter.next();
                            rhs_var_iter.next();
                            continue;
                        }
                        Ordering::Equal => {
                            lhs_var_iter.next();
                            rhs_var_iter.next();
                            continue;
                        }
                        Ordering::Less => return None,
                    },
                    Ordering::Less => {
                        vars.push((*lhs_var, *lhs_pow));
                        lhs_var_iter.next();
                        continue;
                    }
                    Ordering::Greater => {
                        return None;
                    }
                }
            }

            return None;
        }

        for (lhs_var, lhs_pow) in lhs_var_iter {
            vars.push((*lhs_var, *lhs_pow));
        }

        Some(Mono::new(vars))
    }

    pub fn degree_in<T: Into<Var>>(&self, v: T) -> Pow {
        let v = v.into();
        self.exps
            .iter()
            .find(|(va, _)| *va == v)
            .map(|exp| exp.1)
            .unwrap_or(0)
    }

    pub fn lcm(&self, rhs: &Self) -> Self {
        let mut vars = vec![];

        let mut lhs_vars = self.exps.iter().peekable();
        let mut rhs_vars = rhs.exps.iter().peekable();

        loop {
            match (lhs_vars.peek(), rhs_vars.peek()) {
                (Some(lhs_v), Some(rhs_v)) => match lhs_v.0.cmp(&rhs_v.0) {
                    Ordering::Equal => {
                        vars.push((lhs_v.0, lhs_v.1.max(rhs_v.1)));
                        lhs_vars.next();
                        rhs_vars.next();
                    }
                    Ordering::Greater => {
                        vars.push(**rhs_v);
                        rhs_vars.next();
                    }
                    Ordering::Less => {
                        vars.push(**lhs_v);
                        lhs_vars.next();
                    }
                },
                (Some(lhs_v), None) => {
                    vars.push(**lhs_v);
                    lhs_vars.next();
                }
                (None, Some(rhs_v)) => {
                    vars.push(**rhs_v);
                    rhs_vars.next();
                }
                (None, None) => break,
            }
        }

        Mono { exps: vars }
    }

    pub fn assert_canonical(&self) {
        for w in self.exps.windows(2) {
            assert!(
                w[0].0 < w[1].0,
                "monomial vars not strictly ascending: {:?}",
                self.exps
            );
        }
        for &(_, e) in &self.exps {
            assert!(e >= 1, "monomial has zero exponent: {:?}", self.exps);
        }
    }

    fn debug_checked(self) -> Self {
        #[cfg(debug_assertions)]
        self.assert_canonical();
        self
    }
}

impl Ord for Mono {
    /// [Graded-lex](https://en.wikipedia.org/wiki/Monomial_order#Graded_lexicographic_order): total degree first;
    /// ties broken lexicographically on exponent vectors with lower [`Var`] index more significant
    /// (larger exponent on the first differing variable wins).
    fn cmp(&self, other: &Self) -> Ordering {
        self.total_degree()
            .cmp(&other.total_degree())
            .then_with(|| {
                let (mut a, mut b) = (self.exps.iter(), other.exps.iter());
                loop {
                    match (a.next(), b.next()) {
                        (Some(&(va, ea)), Some(&(vb, eb))) => match va.cmp(&vb) {
                            Ordering::Less => return Ordering::Greater,
                            Ordering::Greater => return Ordering::Less,
                            Ordering::Equal => {
                                if ea != eb {
                                    return ea.cmp(&eb);
                                }
                            }
                        },
                        (Some(_), None) => return Ordering::Greater,
                        (None, Some(_)) => return Ordering::Less,
                        (None, None) => return Ordering::Equal,
                    }
                }
            })
    }
}

impl PartialOrd for Mono {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[doc(hidden)]
/// Helper function for the [`mono`](crate::mono) macro.
pub const fn __read_var_name(name: &str) -> Var {
    let bytes = name.as_bytes();
    assert!(bytes.len() == 1, "variable name must have length 1");
    bytes[0] as Var
}

/// Create a [`Mono`]. Accepts 1-character ASCII variable names; for example, `mono!(x*y^5)`.
#[doc(hidden)]
#[macro_export]
macro_rules! __mono {
    () => { $crate::solver::poly::mono::Mono::unit() };

    ($var:ident ^ $pow:literal $(* $($rest:tt)*)?) => {{
        use $crate::solver::poly::mono::{Mono, Var, __read_var_name};
        const VAR_NAME: Var = __read_var_name(stringify!($var));
        Mono::new(vec![(VAR_NAME, $pow)]).mul(&$crate::__mono!($($($rest)*)?))
    }};
    ($var:ident $(* $($rest:tt)*)?) => {{
        use $crate::solver::poly::mono::{Mono, Var, __read_var_name};
        const VAR_NAME: Var = __read_var_name(stringify!($var));
        Mono::new(vec![(VAR_NAME, 1)]).mul(&$crate::__mono!($($($rest)*)?))
    }};
}

#[doc(inline)]
pub use crate::__mono as mono;

#[cfg(test)]
mod tests {
    use super::mono;

    #[test]
    fn graded_lex() {
        let descending = [
            mono!(x ^ 2),
            mono!(x * y),
            mono!(x * z),
            mono!(y ^ 2),
            mono!(y * z),
            mono!(z ^ 2),
            mono!(x),
            mono!(y),
            mono!(z),
            mono!(),
        ];
        for w in descending.windows(2) {
            assert!(w[0] > w[1], "expected {:?} > {:?}", w[0], w[1]);
        }
    }

    #[test]
    fn multiplication() {
        assert_eq!(mono!(x).mul(&mono!(x)), mono!(x ^ 2));
        let a = mono!(x ^ 2 * y).mul(&mono!(x * z ^ 2));
        a.assert_canonical();
        assert_eq!(a, mono!(x ^ 3 * y * z ^ 2));
    }
}
