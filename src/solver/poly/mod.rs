// with thanks to Prof. Emre Sertöz for the great lectures on Computational Algebraic Geometry on YouTube.

pub mod macros;
pub mod mono;
pub mod poly_arithmetic;
pub mod system;

use std::rc::Rc;

use super::field::Field;
use super::poly::mono::*;
use super::rational::{gcd, Rat};
use super::Truth;
use serde::{Serialize, Serializer};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Poly<T: Field> {
    pub terms: Vec<Mono<T>>,
}

impl Poly<Rat> {
    pub fn get_constant_i64(&self) -> Option<i64> {
        self.get_constant_val().and_then(|s| s.try_into().ok())
    }
}

impl<T: Field> Poly<T> {
    pub fn get_constant_val(&self) -> Option<T> {
        if self.terms.is_empty() {
            Some(T::zero())
        } else if self.terms.len() == 1 {
            if self.terms[0].vars.is_empty() {
                Some(self.terms[0].val.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn constant(val: T) -> Self {
        Self {
            terms: if val.is_zero() {
                vec![]
            } else {
                vec![Mono { val, vars: vec![] }]
            },
        }
    }

    pub fn var(var: String, pow: u64) -> Self {
        if pow == 0 {
            Self {
                terms: vec![Mono {
                    val: T::one(),
                    vars: vec![],
                }],
            }
        } else {
            Self {
                terms: vec![Mono {
                    val: T::one(),
                    vars: vec![(Rc::new(var), pow)],
                }],
            }
        }
    }

    pub fn is_zero(&self) -> bool {
        self.terms.is_empty()
    }

    pub fn lt(&self) -> Poly<T> {
        match self.terms.last() {
            Some(m) => Poly {
                terms: vec![m.clone()],
            },
            None => Poly { terms: vec![] },
        }
    }

    pub fn lt_mono(&self) -> Mono<T> {
        match self.terms.last() {
            Some(m) => m.clone(),
            None => Mono {
                val: T::zero(),
                vars: vec![],
            },
        }
    }

    pub fn s_poly(p: Poly<T>, q: Poly<T>) -> Poly<T> {
        let p_lt = p.lt();
        let q_lt = q.lt();

        let lcm_lmp_lmq = Poly {
            terms: vec![monomial_lcm(p_lt.lt_mono(), q_lt.lt_mono())],
        };

        if let (Some(coef_p), Some(coef_q)) =
            (lcm_lmp_lmq.try_divide(&p_lt), lcm_lmp_lmq.try_divide(&q_lt))
        {
            coef_p * p - coef_q * q
        } else {
            unreachable!()
        }
    }

    pub fn deg(&self, var: &str) -> usize {
        self.terms
            .iter()
            .map(|term| term.deg(var))
            .fold(0, |acc, v| acc.max(v))
    }

    pub fn max_deg(&self) -> usize {
        self.terms
            .iter()
            .map(|term| term.max_deg())
            .fold(0, |acc, v| acc.max(v))
    }

    pub fn coefs(&self, var: &str) -> Vec<Poly<T>> {
        let deg = self.deg(var);
        let mut coefs: Vec<_> = std::iter::repeat(Poly::constant(T::zero()))
            .take(deg + 1)
            .collect();

        for term in self.terms.iter().rev() {
            let (term_deg, term_coef) = term.coef(var);

            coefs[deg - term_deg] = coefs[deg - term_deg].clone()
                + Poly {
                    terms: vec![term_coef],
                };
        }

        coefs
    }

    pub fn from_uni_fmt(p: Vec<Self>, var: &str) -> Self {
        let mut new = Poly { terms: vec![] };
        let deg = p.len() - 1;

        for (i, term) in p.into_iter().enumerate() {
            if i == deg {
                new = new + term
            } else {
                let var_pow = Poly {
                    terms: vec![Mono {
                        val: T::one(),
                        vars: vec![(Rc::new(var.to_string()), (deg - i) as u64)],
                    }],
                };

                new = new + term * var_pow;
            }
        }

        new
    }

    pub fn eval(&self, var: &str, val: T) -> Self {
        let mut new = Poly { terms: vec![] };
        let mut val_pow = T::one();
        for mut coef in self.coefs(var).into_iter().rev() {
            for term in &mut coef.terms {
                term.val = term.val.clone() * val_pow.clone();
            }
            new = new + coef;

            val_pow = val_pow * val.clone();
        }

        new
    }

    pub fn eval_poly(&self, var: &str, val: Poly<T>) -> Self {
        let mut new = Poly { terms: vec![] };
        let mut val_pow = Poly::constant(T::one());
        for coef in self.coefs(var).into_iter().rev() {
            new = new + coef * val_pow.clone();

            val_pow = val_pow * val.clone();
        }

        new
    }

    pub fn greater(&self, other: Self) -> Truth {
        match (self.clone() - other).get_constant_val() {
            Some(v) => {
                if v > T::zero() {
                    Truth::True
                } else {
                    Truth::False
                }
            }
            None => Truth::Undetermined,
        }
    }

    pub fn less(&self, other: Self) -> Truth {
        match (self.clone() - other).get_constant_val() {
            Some(v) => {
                if v < T::zero() {
                    Truth::True
                } else {
                    Truth::False
                }
            }
            None => Truth::Undetermined,
        }
    }

    pub fn equal(&self, other: Self) -> Truth {
        if self == &other {
            Truth::True
        } else if (self.clone() - other).max_deg() == 0 {
            Truth::False
        } else {
            Truth::Undetermined
        }
    }

    pub fn var_list(&self) -> Vec<String> {
        let mut r = vec![];

        for term in &self.terms {
            for (var, _) in &term.vars {
                if !r.contains(var.as_ref()) {
                    r.push(var.as_ref().clone())
                }
            }
        }

        r
    }

    pub fn solve_for(&self, var: &str) -> Option<Self> {
        let coefs = self.coefs(var);
        if coefs.len() == 2 {
            if let Some(c) = coefs.first().unwrap().get_constant_val() {
                let minus_reciprocal = (T::one() / c) * -1;

                let res = coefs[1].clone().mul(Self::constant(minus_reciprocal));
                Some(res)
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl Poly<Rat> {
    pub fn int_coefs(&self) -> bool {
        self.terms.iter().all(|t| t.val.den == 1)
    }

    pub fn norm(&self) -> Poly<Rat> {
        let mut new = self.clone();

        let mut all_terms_den = 1;
        let mut all_terms_gcd = 1;

        if let Some(t) = new.terms.last() {
            all_terms_gcd = t.val.num;
        }

        for term in &new.terms {
            all_terms_den *= term.val.den / gcd(all_terms_den, term.val.den);
        }

        for term in &mut new.terms {
            let term_gcd = gcd(term.val.num * all_terms_den, term.val.den);
            term.val.num = term.val.num * all_terms_den / term_gcd;
            term.val.den = 1;

            all_terms_gcd = gcd(term.val.num, all_terms_gcd);
        }

        if let Some(t) = new.terms.last_mut() {
            if t.val.num < 0 {
                all_terms_gcd = -all_terms_gcd.abs();
            } else {
                all_terms_gcd = all_terms_gcd.abs();
            }
        }

        for term in &mut new.terms {
            term.val.num /= all_terms_gcd;
        }

        new
    }
}

impl std::fmt::Debug for Poly<Rat> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.terms.is_empty() {
            write!(f, "0")?;
        }

        for (i, Mono { val, vars }) in (self.terms).iter().rev().enumerate() {
            let coef: f64 = (*val).into();
            if coef != 1. || vars.is_empty() {
                if coef < 0. {
                    if coef == -1. && !vars.is_empty() {
                        if i == 0 {
                            write!(f, "-")?;
                        } else {
                            write!(f, " - ")?;
                        }
                    } else if i == 0 {
                        write!(f, "{coef}")?;
                    } else {
                        write!(f, " - {}", -coef)?;
                    }
                } else if i == 0 {
                    write!(f, "{coef}")?;
                } else {
                    write!(f, " + {coef}")?;
                }
            } else if i != 0 {
                write!(f, " + ")?;
            }

            for (var, pow) in vars {
                if *pow == 1 {
                    write!(f, "{}", var)?;
                } else {
                    write!(f, "{}^{pow}", var)?;
                }
            }
        }

        Ok(())
    }
}

impl Serialize for Poly<Rat> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&format!("{self:?}"))
    }
}

#[cfg(test)]
mod tests {
    use super::{Poly, Rat};
    use crate::poly;

    #[test]
    fn coefs() {
        let a: Poly<Rat> = Poly::var("x".to_string(), 4);
        let b: Poly<Rat> = Poly::var("x".to_string(), 2) * Poly::constant(Rat::from(3));
        let c: Poly<Rat> = Poly::var("x".to_string(), 2)
            * Poly::var("z".to_string(), 3)
            * Poly::constant(Rat::from(5));
        let d: Poly<Rat> = Poly::var("y".to_string(), 1)
            * Poly::var("x".to_string(), 1)
            * Poly::constant(Rat::from(4));
        let e: Poly<Rat> = Poly::var("z".to_string(), 1);
        let f: Poly<Rat> = Poly::constant(Rat::from(2));

        let g = a + b + c + d + e + f;

        assert_eq!("5x^2z^3 + x^4 + 3x^2 + 4xy + z + 2", format!("{g:?}"));

        assert_eq!(
            "[\"1\", \"0\", \"5z^3 + 3\", \"4y\", \"z + 2\"]",
            format!(
                "{:?}",
                g.coefs("x")
                    .iter()
                    .map(|p| format!("{p:?}"))
                    .collect::<Vec<_>>()
            )
        );

        assert_eq!(g, Poly::from_uni_fmt(g.coefs("x"), "x"));
    }

    #[test]
    fn eval() {
        let a: Poly<Rat> = Poly::var("x".to_string(), 4);
        let b: Poly<Rat> = Poly::var("x".to_string(), 2) * Poly::constant(Rat::from(3));
        let c: Poly<Rat> = Poly::var("x".to_string(), 2)
            * Poly::var("z".to_string(), 3)
            * Poly::constant(Rat::from(5));
        let d: Poly<Rat> = Poly::var("y".to_string(), 1)
            * Poly::var("x".to_string(), 1)
            * Poly::constant(Rat::from(4));
        let e: Poly<Rat> = Poly::var("z".to_string(), 1);
        let f: Poly<Rat> = Poly::constant(Rat::from(2));

        let g = a + b + c + d + e + f;

        assert_eq!("5x^2z^3 + x^4 + 3x^2 + 4xy + z + 2", format!("{g:?}"));

        assert_eq!(
            "20z^3 + 8y + z + 30",
            format!("{:?}", g.eval("x", Rat::from(2)))
        );
    }

    #[test]
    fn eval_poly() {
        let x = poly!(a ^ 2 + 3 * a * b + 2 * b + 3);
        let y = poly!(c + 10);

        assert_eq!(
            poly!(a ^ 2 + 3 * a * c + 30 * a + 2 * c + 23),
            x.eval_poly("b", y)
        );
    }

    #[test]
    fn solve_for() {
        let x = poly!(2 * a - 4 * b - 6);
        assert_eq!(Some(poly!(2 * b + 3)), x.solve_for("a"));

        let x = poly!(2 * a - 4 * a * b - 6);
        assert_eq!(None, x.solve_for("a"));

        let x = poly!(2 * a ^ 2 + 6);
        assert_eq!(None, x.solve_for("a"));
    }
}
