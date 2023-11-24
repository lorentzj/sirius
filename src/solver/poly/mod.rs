pub mod macros;
pub mod mono;
pub mod poly_arithmetic;
pub mod system;

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
                    vars: vec![(var, pow)],
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
                        vars: vec![(var.to_string(), (deg - i) as u64)],
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
                if !r.contains(var) {
                    r.push(var.clone())
                }
            }
        }

        r
    }
}

impl Poly<Rat> {
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
}
