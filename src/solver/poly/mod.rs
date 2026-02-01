pub mod macros;
pub mod mono;
pub mod poly_arithmetic;
pub mod system;

use std::fmt::Write;

use super::Rat;
use mono::*;
use num::Signed;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Poly {
    pub terms: Vec<Mono>,
}

impl Poly {
    pub fn get_constant_val(&self) -> Option<Rat> {
        if self.terms.is_empty() {
            Some(Rat::zero())
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
}

impl Poly {
    pub fn constant(val: Rat) -> Self {
        Self {
            terms: if val.is_zero() {
                vec![]
            } else {
                vec![Mono { val, vars: vec![] }]
            },
        }
    }

    pub fn var(var: usize, pow: u64) -> Self {
        if pow == 0 {
            Self {
                terms: vec![Mono {
                    val: Rat::one(),
                    vars: vec![],
                }],
            }
        } else {
            Self {
                terms: vec![Mono {
                    val: Rat::one(),
                    vars: vec![(var, pow)],
                }],
            }
        }
    }

    pub fn is_zero(&self) -> bool {
        self.terms.is_empty()
    }

    pub fn lt(&self) -> Poly {
        match self.terms.last() {
            Some(m) => Poly {
                terms: vec![m.clone()],
            },
            None => Poly { terms: vec![] },
        }
    }

    pub fn lt_mono(&self) -> Mono {
        match self.terms.last() {
            Some(m) => m.clone(),
            None => Mono {
                val: Rat::zero(),
                vars: vec![],
            },
        }
    }

    pub fn s_poly(p: Poly, q: Poly) -> Poly {
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

    pub fn deg(&self, var: usize) -> usize {
        self.terms
            .iter()
            .map(|term| term.deg(var))
            .fold(0, |acc, v| acc.max(v))
    }

    pub fn coefs(&self, var: usize) -> Vec<Poly> {
        let deg = self.deg(var);
        let mut coefs: Vec<_> = std::iter::repeat_n(Poly::constant(Rat::zero()), deg + 1).collect();

        for term in self.terms.iter().rev() {
            let (term_deg, term_coef) = term.coef(var);

            coefs[deg - term_deg] = coefs[deg - term_deg].clone()
                + Poly {
                    terms: vec![term_coef],
                };
        }

        coefs
    }

    pub fn from_uni_fmt(p: Vec<Self>, var: usize) -> Self {
        let mut new = Poly { terms: vec![] };
        let deg = p.len() - 1;

        for (i, term) in p.into_iter().enumerate() {
            if i == deg {
                new = new + term
            } else {
                let var_pow = Poly {
                    terms: vec![Mono {
                        val: Rat::one(),
                        vars: vec![(var, (deg - i) as u64)],
                    }],
                };

                new = new + term * var_pow;
            }
        }

        new
    }

    pub fn eval(&self, var: usize, val: Rat) -> Self {
        let mut new = Poly { terms: vec![] };
        let mut val_pow = Rat::one();
        for mut coef in self.coefs(var).into_iter().rev() {
            for term in &mut coef.terms {
                term.val = term.val.clone() * val_pow.clone();
            }
            new = new + coef;

            val_pow = val_pow * val.clone();
        }

        new
    }

    pub fn norm(&self) -> Poly {
        use num::{BigInt, BigRational, integer::gcd};
        let mut new = self.clone();

        let mut all_terms_den_gcd = BigInt::from(1);
        let mut all_terms_num_gcd = BigInt::from(1);

        if let Some(t) = new.terms.last() {
            all_terms_num_gcd = t.val.0.numer().clone();
            all_terms_den_gcd = t.val.0.denom().clone();
        }

        for term in &new.terms {
            all_terms_den_gcd = gcd(all_terms_den_gcd, term.val.0.denom().clone());
            all_terms_num_gcd = gcd(all_terms_num_gcd, term.val.0.numer().clone());
        }

        if let Some(t) = new.terms.last()
            && t.val.0.is_negative()
        {
            all_terms_num_gcd = -all_terms_num_gcd;
        }

        for term in &mut new.terms {
            term.val.0 = BigRational::new(
                term.val.0.numer().clone() / all_terms_num_gcd.clone(),
                term.val.0.denom().clone() / all_terms_den_gcd.clone(),
            );
        }

        new
    }

    pub fn format(&self, var_dict: &[String]) -> String {
        use num::ToPrimitive;

        let mut s = String::new();
        if self.terms.is_empty() {
            write!(s, "0").unwrap();
        }

        for (i, Mono { val, vars }) in (self.terms).iter().rev().enumerate() {
            let coef: f64 = val.0.to_f64().unwrap_or(f64::NAN);
            if coef != 1. || vars.is_empty() {
                if coef < 0. {
                    if coef == -1. && !vars.is_empty() {
                        if i == 0 {
                            write!(s, "-").unwrap();
                        } else {
                            write!(s, " - ").unwrap();
                        }
                    } else if i == 0 {
                        write!(s, "{coef}").unwrap();
                    } else {
                        write!(s, " - {}", -coef).unwrap();
                    }
                } else if i == 0 {
                    write!(s, "{coef}").unwrap();
                } else {
                    write!(s, " + {coef}").unwrap();
                }
            } else if i != 0 {
                write!(s, " + ").unwrap();
            }

            for (var, pow) in vars {
                if *pow == 1 {
                    write!(s, "{}", var_dict[*var]).unwrap();
                } else {
                    write!(s, "{}^{pow}", var_dict[*var]).unwrap();
                }
            }
        }

        s
    }
}

#[cfg(test)]
mod tests {
    use super::{Poly, Rat};

    #[test]
    fn coefs() {
        let var_dict = vec!["x".to_string(), "y".to_string(), "z".to_string()];

        let a = Poly::var(0, 4);
        let b = Poly::var(0, 2) * Poly::constant(Rat::from(3));
        let c = Poly::var(0, 2) * Poly::var(2, 3) * Poly::constant(Rat::from(5));
        let d = Poly::var(1, 1) * Poly::var(0, 1) * Poly::constant(Rat::from(4));
        let e = Poly::var(2, 1);
        let f = Poly::constant(Rat::from(2));

        let g = a + b + c + d + e + f;

        assert_eq!(
            "5x^2z^3 + x^4 + 3x^2 + 4xy + z + 2",
            format!("{}", g.format(&var_dict))
        );

        assert_eq!(
            "[\"1\", \"0\", \"5z^3 + 3\", \"4y\", \"z + 2\"]",
            format!(
                "{:?}",
                g.coefs(0)
                    .iter()
                    .map(|p| p.format(&var_dict))
                    .collect::<Vec<_>>()
            )
        );

        assert_eq!(g, Poly::from_uni_fmt(g.coefs(0), 0));
    }

    #[test]
    fn eval() {
        let var_dict = vec!["x".to_string(), "y".to_string(), "z".to_string()];

        let a = Poly::var(0, 4);
        let b = Poly::var(0, 2) * Poly::constant(Rat::from(3));
        let c = Poly::var(0, 2) * Poly::var(2, 3) * Poly::constant(Rat::from(5));
        let d = Poly::var(1, 1) * Poly::var(0, 1) * Poly::constant(Rat::from(4));
        let e = Poly::var(2, 1);
        let f = Poly::constant(Rat::from(2));

        let g = a + b + c + d + e + f;

        assert_eq!(
            "5x^2z^3 + x^4 + 3x^2 + 4xy + z + 2",
            format!("{}", g.format(&var_dict))
        );

        assert_eq!(
            "20z^3 + 8y + z + 30",
            format!("{}", g.eval(0, Rat::from(2)).format(&var_dict))
        );
    }
}
