use std::fmt;
use std::ops;

use serde::{Serialize, Serializer};

type Term = (i64, Vec<(String, usize)>);

fn mul_term(lhs: &Term, rhs: &Term) -> Term {
    let (lhs_coef, lhs_vars) = lhs;
    let (rhs_coef, rhs_vars) = rhs;

    let mut new_vars = lhs_vars.clone();

    for (rhs_var, rhs_pow) in rhs_vars {
        let mut found_var = false;
        for new_var in &mut new_vars {
            if &new_var.0 == rhs_var {
                new_var.1 += rhs_pow;
                found_var = true;
                break;
            }
        }
        if !found_var {
            new_vars.push((rhs_var.clone(), *rhs_pow));
        }
    }

    (lhs_coef * rhs_coef, new_vars)
}

fn eq_vars(lhs: &Vec<(String, usize)>, rhs: &Vec<(String, usize)>) -> bool {
    if rhs.len() != lhs.len() {
        return false;
    }

    for (rhs_var, rhs_pow) in rhs {
        let mut found = false;

        for (lhs_var, lhs_pow) in lhs {
            if lhs_var == rhs_var {
                if lhs_pow == rhs_pow {
                    found = true;
                    break;
                } else {
                    return false;
                }
            }
        }

        if !found {
            return false;
        }
    }

    true
}

#[derive(Clone)]
pub struct Ind {
    terms: Vec<Term>,
}

impl Serialize for Ind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(&format!("{self:?}"))
    }
}

impl fmt::Debug for Ind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.terms.is_empty() {
            write!(f, "0")?
        }

        for (i, (coef, vars)) in (self.terms).iter().enumerate() {
            if coef != &1 || vars.is_empty() {
                if coef < &0 {
                    if i == 0 {
                        write!(f, "{coef}")?;
                    } else {
                        write!(f, " - {}", -coef)?;
                    }
                } else if i == 0 {
                    write!(f, "{coef}")?;
                } else {
                    write!(f, " + {coef}")?;
                }
            }

            for (var, pow) in vars {
                if *pow == 1 {
                    write!(f, "{var}")?;
                } else {
                    write!(f, "{var}^{pow}")?;
                }
            }
        }

        Ok(())
    }
}

#[allow(dead_code)]
impl Ind {
    pub fn var(var: &str) -> Self {
        Self {
            terms: vec![(1, vec![(var.to_string(), 1)])],
        }
    }

    pub fn constant(val: i64) -> Self {
        Self {
            terms: vec![(val, vec![])],
        }
    }

    fn filter_zeros(&self) -> Ind {
        let mut new_terms = vec![];

        for term in &self.terms {
            if term.0 != 0 {
                new_terms.push(term.clone())
            }
        }

        Ind { terms: new_terms }
    }

    pub fn constant_val(&self) -> Option<i64> {
        if self.terms.len() == 1 {
            if self.terms[0].1.is_empty() {
                Some(self.terms[0].0)
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl ops::Add<Ind> for Ind {
    type Output = Self;

    fn add(self, rhs: Ind) -> Self {
        let mut new_terms: Vec<Term> = vec![];

        for (lhs_coef, lhs_term) in &self.terms {
            let mut found_term = false;

            for (rhs_coef, rhs_term) in &rhs.terms {
                if eq_vars(lhs_term, rhs_term) {
                    let new_coef = lhs_coef + rhs_coef;
                    new_terms.push((new_coef, lhs_term.clone()));
                    found_term = true;
                    break;
                }
            }

            if !found_term {
                new_terms.push((*lhs_coef, lhs_term.clone()));
            }
        }

        for (rhs_coef, rhs_term) in &rhs.terms {
            let mut found_term = false;

            for (_, new_term) in &new_terms {
                if rhs_term == new_term {
                    found_term = true;
                    break;
                }
            }

            if !found_term {
                new_terms.push((*rhs_coef, rhs_term.clone()));
            }
        }

        Ind { terms: new_terms }.filter_zeros()
    }
}

impl ops::Sub<Ind> for Ind {
    type Output = Self;

    fn sub(self, rhs: Ind) -> Self {
        self + (Ind::constant(-1) * rhs)
    }
}

#[allow(clippy::suspicious_arithmetic_impl)]
impl ops::Mul<Ind> for Ind {
    type Output = Self;

    fn mul(self, rhs: Ind) -> Self {
        let mut new = Ind::constant(0);

        for lhs_term in self.terms {
            for rhs_term in &rhs.terms {
                new = new
                    + Ind {
                        terms: vec![mul_term(&lhs_term, rhs_term)],
                    };
            }
        }

        new.filter_zeros()
    }
}

impl PartialEq for Ind {
    fn eq(&self, other: &Self) -> bool {
        for lhs_term in &self.terms {
            let mut found_term = false;
            for rhs_term in &other.terms {
                if eq_vars(&lhs_term.1, &rhs_term.1) {
                    if lhs_term.0 == rhs_term.0 {
                        found_term = true;
                        break;
                    } else {
                        return false;
                    }
                }
            }

            if !found_term {
                return false;
            }
        }

        true
    }
}

impl Eq for Ind {}

#[cfg(test)]
mod tests {
    use super::Ind;

    #[test]
    fn ind_arith() {
        let a = Ind::var("a") * Ind::var("a") * Ind::constant(3);
        let b = Ind::var("a") * Ind::constant(4);
        let c = Ind::constant(2);

        assert_eq!("3a^2 + 4a + 2", format!("{:?}", a + b + c));

        let d = Ind::constant(4);
        let e = Ind::constant(5);

        assert_eq!("-1", format!("{:?}", d - e));

        let f = Ind::constant(2) * Ind::var("a") * Ind::var("a");
        let g = Ind::constant(5) * Ind::var("a");
        let h = Ind::constant(3) * Ind::var("b");

        assert_eq!("2a^2 - 5a + 3b", format!("{:?}", f - g + h));
    }

    #[test]
    fn equality() {
        let a = (Ind::var("a") + Ind::constant(1)) * (Ind::var("a") + Ind::constant(1));
        let b = Ind::var("a") * Ind::var("a") + Ind::constant(2) * Ind::var("a") + Ind::constant(1);

        assert!(a == b);
    }
}
