use serde::{Serialize, Serializer};
use std::fmt;

struct M {
    coef: i64,
    vars: Vec<(usize, u64)>,
}

pub struct Poly {
    var_dict: Vec<String>,
    terms: Vec<M>,
}

impl Poly {
    pub fn var(var: &str) -> Self {
        Self {
            terms: vec![M {
                coef: 1,
                vars: vec![(0, 1)],
            }],
            var_dict: vec![var.to_string()],
        }
    }

    pub fn constant(val: i64) -> Self {
        Self {
            terms: vec![M {
                coef: val,
                vars: vec![],
            }],
            var_dict: vec![],
        }
    }
}

impl fmt::Debug for Poly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.terms.is_empty() {
            write!(f, "0")?
        }

        for (i, M { coef, vars }) in (self.terms).iter().enumerate() {
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
                    write!(f, "{}", self.var_dict[*var])?;
                } else {
                    write!(f, "{}^{pow}", self.var_dict[*var])?;
                }
            }
        }

        Ok(())
    }
}

impl Serialize for Poly {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(&format!("{self:?}"))
    }
}

#[cfg(test)]
mod tests {
    use super::Poly;

    #[test]
    fn arith() {
        let a = Poly::constant(10);
        let b = Poly::var("b");

        assert_eq!("10, b", format!("{:?}, {:?}", a, b));
    }
}
