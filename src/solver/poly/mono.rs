use std::cmp::Ordering;
use super::super::Rat;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Mono {
    pub val: Rat,
    pub vars: Vec<(usize, u64)>,
}

impl Mono {
    pub fn deg(&self, var: usize) -> usize {
        self.vars
            .iter()
            .find_map(|(v, pow)| match var.cmp(v) {
                Ordering::Equal => Some(*pow as usize),
                Ordering::Greater => Some(0),
                Ordering::Less => None,
            })
            .unwrap_or(0)
    }

    pub fn coef(&self, var: usize) -> (usize, Mono) {
        let mut new_vars = vec![];
        let mut deg = 0;

        for (v, pow) in &self.vars {
            if *v == var {
                deg = *pow as usize;
            } else {
                new_vars.push((*v, *pow));
            }
        }

        (
            deg,
            Mono {
                val: self.val.clone(),
                vars: new_vars,
            },
        )
    }
}

#[cfg(test)]
pub fn print_exps(term: &Mono, var_dict: &[String]) -> String {
    use std::fmt::Write;

    let mut res = String::new();

    for (var, pow) in &term.vars {
        if *pow == 1 {
            write!(res, "{}", var_dict[*var]).unwrap();
        } else {
            write!(res, "{}^{pow}", var_dict[*var]).unwrap();
        }
    }

    res
}

pub fn grevlex(lhs: &Mono, rhs: &Mono) -> Ordering {
    let lhs_total_degree = lhs.vars.iter().fold(0, |acc, (_, pow)| acc + pow);
    let rhs_total_degree = rhs.vars.iter().fold(0, |acc, (_, pow)| acc + pow);

    match lhs_total_degree.cmp(&rhs_total_degree) {
        Ordering::Less => Ordering::Less,
        Ordering::Greater => Ordering::Greater,
        Ordering::Equal => {
            for ((lhs_var, lhs_pow), (rhs_var, rhs_pow)) in lhs.vars.iter().zip(&rhs.vars) {
                match lhs_var.cmp(rhs_var) {
                    Ordering::Less => return Ordering::Greater,
                    Ordering::Greater => return Ordering::Less,
                    Ordering::Equal => match lhs_pow.cmp(rhs_pow) {
                        Ordering::Less => return Ordering::Less,
                        Ordering::Greater => return Ordering::Greater,
                        Ordering::Equal => continue,
                    },
                }
            }

            Ordering::Equal
        }
    }
}

pub fn monomial_div(lhs: &Mono, rhs: &Mono) -> Option<Mono> {
    if rhs.val.is_zero() {
        None
    } else if lhs.val.is_zero() {
        Some(Mono {
            val: Rat::zero(),
            vars: vec![],
        })
    } else {
        let mut lhs_var_iter = lhs.vars.iter().peekable();
        let mut rhs_var_iter = rhs.vars.iter().peekable();
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

        Some(Mono {
            val: lhs.val.clone() / rhs.val.clone(),
            vars,
        })
    }
}

pub fn monomial_mul(lhs: &Mono, rhs: &Mono) -> Mono {
    let val = if lhs.val.is_zero() || rhs.val.is_zero() {
        return Mono {
            val: Rat::zero(),
            vars: vec![],
        };
    } else {
        lhs.val.clone() * rhs.val.clone()
    };

    let mut vars = vec![];

    let mut lhs_var_ind = 0;
    let mut rhs_var_ind = 0;

    while lhs_var_ind < lhs.vars.len() || rhs_var_ind < rhs.vars.len() {
        if lhs_var_ind < lhs.vars.len() && rhs_var_ind < rhs.vars.len() {
            match lhs.vars[lhs_var_ind].0.cmp(&rhs.vars[rhs_var_ind].0) {
                Ordering::Equal => {
                    vars.push((
                        lhs.vars[lhs_var_ind].0,
                        lhs.vars[lhs_var_ind].1 + rhs.vars[rhs_var_ind].1,
                    ));
                    lhs_var_ind += 1;
                    rhs_var_ind += 1;
                }
                Ordering::Greater => {
                    vars.push(rhs.vars[rhs_var_ind]);
                    rhs_var_ind += 1;
                }
                Ordering::Less => {
                    vars.push(lhs.vars[lhs_var_ind]);
                    lhs_var_ind += 1;
                }
            }
        } else if lhs_var_ind < lhs.vars.len() {
            vars.push(lhs.vars[lhs_var_ind]);
            lhs_var_ind += 1;
        } else if rhs_var_ind < rhs.vars.len() {
            vars.push(rhs.vars[rhs_var_ind]);
            rhs_var_ind += 1;
        }
    }

    Mono { val, vars }
}

// ignore coef, just applied to vars
pub fn monomial_lcm(lhs: Mono, rhs: Mono) -> Mono {
    let mut vars = vec![];

    let mut lhs_var_ind = 0;
    let mut rhs_var_ind = 0;

    while lhs_var_ind < lhs.vars.len() || rhs_var_ind < rhs.vars.len() {
        if lhs_var_ind < lhs.vars.len() && rhs_var_ind < rhs.vars.len() {
            match lhs.vars[lhs_var_ind].0.cmp(&rhs.vars[rhs_var_ind].0) {
                Ordering::Equal => {
                    vars.push((
                        lhs.vars[lhs_var_ind].0,
                        lhs.vars[lhs_var_ind].1.max(rhs.vars[rhs_var_ind].1),
                    ));
                    lhs_var_ind += 1;
                    rhs_var_ind += 1;
                }
                Ordering::Greater => {
                    vars.push(rhs.vars[rhs_var_ind]);
                    rhs_var_ind += 1;
                }
                Ordering::Less => {
                    vars.push(lhs.vars[lhs_var_ind]);
                    lhs_var_ind += 1;
                }
            }
        } else if lhs_var_ind < lhs.vars.len() {
            vars.push(lhs.vars[lhs_var_ind]);
            lhs_var_ind += 1;
        } else if rhs_var_ind < rhs.vars.len() {
            vars.push(rhs.vars[rhs_var_ind]);
            rhs_var_ind += 1;
        }
    }

    Mono {
        val: Rat::one(),
        vars,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::prelude::*;

    #[test]
    fn ordering() {
        let var_dict = ["x".to_string(), "y".to_string(), "z".to_string()];
        let mut terms = vec![];

        for i in 0..4 {
            for j in 0..4 {
                for k in 0..4 {
                    let mut vars = vec![];
                    if i > 0 {
                        vars.push((0, i))
                    }
                    if j > 0 {
                        vars.push((1, j))
                    }
                    if k > 0 {
                        vars.push((2, k))
                    }

                    terms.push(Mono {
                        val: Rat::one(),
                        vars,
                    });
                }
            }
        }

        let expected_sort = "x^3y^3z^3
x^3y^3z^2
x^3y^2z^3
x^2y^3z^3
x^3y^3z
x^3y^2z^2
x^3yz^3
x^2y^3z^2
x^2y^2z^3
xy^3z^3
x^3y^3
x^3y^2z
x^3yz^2
x^3z^3
x^2y^3z
x^2y^2z^2
x^2yz^3
xy^3z^2
xy^2z^3
y^3z^3
x^3y^2
x^3yz
x^3z^2
x^2y^3
x^2y^2z
x^2yz^2
x^2z^3
xy^3z
xy^2z^2
xyz^3
y^3z^2
y^2z^3
x^3y
x^3z
x^2y^2
x^2yz
x^2z^2
xy^3
xy^2z
xyz^2
xz^3
y^3z
y^2z^2
yz^3
x^3
x^2y
x^2z
xy^2
xyz
xz^2
y^3
y^2z
yz^2
z^3
x^2
xy
xz
y^2
yz
z^2
x
y
z


"
        .split("\n")
        .collect::<Vec<_>>();

        terms.sort_by(|a, b| grevlex(a, b));

        for (i, term) in terms.iter().rev().enumerate() {
            assert_eq!(expected_sort[i], print_exps(&term, &var_dict));
        }
    }

    #[test]
    fn div_mul_fuzz() {
        let mut rng = SmallRng::seed_from_u64(1);

        fn random_mono(rng: &mut SmallRng, min_coef: i32, max_coef: i32) -> Mono {
            let coef = rng.gen_range(min_coef..max_coef);

            let mut vars = vec![];

            let wpow = rng.gen_range(0..3);
            if wpow > 0 {
                vars.push((0, wpow));
            }

            let xpow = rng.gen_range(0..1);
            if xpow > 0 {
                vars.push((1, xpow));
            }

            let ypow = rng.gen_range(0..1);
            if ypow > 0 {
                vars.push((2, ypow));
            }

            let zpow = rng.gen_range(0..2);
            if zpow > 0 {
                vars.push((3, zpow));
            }

            Mono {
                val: Rat::from(coef as i64),
                vars: if coef == 0 { vec![] } else { vars },
            }
        }

        for _i in 0..1000 {
            let a = random_mono(&mut rng, 6, 12);
            let b = random_mono(&mut rng, 0, 6);
            let c = monomial_div(&a, &b);
            if let Some(c) = c {
                assert_eq!(a, monomial_mul(&c, &b));
            }
        }
    }
}
