use serde::{Serialize, Serializer};
use std::cmp::Ordering;
use std::fmt;
use std::ops;

#[derive(Clone, Debug)]
pub struct Term {
    coef: i64,
    vars: Vec<(usize, u8)>,
}

// returns var indices from dividend_var_dict
fn monomial_div(
    dividend: Term,
    dividend_var_dict: &[String],
    divisor: Term,
    divisor_var_dict: &[String],
) -> Option<Term> {
    if matches!(divisor, Term { coef: 0, .. }) {
        None
    } else if matches!(dividend, Term { coef: 0, .. }) {
        Some(Term {
            coef: 0,
            vars: vec![],
        })
    } else if dividend.coef % divisor.coef == 0 {
        let coef = dividend.coef / divisor.coef;

        let mut dividend_var_iter = dividend.vars.iter().peekable();
        let mut divisor_var_iter = divisor.vars.iter().peekable();
        let mut vars = vec![];
        while let Some((divisor_var, divisor_pow)) = divisor_var_iter.peek() {
            if let Some((dividend_var, dividend_pow)) = dividend_var_iter.peek() {
                match dividend_var_dict[*dividend_var].cmp(&divisor_var_dict[*divisor_var]) {
                    Ordering::Equal => match dividend_pow.cmp(divisor_pow) {
                        Ordering::Greater => {
                            vars.push((*dividend_var, dividend_pow - divisor_pow));
                            dividend_var_iter.next();
                            divisor_var_iter.next();
                            continue;
                        }
                        Ordering::Equal => {
                            dividend_var_iter.next();
                            divisor_var_iter.next();
                            continue;
                        }
                        Ordering::Less => return None,
                    },
                    Ordering::Less => {
                        vars.push((*dividend_var, *dividend_pow));
                        dividend_var_iter.next();
                        continue;
                    }
                    Ordering::Greater => {
                        return None;
                    }
                }
            }

            return None;
        }

        for (dividend_var, dividend_pow) in dividend_var_iter {
            vars.push((*dividend_var, *dividend_pow));
        }

        Some(Term { coef, vars })
    } else {
        None
    }
}

pub fn grevlex(
    lhs: &Term,
    lhs_var_dict: &[String],
    rhs: &Term,
    rhs_var_dict: &[String],
) -> Ordering {
    let lhs_total_degree = lhs.vars.iter().fold(0, |acc, (_, pow)| acc + pow);
    let rhs_total_degree = rhs.vars.iter().fold(0, |acc, (_, pow)| acc + pow);

    match lhs_total_degree.cmp(&rhs_total_degree) {
        Ordering::Less => Ordering::Greater,
        Ordering::Greater => Ordering::Less,
        Ordering::Equal => {
            for ((lhs_var, lhs_pow), (rhs_var, rhs_pow)) in lhs.vars.iter().zip(&rhs.vars) {
                match lhs_var_dict[*lhs_var].cmp(&rhs_var_dict[*rhs_var]) {
                    Ordering::Less => return Ordering::Less,
                    Ordering::Greater => return Ordering::Greater,
                    Ordering::Equal => match lhs_pow.cmp(rhs_pow) {
                        Ordering::Less => return Ordering::Greater,
                        Ordering::Greater => return Ordering::Less,
                        Ordering::Equal => continue,
                    },
                }
            }

            Ordering::Equal
        }
    }
}

#[derive(Clone)]
pub struct Poly {
    terms: Vec<Term>,
    var_dict: Vec<String>,
}

impl Poly {
    pub fn var(var: &str, pow: u8) -> Self {
        if pow == 0 {
            Self {
                terms: vec![Term {
                    coef: 1,
                    vars: vec![],
                }],
                var_dict: vec![],
            }
        } else {
            Self {
                terms: vec![Term {
                    coef: 1,
                    vars: vec![(0, pow)],
                }],
                var_dict: vec![var.to_string()],
            }
        }
    }

    pub fn constant(val: i64) -> Self {
        Self {
            terms: if val == 0 {
                vec![]
            } else {
                vec![Term {
                    coef: val,
                    vars: vec![],
                }]
            },
            var_dict: vec![],
        }
    }

    pub fn leading_term(&self) -> Term {
        match self.terms.first() {
            Some(t) => t.clone(),
            None => Term {
                coef: 0,
                vars: vec![],
            },
        }
    }

    pub fn leading_term_poly(&self) -> Poly {
        let mut var_dict = vec![];

        let mut term = self.leading_term();

        for (var, _) in &mut term.vars {
            let new_var_index = match self.var_dict.iter().position(|p| p == &self.var_dict[*var]) {
                Some(i) => i,
                None => {
                    var_dict.push(self.var_dict[*var].clone());
                    var_dict.len() - 1
                }
            };

            *var = new_var_index;
        }

        Poly {
            terms: vec![term],
            var_dict,
        }
    }

    pub fn get_constant_val(&self) -> Option<i64> {
        match self.terms.len() {
            0 => Some(0),
            1 => {
                if self.terms[0].vars.is_empty() {
                    Some(self.terms[0].coef)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn append_term(&mut self, term: Term, term_var_dict: &[String]) {
        let mut new_vars = vec![];
        for (var, pow) in term.vars {
            let new_var_index = match self.var_dict.iter().position(|p| p == &term_var_dict[var]) {
                Some(i) => i,
                None => {
                    self.var_dict.push(term_var_dict[var].clone());
                    self.var_dict.len() - 1
                }
            };

            new_vars.push((new_var_index, pow));
        }

        self.terms.push(Term {
            coef: term.coef,
            vars: new_vars,
        });
    }

    pub fn compound_divide(mut self, divisors: Vec<Poly>) -> (Vec<Poly>, Poly) {
        if divisors.is_empty() {
            return (vec![], self);
        }

        let mut rem = Poly::constant(0);
        let mut quotients: Vec<_> = std::iter::repeat(Poly::constant(0))
            .take(divisors.len())
            .collect();

        let mut curr_divisor = 0;

        while self.get_constant_val() != Some(0) {
            let self_lt = self.leading_term();
            let div_lt = divisors[curr_divisor].leading_term();
            let self_over_div_lt = monomial_div(
                self_lt.clone(),
                &self.var_dict,
                div_lt,
                &divisors[curr_divisor].var_dict,
            );

            if let Some(self_over_div_lt) = self_over_div_lt {
                quotients[curr_divisor].append_term(self_over_div_lt.clone(), &self.var_dict);

                let self_over_div_lt = Poly {
                    terms: vec![self_over_div_lt],
                    var_dict: self.var_dict.clone(),
                };

                self = self - (self_over_div_lt * divisors[curr_divisor].clone());
                curr_divisor = 0;
            } else {
                curr_divisor += 1;
            }

            if curr_divisor == divisors.len() {
                let self_lt = Poly {
                    terms: vec![self_lt],
                    var_dict: self.var_dict.clone(),
                };

                self = self - self_lt.clone();

                rem = rem + self_lt;
                curr_divisor = 0;
            }
        }

        (quotients, rem)
    }
}

impl ops::Add<Poly> for Poly {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        let mut new = Self::constant(0);

        let mut lhs_term_iter = self.terms.into_iter().peekable();
        let mut rhs_term_iter = rhs.terms.into_iter().peekable();

        loop {
            if let Some(lhs_term) = lhs_term_iter.peek() {
                if let Some(rhs_term) = rhs_term_iter.peek() {
                    match grevlex(lhs_term, &self.var_dict, rhs_term, &rhs.var_dict) {
                        Ordering::Equal => {
                            let new_coef = lhs_term.coef + rhs_term.coef;
                            if new_coef != 0 {
                                new.append_term(
                                    Term {
                                        coef: new_coef,
                                        vars: lhs_term.vars.clone(),
                                    },
                                    &self.var_dict,
                                );
                            }
                            lhs_term_iter.next();
                            rhs_term_iter.next();
                        }
                        Ordering::Greater => {
                            new.append_term(rhs_term.clone(), &rhs.var_dict);
                            rhs_term_iter.next();
                        }
                        Ordering::Less => {
                            new.append_term(lhs_term.clone(), &self.var_dict);
                            lhs_term_iter.next();
                        }
                    }
                } else {
                    new.append_term(lhs_term.clone(), &self.var_dict);
                    lhs_term_iter.next();
                }
            } else if let Some(rhs_term) = rhs_term_iter.next() {
                new.append_term(rhs_term, &rhs.var_dict);
            } else {
                break;
            }
        }

        new
    }
}

#[allow(clippy::suspicious_arithmetic_impl)]
impl ops::Mul<Poly> for Poly {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        let mut new = Self::constant(0);

        for lhs_term in self.terms {
            for rhs_term in &rhs.terms {
                let mut new_term = Term {
                    coef: lhs_term.coef * rhs_term.coef,
                    vars: vec![],
                };

                let mut new_var_dict = vec![];

                for var in &lhs_term.vars {
                    let new_var_index =
                        match new_var_dict.iter().position(|p| p == &self.var_dict[var.0]) {
                            Some(i) => i,
                            None => {
                                new_var_dict.push(self.var_dict[var.0].clone());
                                new_var_dict.len() - 1
                            }
                        };

                    let new_var_term_index =
                        match new_term.vars.iter().position(|p| p.0 == new_var_index) {
                            Some(i) => i,
                            None => {
                                new_term.vars.push((new_var_index, 0));
                                new_term.vars.len() - 1
                            }
                        };

                    new_term.vars[new_var_term_index].1 += var.1;
                }

                for var in &rhs_term.vars {
                    let new_var_index =
                        match new_var_dict.iter().position(|p| p == &rhs.var_dict[var.0]) {
                            Some(i) => i,
                            None => {
                                new_var_dict.push(rhs.var_dict[var.0].clone());
                                new_var_dict.len() - 1
                            }
                        };

                    let new_var_term_index =
                        match new_term.vars.iter().position(|p| p.0 == new_var_index) {
                            Some(i) => i,
                            None => {
                                new_term.vars.push((new_var_index, 0));
                                new_term.vars.len() - 1
                            }
                        };

                    new_term.vars[new_var_term_index].1 += var.1;
                }

                new_term
                    .vars
                    .sort_by_cached_key(|(v, _)| new_var_dict[*v].clone());

                new = new
                    + Self {
                        terms: vec![new_term],
                        var_dict: new_var_dict,
                    };
            }
        }

        new
    }
}

impl ops::Sub<Poly> for Poly {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        self + (Self::constant(-1) * rhs)
    }
}

impl PartialEq for Poly {
    fn eq(&self, other: &Self) -> bool {
        if self.terms.len() != other.terms.len() {
            return false;
        }

        for (lhs_term, rhs_term) in self.terms.iter().zip(&other.terms) {
            if lhs_term.coef != rhs_term.coef
                || grevlex(lhs_term, &self.var_dict, rhs_term, &other.var_dict) != Ordering::Equal
            {
                return false;
            }
        }

        true
    }
}

impl Eq for Poly {}

impl fmt::Debug for Poly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.terms.is_empty() {
            write!(f, "0")?
        }

        for (i, Term { coef, vars }) in (self.terms).iter().enumerate() {
            if *coef != 1 || vars.is_empty() {
                if *coef < 0 {
                    if *coef == -1 && !vars.is_empty() {
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
    use super::{Poly, Term};
    use crate::solver::poly::monomial_div;

    use rand::prelude::*;

    #[test]
    fn arith() {
        let a = Poly::var("a", 2) * Poly::constant(3);
        let b = Poly::var("a", 1) * Poly::constant(4);
        let c = Poly::constant(2);

        assert_eq!("3a^2 + 4a + 2", format!("{:?}", a + b + c));

        let d = Poly::constant(4);
        let e = Poly::constant(5);

        assert_eq!("-1", format!("{:?}", d - e));

        let f = Poly::constant(2) * Poly::var("a", 2);
        let g = Poly::constant(5) * Poly::var("a", 1);
        let h = Poly::constant(3) * Poly::var("b", 1);

        assert_eq!("2a^2 - 5a + 3b", format!("{:?}", f - g + h));
    }

    #[test]
    fn equality() {
        let a = (Poly::var("a", 1) + Poly::constant(1)) * (Poly::var("a", 1) + Poly::constant(1));
        let b = Poly::var("a", 2) + Poly::constant(2) * Poly::var("a", 1) + Poly::constant(1);

        assert!(a == b);
    }

    #[test]
    fn grevlex_ordering() {
        // from https://people.sc.fsu.edu/~jburkardt/c_src/monomial/monomial.html
        let required_output = " #  monomial   expon
--  ---------  -----
 1             0 0 0

 2        z    0 0 1
 3     y       0 1 0
 4  x          1 0 0

 5        z^2  0 0 2
 6     y  z    0 1 1
 7     y^2     0 2 0
 8  x     z    1 0 1
 9  x  y       1 1 0
10  x^2        2 0 0

11        z^3  0 0 3
12     y  z^2  0 1 2
13     y^2z    0 2 1
14     y^3     0 3 0
15  x     z^2  1 0 2
16  x  y  z    1 1 1
17  x  y^2     1 2 0
18  x^2   z    2 0 1
19  x^2y       2 1 0
20  x^3        3 0 0

21        z^4  0 0 4
22     y  z^3  0 1 3
23     y^2z^2  0 2 2
24     y^3z    0 3 1
25     y^4     0 4 0
26  x     z^3  1 0 3
27  x  y  z^2  1 1 2
28  x  y^2z    1 2 1
29  x  y^3     1 3 0
30  x^2   z^2  2 0 2
31  x^2y  z    2 1 1
32  x^2y^2     2 2 0
33  x^3   z    3 0 1
34  x^3y       3 1 0
35  x^4        4 0 0

36        z^5  0 0 5
";

        let mut output = String::new();

        output.push_str(" #  monomial   expon\n");
        output.push_str("--  ---------  -----\n");

        let mut x = Poly::constant(0);

        let pm = 5;

        for x_i in 0..pm + 1 {
            for y_i in 0..pm + 1 {
                for z_i in 0..pm + 1 {
                    if x_i + y_i + z_i <= pm {
                        let mut term = Poly::constant(1);
                        if x_i > 0 {
                            term = term * Poly::var("x", x_i)
                        }
                        if y_i > 0 {
                            term = term * Poly::var("y", y_i)
                        }
                        if z_i > 0 {
                            term = term * Poly::var("z", z_i)
                        }

                        x = x + term;
                    }
                }
            }
        }

        let mut level = (0, 1, 2);

        for (i, term) in x.terms.iter().rev().enumerate() {
            if i == 36 {
                break;
            }
            let (x_pow_str, x_pow) = match term
                .vars
                .iter()
                .position(|(var, _)| x.var_dict[*var] == "x")
            {
                Some(i) => (
                    if term.vars[i].1 == 1 {
                        "x  ".to_string()
                    } else {
                        format!("x^{}", term.vars[i].1)
                    },
                    term.vars[i].1,
                ),
                None => ("   ".to_string(), 0),
            };

            let (y_pow_str, y_pow) = match term
                .vars
                .iter()
                .position(|(var, _)| x.var_dict[*var] == "y")
            {
                Some(i) => (
                    if term.vars[i].1 == 1 {
                        "y  ".to_string()
                    } else {
                        format!("y^{}", term.vars[i].1)
                    },
                    term.vars[i].1,
                ),
                None => ("   ".to_string(), 0),
            };

            let (z_pow_str, z_pow) = match term
                .vars
                .iter()
                .position(|(var, _)| x.var_dict[*var] == "z")
            {
                Some(i) => (
                    if term.vars[i].1 == 1 {
                        "z  ".to_string()
                    } else {
                        format!("z^{}", term.vars[i].1)
                    },
                    term.vars[i].1,
                ),
                None => ("   ".to_string(), 0),
            };

            output.push_str(&format!(
                "{:2?}  {x_pow_str}{y_pow_str}{z_pow_str}  {x_pow} {y_pow} {z_pow}\n",
                i + 1
            ));
            if i + 1 == level.0 + level.1 {
                output.push_str("\n");
                level = (i + 1, level.1 + level.2, level.2 + 1);
            }
        }

        assert_eq!(output, required_output);
    }

    #[test]
    fn div() {
        let vars = vec![
            "w".to_string(),
            "x".to_string(),
            "y".to_string(),
            "z".to_string(),
        ];
        let t1 = Term {
            coef: 4,
            vars: vec![(0, 2), (1, 3), (2, 4)],
        };
        let t2 = Term {
            coef: 2,
            vars: vec![(0, 1), (2, 2)],
        };
        if let Some(q) = monomial_div(t1, &vars, t2, &vars) {
            assert_eq!(q.vars, vec![(0, 1), (1, 3), (2, 2)]);
            assert_eq!(q.coef, 2);
        } else {
            panic!()
        }

        let dividend = Poly::var("x", 2) * Poly::var("y", 1)
            + Poly::var("x", 1) * Poly::var("y", 2)
            + Poly::var("y", 2);
        let divisor_1 = Poly::var("y", 2) - Poly::constant(1);
        let divisor_2 = Poly::var("x", 1) * Poly::var("y", 1) - Poly::constant(1);

        let (quotients, rem) = dividend
            .clone()
            .compound_divide(vec![divisor_1.clone(), divisor_2.clone()]);

        let solution = format!(
            "({:?})({:?}) + ({:?})({:?}) + {:?} = {:?}",
            quotients[0], divisor_1, quotients[1], divisor_2, rem, dividend
        );

        assert_eq!(
            solution,
            "(x + 1)(y^2 - 1) + (x)(xy - 1) + 2x + 1 = x^2y + xy^2 + y^2"
        );

        let a = Poly::constant(-3) * Poly::var("x", 1) * Poly::var("y", 1) + Poly::constant(1);
        println!("{:?}", Poly::constant(0) + a);
    }

    #[test]
    fn arith_fuzz() {
        let mut rng = SmallRng::seed_from_u64(1);

        fn create_random_poly(rng: &mut SmallRng, term_max: i32) -> Poly {
            let mut p = Poly::constant(0);

            for _ in 0..rng.gen_range(0..term_max) {
                let coef = rng.gen_range(-6..6);
                let xpow = rng.gen_range(0..3);
                let ypow = rng.gen_range(0..1);
                let zpow = rng.gen_range(0..2);

                p = p
                    + Poly::constant(coef) * Poly::var("x", xpow) * Poly::var("y", ypow)
                    + Poly::var("z", zpow);
            }

            p
        }

        for _ in 0..1000 {
            let dividend = create_random_poly(&mut rng, 2);
            let n_divs = rng.gen_range(0..4);
            let divisors: Vec<_> = std::iter::repeat_with(|| create_random_poly(&mut rng, 1))
                .take(n_divs)
                .collect();

            let (quotients, rem) = dividend.clone().compound_divide(divisors.clone());

            let calculated_dividend = quotients
                .clone()
                .into_iter()
                .zip(divisors.clone())
                .fold(Poly::constant(0), |acc, (x, y)| acc + x * y)
                + rem.clone();

            assert_eq!(
                calculated_dividend, dividend,
                "tried dividing {:?} by {:?}; got {:?} and rem {:?}",
                dividend, divisors, quotients, rem
            );
        }
    }
}
