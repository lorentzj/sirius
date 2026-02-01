use std::cmp::Ordering;
use std::collections::VecDeque;
use std::ops;

use super::mono::*;
use super::*;

impl ops::Add<Poly> for Poly {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        if self.terms.is_empty() {
            return rhs;
        }

        if rhs.terms.is_empty() {
            return self;
        }

        let mut new_terms = vec![];

        let mut lhs_term_iter = self.terms.into_iter().peekable();
        let mut rhs_term_iter = rhs.terms.into_iter().peekable();

        loop {
            if let Some(lhs_term) = lhs_term_iter.peek() {
                if let Some(rhs_term) = rhs_term_iter.peek() {
                    match grevlex(lhs_term, rhs_term) {
                        Ordering::Equal => {
                            let new_val = lhs_term.val.clone() + rhs_term.val.clone();
                            if !new_val.is_zero() {
                                new_terms.push(Mono {
                                    val: new_val,
                                    vars: lhs_term.vars.clone(),
                                });
                            }
                            lhs_term_iter.next();
                            rhs_term_iter.next();
                        }

                        Ordering::Greater => {
                            new_terms.push(rhs_term.clone());
                            rhs_term_iter.next();
                        }
                        Ordering::Less => {
                            new_terms.push(lhs_term.clone());
                            lhs_term_iter.next();
                        }
                    }
                } else {
                    new_terms.push(lhs_term.clone());
                    lhs_term_iter.next();
                }
            } else if let Some(rhs_term) = rhs_term_iter.next() {
                new_terms.push(rhs_term);
            } else {
                break;
            }
        }

        Self { terms: new_terms }
    }
}

impl ops::Sub<Poly> for Poly {
    type Output = Self;

    fn sub(self, mut rhs: Self) -> Self {
        for term in &mut rhs.terms {
            term.val = term.val.clone() * Rat::from(-1);
        }

        self + rhs
    }
}

impl ops::Mul<Poly> for Poly {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        self.mul_ref(&rhs)
    }
}

impl Poly {
    pub fn mul_ref(&self, other: &Poly) -> Poly {
        let mut new = Self::constant(Rat::zero());

        for lhs_term in &self.terms {
            for rhs_term in &other.terms {
                let new_term = Poly {
                    terms: vec![monomial_mul(lhs_term, rhs_term)],
                };

                new = new + new_term;
            }
        }

        new
    }

    pub fn compound_divide(&self, divisors: &[Poly]) -> (Vec<Poly>, Poly) {
        if divisors.is_empty() {
            return (vec![], self.clone());
        }

        let mut dividend = self.clone();

        let mut rem = Poly::constant(Rat::zero());
        let mut quotients: Vec<VecDeque<Mono>> =
            std::iter::repeat_n(VecDeque::from(vec![]), divisors.len()).collect();

        let mut curr_divisor = 0;

        while !dividend.is_zero() {
            let self_lt = dividend.lt_mono();
            if !divisors[curr_divisor].terms.is_empty() {
                let div_lt = &divisors[curr_divisor].lt_mono();
                let self_over_div_lt = monomial_div(&self_lt, div_lt);

                if let Some(self_over_div_lt) = self_over_div_lt {
                    quotients[curr_divisor].push_front(self_over_div_lt.clone());

                    let self_over_div_lt = Poly {
                        terms: vec![self_over_div_lt],
                    };

                    dividend = dividend - (self_over_div_lt.mul_ref(&divisors[curr_divisor]));
                    curr_divisor = 0;
                } else {
                    curr_divisor += 1;
                }
            } else {
                curr_divisor += 1;
            }

            if curr_divisor == divisors.len() {
                let self_lt = Poly {
                    terms: vec![self_lt.clone()],
                };

                dividend.terms.pop();

                rem = rem + self_lt;
                curr_divisor = 0;
            }
        }

        let quotients = quotients
            .into_iter()
            .map(|v| Poly {
                terms: Vec::from(v),
            })
            .collect();

        (quotients, rem)
    }

    pub fn try_divide(&self, divisor: &Poly) -> Option<Poly> {
        let (quots, rem) = self.compound_divide(std::slice::from_ref(divisor));

        if rem.is_zero() {
            Some(quots[0].clone())
        } else {
            None
        }
    }

    pub fn derivative(&self, by: usize) -> Poly {
        let mut new_terms = vec![];
        for term in &self.terms {
            let mut new_term = Mono {
                val: term.val.clone(),
                vars: vec![],
            };
            let mut found = false;
            for (var, pow) in &term.vars {
                if *var == by {
                    found = true;
                    if *pow > 1 {
                        new_term.val = new_term.val * Rat::from(*pow as i64);
                        new_term.vars.push((*var, *pow - 1));
                    }
                } else {
                    new_term.vars.push((*var, *pow));
                }
            }

            if found {
                new_terms.push(new_term);
            }
        }

        Poly { terms: new_terms }
    }
}

#[cfg(test)]
mod tests {
    use super::Poly;
    use super::Rat;
    use rand::prelude::*;
    use std::rc::Rc;

    #[test]
    fn arith() {
        let var_dict = Rc::new(vec!["a".to_string(), "b".to_string(), "c".to_string()]);

        let a = Poly::var(0, 2) * Poly::constant(Rat::from(3));
        let b = Poly::var(1, 1) * Poly::constant(Rat::from(4));
        let c = Poly::constant(Rat::from(2));

        println!("{}", (a).format(&var_dict));
        println!("{}", (b).format(&var_dict));
        println!("{}", (c).format(&var_dict));
        println!("{}", (a.clone() + b.clone()).format(&var_dict));

        assert_eq!("3a^2 + 4b - 2", (b + a - c).format(&var_dict));

        // (a + 1)(a + 1)
        let a = (Poly::var(0, 1) + Poly::constant(Rat::from(1)))
            * (Poly::var(0, 1) + Poly::constant(Rat::from(1)));
        // a^2 + 2a + 1
        let b = Poly::var(0, 2)
            + Poly::constant(Rat::from(2)) * Poly::var(0, 1)
            + Poly::constant(Rat::from(1));

        assert!(a == b);
    }

    #[test]
    fn arith_fuzz() {
        let mut rng = SmallRng::seed_from_u64(1);

        fn create_random_poly(rng: &mut SmallRng, term_max: i32) -> Poly {
            let mut p = Poly::constant(Rat::zero());

            for _ in 0..rng.gen_range(0..term_max + 1) {
                let coef = rng.gen_range(-6..6);
                let xpow = rng.gen_range(0..4);
                let ypow = rng.gen_range(0..2);
                let zpow = rng.gen_range(0..3);

                p = p + Poly::constant(Rat::from(coef))
                    * Poly::var(0, xpow)
                    * Poly::var(1, ypow)
                    * Poly::var(2, zpow);
            }

            p
        }

        for _ in 0..1000 {
            let dividend = create_random_poly(&mut rng, 10);
            let n_divs = rng.gen_range(0..4);
            let divisors: Vec<_> = std::iter::repeat_with(|| create_random_poly(&mut rng, 6))
                .take(n_divs)
                .collect();

            let (quotients, rem) = dividend.clone().compound_divide(&divisors);

            let calculated_dividend = quotients
                .clone()
                .into_iter()
                .zip(divisors.clone())
                .fold(Poly::constant(Rat::zero()), |acc, (x, y)| acc + x * y)
                + rem.clone();

            assert_eq!(calculated_dividend, dividend);
        }
    }

    // #[test]
    // fn tricky_order() {
    //     let sys = crate::system! {
    //         y^2 - 2*y + 1,
    //         y - 1
    //     };

    //     assert_eq!(
    //         "y - 1",
    //         (sys.members[0].try_divide(&sys.members[1]).unwrap()).format(&sys.var_dict)
    //     );
    // }

    #[test]
    fn derivative() {
        let var_dict = vec!["x".to_string(), "y".to_string(), "z".to_string()];

        let p = Poly::var(0, 2) * Poly::var(1, 2) * Poly::constant(Rat::from(3))
            + Poly::var(0, 1) * Poly::var(2, 1);

        assert_eq!(
            "6xy^2 + z",
            format!("{}", p.derivative(0).format(&var_dict))
        );
    }
}
