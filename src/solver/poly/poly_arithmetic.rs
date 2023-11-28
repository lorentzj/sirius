use std::cmp::Ordering;
use std::collections::VecDeque;
use std::ops;

use crate::solver::poly::mono::*;
use crate::solver::poly::*;

impl<T: Field> ops::Add<Poly<T>> for Poly<T> {
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

impl<T: Field> ops::Sub<Poly<T>> for Poly<T> {
    type Output = Self;

    fn sub(self, mut rhs: Self) -> Self {
        for term in &mut rhs.terms {
            term.val = term.val.clone() * -1;
        }

        self + rhs
    }
}

impl<T: Field> ops::Mul<Poly<T>> for Poly<T> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        self.mul_ref(&rhs)
    }
}

impl<T: Field> Poly<T> {
    pub fn mul_ref(&self, other: &Poly<T>) -> Poly<T> {
        let mut new = Self::constant(T::zero());

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

    pub fn compound_divide(&self, divisors: &Vec<Poly<T>>) -> (Vec<Poly<T>>, Poly<T>) {
        if divisors.is_empty() {
            return (vec![], self.clone());
        }

        let mut dividend = self.clone();

        let mut rem = Poly::constant(T::zero());
        let mut quotients: Vec<VecDeque<Mono<T>>> = std::iter::repeat(VecDeque::from(vec![]))
            .take(divisors.len())
            .collect();

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

    pub fn try_divide(&self, divisor: &Poly<T>) -> Option<Poly<T>> {
        let (quots, rem) = self.compound_divide(&vec![divisor.clone()]);

        if rem.is_zero() {
            Some(quots[0].clone())
        } else {
            None
        }
    }

    pub fn derivative(&self, by: &str) -> Poly<T> {
        let mut new_terms = vec![];
        for term in &self.terms {
            let mut new_term = Mono {
                val: term.val.clone(),
                vars: vec![],
            };
            let mut found = false;
            for (var, pow) in &term.vars {
                if var.as_ref() == by {
                    found = true;
                    if *pow > 1 {
                        new_term.val = new_term.val * *pow as i64;
                        new_term.vars.push((var.clone(), *pow - 1));
                    }
                } else {
                    new_term.vars.push((var.clone(), *pow));
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
    use crate::solver::field::Zero;
    use crate::solver::rational::Rat;
    use rand::prelude::*;

    #[test]
    fn arith() {
        let a = Poly::var("a".to_string(), 2) * Poly::constant(Rat::from(3));
        let b = Poly::var("b".to_string(), 1) * Poly::constant(Rat::from(4));
        let c = Poly::constant(Rat::from(2));

        assert_eq!("3a^2 + 4b - 2", format!("{:?}", b + a - c));

        // (a + 1)(a + 1)
        let a: Poly<Rat> = (Poly::var("a".to_string(), 1) + Poly::constant(Rat::from(1)))
            * (Poly::var("a".to_string(), 1) + Poly::constant(Rat::from(1)));
        // a^2 + 2a + 1
        let b = Poly::var("a".to_string(), 2)
            + Poly::constant(Rat::from(2)) * Poly::var("a".to_string(), 1)
            + Poly::constant(Rat::from(1));

        assert!(a == b);
    }

    #[test]
    fn arith_fuzz() {
        let mut rng = SmallRng::seed_from_u64(1);

        fn create_random_poly(rng: &mut SmallRng, term_max: i32) -> Poly<Rat> {
            let mut p = Poly::constant(Rat::zero());

            for _ in 0..rng.gen_range(0..term_max + 1) {
                let coef = rng.gen_range(-6..6);
                let xpow = rng.gen_range(0..4);
                let ypow = rng.gen_range(0..2);
                let zpow = rng.gen_range(0..3);

                p = p + Poly::constant(Rat::from(coef))
                    * Poly::var("x".to_string(), xpow)
                    * Poly::var("y".to_string(), ypow)
                    * Poly::var("z".to_string(), zpow);
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

    #[test]
    fn tricky_order() {
        let sys = crate::system! {
            y^2 - 2*y + 1,
            y - 1
        };

        assert_eq!(
            "y - 1",
            format!("{:?}", sys.members[0].try_divide(&sys.members[1]).unwrap())
        );
    }

    #[test]
    fn derivative() {
        let p: Poly<Rat> = Poly::var("x".to_string(), 2)
            * Poly::var("y".to_string(), 2)
            * Poly::constant(Rat::from(3))
            + Poly::var("x".to_string(), 1) * Poly::var("z".to_string(), 1);

        assert_eq!("6xy^2 + z", format!("{:?}", p.derivative("x")));
    }
}
