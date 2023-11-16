// thanks to Osvaldo Carvalho
// https://www.researchgate.net/publication/320864673_A_simple_recursive_algorithm_to_find_all_real_roots_of_a_polynomial

use std::cmp::Ordering;

use crate::solver::field::Field;
use crate::solver::rational::Rat;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UPoly<T: Field>(pub Vec<T>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Root<T: Field> {
    Point(T),
    Interval(T, T),
}

impl<T: Field> Root<T> {
    pub fn approx(&self) -> T {
        match self {
            Root::Point(p) => p.clone(),
            Root::Interval(start, end) => (start.clone() + end.clone()) / T::from(2),
        }
    }
}

impl<T: Field> UPoly<T> {
    // Horner's method
    pub fn eval(&self, x: &T) -> T {
        self.0
            .iter()
            .fold(T::zero(), |acc, next| acc * x.clone() + next.clone())
    }

    pub fn derivative(&self) -> Self {
        let mut new = self.0.clone();
        new.pop();
        let deg = new.len() - 1;

        for (i, coef) in new.iter_mut().enumerate() {
            *coef = coef.clone() * ((deg + 1 - i) as i64);
        }

        Self(new)
    }

    pub fn real_root_intervals(&self, tolerance: T) -> Vec<Root<T>> {
        match self.0.len() {
            0 | 1 => vec![],
            2 => {
                vec![Root::Point(self.0[1].clone() * -1 / self.0[0].clone())]
            }
            _ => {
                let derivative = self.derivative();
                let mut derivative_roots = derivative.real_root_intervals(tolerance.clone());

                if derivative_roots.is_empty() {
                    // add one arbitrary root to check (-inf, inf)
                    derivative_roots.push(Root::Point(T::zero()));
                }

                let mut new_roots = vec![];

                let first_derivative_root = derivative_roots[0].approx();

                match self.eval(&first_derivative_root).cmp(&T::zero()) {
                    Ordering::Less => {
                        if (self.0[0] < T::zero()) ^ (self.0.len() % 2 == 0) {
                            // value here is same sign as -inf; no root
                        } else {
                            // probe backwards until we have a finite interval
                            let mut lhs = T::from(-1);
                            while self.eval(&(first_derivative_root.clone() + lhs.clone()))
                                < T::zero()
                            {
                                lhs = lhs.clone() * T::from(2);
                            }
                            new_roots.push(self.refine_root_interval(
                                &derivative,
                                first_derivative_root.clone() + lhs,
                                first_derivative_root,
                                tolerance.clone(),
                            ))
                        }
                    }
                    Ordering::Greater => {
                        if !((self.0[0] < T::zero()) ^ (self.0.len() % 2 == 0)) {
                            // value here is same sign as -inf; no root
                        } else {
                            // probe backwards until we have a finite interval
                            let mut lhs = T::from(-1);
                            while self.eval(&(first_derivative_root.clone() + lhs.clone()))
                                > T::zero()
                            {
                                lhs = lhs.clone() * T::from(2);
                            }
                            new_roots.push(self.refine_root_interval(
                                &derivative,
                                first_derivative_root.clone() + lhs,
                                first_derivative_root,
                                tolerance.clone(),
                            ))
                        }
                    }
                    Ordering::Equal => new_roots.push(Root::Point(first_derivative_root)),
                }

                for i in 0..derivative_roots.len() - 1 {
                    let interval_start = derivative_roots[i].approx();
                    let interval_end = derivative_roots[i + 1].approx();

                    let start_val = self.eval(&interval_start);
                    let end_val = self.eval(&interval_end);

                    if start_val == T::zero() {
                        // no roots in this interval
                    } else if end_val == T::zero() {
                        new_roots.push(Root::Point(interval_end));
                    } else if (start_val < T::zero() && end_val < T::zero())
                        || (start_val > T::zero() && end_val > T::zero())
                    {
                        // no roots in this interval
                    } else {
                        new_roots.push(self.refine_root_interval(
                            &derivative,
                            interval_start,
                            interval_end,
                            tolerance.clone(),
                        ))
                    }
                }

                let last_derivative_root = derivative_roots[derivative_roots.len() - 1].approx();

                match self.eval(&last_derivative_root).cmp(&T::zero()) {
                    Ordering::Less => {
                        if self.0[0] < T::zero() {
                            // value here is same sign as inf; no root
                        } else {
                            // probe backwards until we have a finite interval
                            let mut lhs = T::from(1);
                            while self.eval(&(last_derivative_root.clone() + lhs.clone()))
                                < T::zero()
                            {
                                lhs = lhs.clone() * T::from(2);
                            }
                            new_roots.push(self.refine_root_interval(
                                &derivative,
                                last_derivative_root.clone(),
                                last_derivative_root + lhs,
                                tolerance,
                            ))
                        }
                    }
                    Ordering::Greater => {
                        if self.0[0] > T::zero() {
                            // value here is same sign as inf; no root
                        } else {
                            // probe backwards until we have a finite interval
                            let mut lhs = T::from(1);
                            while self.eval(&(last_derivative_root.clone() + lhs.clone()))
                                > T::zero()
                            {
                                lhs = lhs.clone() * T::from(2);
                            }
                            new_roots.push(self.refine_root_interval(
                                &derivative,
                                last_derivative_root.clone(),
                                last_derivative_root + lhs,
                                tolerance,
                            ));
                        }
                    }
                    Ordering::Equal => {}
                }

                new_roots
            }
        }
    }

    pub fn refine_root_interval(
        &self,
        derivative: &UPoly<T>,
        mut start: T,
        mut end: T,
        tolerance: T,
    ) -> Root<T> {
        let start_sign = self.eval(&start) > T::zero();

        let max_newton_iters = 10;
        let mut newton_iters = 0;
        while end.clone() - start.clone() > tolerance && newton_iters < max_newton_iters {
            let start_deriv = derivative.eval(&start);
            let end_deriv = derivative.eval(&end);

            let mid = (start.clone() + end.clone()) / T::from(2);
            let mid_eval = self.eval(&mid);

            if mid_eval == T::zero() {
                return Root::Point(mid);
            }

            let candidate1 = mid.clone() - mid_eval.clone() / start_deriv;
            let candidate1_eval = self.eval(&candidate1);

            if candidate1_eval == T::zero() {
                return Root::Point(candidate1);
            }

            let candidate2 = mid - mid_eval / end_deriv;
            let candidate2_eval = self.eval(&candidate2);

            if candidate2_eval == T::zero() {
                return Root::Point(candidate2);
            }

            let mut progress = false;

            if candidate1 > start && candidate1 < end {
                let candidate1_sign = self.eval(&candidate1) > T::zero();
                if candidate1_sign == start_sign {
                    start = candidate1;
                } else {
                    end = candidate1;
                }
                progress = true;
            }

            if candidate2 > start && candidate2 < end {
                let candidate2_sign = self.eval(&candidate2) > T::zero();
                if candidate2_sign == start_sign {
                    start = candidate2;
                } else {
                    end = candidate2;
                }
                progress = true;
            }

            if !progress {
                break;
            }

            newton_iters += 1;
        }

        while end.clone() - start.clone() > tolerance {
            let mid = (start.clone() + end.clone()) / T::from(2);

            let mid_eval = self.eval(&mid);

            if mid_eval == T::zero() {
                return Root::Point(mid);
            }

            if (mid_eval > T::zero()) == start_sign {
                start = mid;
            } else {
                end = mid;
            }
        }

        Root::Interval(start, end)
    }
}

impl UPoly<Rat> {
    pub fn real_roots(&self, tolerance: f64) -> Vec<f64> {
        let mut tolerance_rat = Rat::from(1);
        while f64::from(tolerance_rat) > tolerance {
            tolerance_rat = tolerance_rat / Rat::from(10);
        }

        self.real_root_intervals(tolerance_rat)
            .iter()
            .map(|root| f64::from(root.approx()))
            .collect()
    }
}

#[macro_export]
macro_rules! univariate {
    ( $($t:tt)* ) => ({
        use $crate::univariate::UPoly;
        use $crate::rational::Rat;
        use $crate::field::Field;

        let system = $crate::system! { $($t)* };

        if system.members.len() != 1 {
            panic!("pass 1 polynomial to univariate macro")
        }

        if system.var_dict.len() > 1 {
            panic!("use at most 1 variable in univariate macro")
        }

        let mut coefs = vec![];

        let mut curr_pow = 0;
        for term in &system.members[0].terms {
            if term.vars.is_empty() {
                coefs.push(term.val);
                curr_pow += 1;
            } else {
                while curr_pow < term.vars[0].1 {
                    coefs.push(Rat::zero());
                    curr_pow += 1
                }
                coefs.push(term.val);
                curr_pow += 1;
            }
        }

        UPoly(coefs.into_iter().rev().collect::<Vec<_>>())
    })
}

#[cfg(test)]
mod tests {
    use super::{Root, UPoly};
    use crate::solver::rational::Rat;

    #[test]
    fn eval() {
        let p = UPoly(vec![
            Rat::from(2),
            Rat::from(3),
            Rat::from(-4),
            Rat::from(1),
        ]);
        assert_eq!(p.eval(&Rat::from(8)), Rat::from(1185));
    }

    #[test]
    fn derivative() {
        let p = UPoly(vec![
            Rat::from(1),
            Rat::from(5),
            Rat::from(0),
            Rat::from(2),
            Rat::from(3),
        ]);
        let dp = p
            .derivative()
            .0
            .into_iter()
            .map(|x| i64::try_from(x).unwrap())
            .collect::<Vec<_>>();

        assert_eq!(dp, vec![4, 15, 0, 2]);
    }

    #[test]
    fn refine_root_interval() {
        let quadratic = UPoly(vec![Rat::from(1), Rat::from(0), Rat::from(-2)]);
        let tol = Rat::from(1) / Rat::from(10000);
        let refined = quadratic
            .refine_root_interval(&quadratic.derivative(), Rat::from(0), Rat::from(2), tol)
            .approx();
        let approx_zero = (2. - f64::from(refined) * f64::from(refined)).abs();

        assert!(approx_zero < f64::from(tol));
    }

    #[test]
    fn lin_root() {
        let linear = UPoly(vec![Rat::from(3), Rat::from(-2)]);
        assert_eq!(
            linear.real_root_intervals(Rat::from(1) / Rat::from(10000)),
            vec![Root::Point(Rat::from(2) / Rat::from(3))]
        );
    }

    #[test]
    fn big_root() {
        let poly = UPoly(vec![
            Rat::from(1),
            Rat::from(-3),
            Rat::from(-21),
            Rat::from(43),
            Rat::from(60),
        ]);

        let tol = Rat::from(1) / Rat::from(10000);
        let roots = poly.real_root_intervals(tol);
        let roots_f: Vec<_> = roots.iter().map(|r| f64::from(r.approx())).collect();

        assert!((roots_f[0] + 4.).abs() < f64::from(tol));
        assert!((roots_f[1] + 1.).abs() < f64::from(tol));
        assert!((roots_f[2] - 3.).abs() < f64::from(tol));
        assert!((roots_f[3] - 5.).abs() < f64::from(tol));
    }
}
