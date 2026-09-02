//! A multivariable [polynomial ring](https://en.wikipedia.org/wiki/Polynomial_ring) with integer coefficients.

use std::cmp::Ordering;

pub mod coef;
pub mod mono;

mod arithmetic;
mod display;
mod macros;

pub use arithmetic::monomial_div;
pub use macros::poly;

use coef::Coef;
pub use mono::{Mono, Pow, Var};

/// A multivariable polynomial with integer coefficients in canonical form, $\mathbb{Z}[x_1,x_2 \dots x_i]$.
///
/// For example, $4x^2y^5 - 7xz + 3y + 1$.
///
/// ```
/// use sirius::solver::poly::poly;
///
/// assert_eq!(poly!(a^2 - b^2).try_divide(&poly!(a - b)), Some(poly!(a + b)));
/// assert_eq!(poly!(2*x + 5).substitute('x', &poly!(z^2 + 1)), poly!(2*z^2 + 7));
/// ```
#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Poly {
    terms: Vec<(Coef, Mono)>,
}

impl Poly {
    pub fn zero() -> Self {
        Self { terms: vec![] }
    }

    pub fn is_zero(&self) -> bool {
        self.terms.is_empty()
    }

    pub fn constant<T: Into<Coef>>(c: T) -> Self {
        let c = c.into();
        if c == 0.into() {
            Poly::zero()
        } else {
            Poly {
                terms: vec![(c, Mono::unit())],
            }
        }
    }

    pub fn var<T: Into<Var>>(v: T, p: Pow) -> Self {
        if p == 0 {
            Self::constant(1)
        } else {
            Self::term(1, Mono::new(vec![(v.into(), p)]))
        }
    }

    pub fn term<T: Into<Coef>>(c: T, m: Mono) -> Self {
        let c = c.into();
        if c == 0.into() {
            Poly::zero()
        } else {
            Poly {
                terms: vec![(c, m)],
            }
        }
    }

    pub fn terms(&self) -> Vec<(Coef, Mono)> {
        self.terms.clone()
    }

    pub fn from_terms(terms: impl IntoIterator<Item = (Coef, Mono)>) -> Self {
        let mut raw: Vec<_> = terms.into_iter().collect();
        raw.sort_unstable_by(|(_, m1), (_, m2)| m2.cmp(m1));
        let mut terms = Vec::with_capacity(raw.len());
        for (c, m) in raw {
            match terms.last_mut() {
                Some((lc, lm)) if *lm == m => {
                    *lc = *lc + c;
                    if *lc == 0.into() {
                        terms.pop();
                    }
                }
                _ => {
                    if c != 0.into() {
                        terms.push((c, m));
                    }
                }
            }
        }
        Self { terms }.debug_checked()
    }

    pub fn total_degree(&self) -> Pow {
        self.terms
            .first()
            .map(|(_, m)| m.total_degree())
            .unwrap_or(0)
    }

    pub fn degree_in<T: Into<Var>>(&self, v: T) -> Pow {
        let v = v.into();
        let mut deg = 0;
        for (_, mono) in &self.terms {
            let mut term_deg = 0;
            for (var, pow) in mono.exps() {
                term_deg += pow;
                match var.cmp(&v) {
                    Ordering::Greater => continue,
                    Ordering::Equal => {
                        deg = deg.max(*pow);
                        break;
                    }
                    Ordering::Less => break,
                }
            }
            if term_deg < deg {
                break;
            }
        }

        deg
    }

    pub fn leading_term(&self) -> (Coef, Mono) {
        match self.terms.first() {
            Some(m) => m.clone(),
            None => (Coef::new(0), Mono::unit()),
        }
    }

    pub fn eval(&self, mut point: impl FnMut(Var) -> i128) -> Coef {
        let mut acc = Coef::new(0);
        for (c, m) in &self.terms {
            let mut t = *c;
            for &(v, e) in m.exps() {
                t = t * Coef::new(point(v).checked_pow(e).unwrap());
            }
            acc = acc + t;
        }
        acc
    }

    pub fn map_vars(&self, mut f: impl FnMut(Var) -> Self) -> Self {
        let mut acc = Poly::zero();
        for (c, m) in &self.terms {
            let mut t = Poly::constant(*c);
            for &(v, e) in m.exps() {
                t = t.mul(&f(v).pow(e));
            }
            acc = acc.add(&t);
        }
        acc
    }

    pub fn substitute<T: Into<Var>>(&self, v: T, q: &Self) -> Self {
        let v = v.into();
        self.map_vars(|w| if w == v { q.clone() } else { Self::var(w, 1) })
    }

    pub fn coef_gcd(&self) -> Coef {
        self.terms
            .iter()
            .fold(Coef::from(0), |g, &(c, _)| g.gcd(&c))
            .abs()
    }

    pub fn coef_lcm(&self) -> Coef {
        self.terms
            .iter()
            .fold(Coef::from(1), |g, &(c, _)| g.lcm(&c))
            .abs()
    }

    pub fn divide_coefs<T: Into<Coef>>(&self, d: T) -> Option<Self> {
        let d = d.into();
        if d.is_zero() {
            return None;
        }
        let mut terms = Vec::with_capacity(self.terms.len());
        for (c, m) in &self.terms {
            let (quot, rem) = c.divrem(d);
            if rem.is_zero() {
                terms.push((quot, m.clone()));
            } else {
                return None;
            }
        }
        Some(Poly { terms }.debug_checked())
    }

    pub fn as_constant(&self) -> Option<Coef> {
        match self.terms.as_slice() {
            [] => Some(Coef::new(0)),
            [(c, m)] if m.is_unit() => Some(*c),
            _ => None,
        }
    }

    pub fn vars(&self) -> Vec<Var> {
        let mut vs = vec![];

        for term in &self.terms {
            for (var, _) in term.1.exps() {
                if !vs.contains(var) {
                    vs.push(*var);
                }
            }
        }

        vs.sort_unstable();
        vs
    }

    pub fn always_nonneg(&self, nonneg: &dyn Fn(Var) -> bool) -> bool {
        self.terms()
            .iter()
            .all(|(c, m)| c.is_positive() && m.always_nonneg(nonneg))
    }

    pub fn assert_canonical(&self) {
        for w in self.terms.windows(2) {
            assert!(
                w[0].1 > w[1].1,
                "terms not strictly descending: {:?} then {:?}",
                w[0],
                w[1]
            );
        }
        for (c, m) in &self.terms {
            assert!(*c != 0.into(), "zero coefficient on {m:?}");
            m.assert_canonical();
        }
    }

    fn debug_checked(self) -> Self {
        #[cfg(debug_assertions)]
        self.assert_canonical();
        self
    }
}

#[cfg(test)]
mod tests {
    use super::{Coef, Poly, monomial_div, poly};

    #[test]
    fn constants() {
        let x = poly!(3 * x ^ 2 + 5 * z - 2 + 1);
        let y = poly!(4 * y * z);
        assert_eq!(
            x.mul(&y),
            poly!(12 * x ^ 2 * y * z + 20 * y * z ^ 2 - 4 * y * z)
        );
        assert_eq!(poly!(), poly!(x - x));
    }

    #[test]
    fn arithmetic_sanity() {
        let x = poly!(a ^ 2 + 2 * b + c);
        let y = poly!(2 * a ^ 2 - c ^ 3 + d);
        assert!(x.add(&y) == poly!(-1 * c ^ 3 + 3 * a ^ 2 + 2 * b + c + d));

        let x = poly!(a ^ 4 - b ^ 4);
        let y = poly!(a ^ 2 + b ^ 2);
        assert_eq!(x.try_divide(&y), Some(poly!(a ^ 2 - b ^ 2)));

        let x = poly!(a ^ 2 - 2 * a * b + b ^ 2);
        let y = poly!(a - b);
        assert_eq!(x.try_divide(&y), Some(poly!(a - b)));

        let x = poly!(-4 * b);
        let y = poly!(a);
        assert_eq!(x.try_divide(&y), None);
    }

    #[test]
    fn arithmetic_fuzz() {
        use rand::prelude::*;

        use super::Poly;

        let mut rng = SmallRng::seed_from_u64(1);

        fn create_random_poly(rng: &mut SmallRng, term_max: i32) -> Poly {
            let mut p = Poly::zero();

            for _ in 0..rng.gen_range(0..term_max + 1) {
                let coef = rng.gen_range(-7..7);
                let xpow = rng.gen_range(0..2);
                let ypow = rng.gen_range(0..2);
                let zpow = rng.gen_range(0..4);
                let coef = Poly::constant(coef);
                let xpow = Poly::var('x', xpow);
                let ypow = Poly::var('y', ypow);
                let zpow = Poly::var('z', zpow);

                p = p.add(&coef.mul(&xpow).mul(&ypow).mul(&zpow));
            }

            p
        }

        for _ in 0..10_000 {
            let dividend = create_random_poly(&mut rng, 10);
            let n_divs = rng.gen_range(0..4);
            let mut divisors: Vec<_> = std::iter::repeat_with(|| create_random_poly(&mut rng, 6))
                .take(n_divs)
                .collect();

            let (quotients, rem) = dividend.compound_divide(&mut divisors);

            println!("-------------------------");
            println!("calculated {:?} / {:?}", dividend, divisors);
            println!("got {:?} rem {:?}", quotients, rem);
            println!("-------------------------");

            let calculated_dividend = quotients
                .into_iter()
                .zip(divisors.clone())
                .fold(Poly::zero(), |acc, (x, y)| acc.add(&mut x.mul(&y)))
                .add(&mut rem.clone());

            assert_eq!(calculated_dividend, dividend);
        }
    }

    pub fn s_poly(p: &Poly, q: &Poly) -> Poly {
        let p_lt = Poly {
            terms: vec![p.leading_term()],
        };
        let q_lt = Poly {
            terms: vec![q.leading_term()],
        };

        let lcm_ltp_ltq = Poly {
            terms: vec![(
                p_lt.terms[0].0 * q_lt.terms[0].0,
                p_lt.terms[0].1.lcm(&q_lt.terms[0].1),
            )],
        };

        let coef_p = lcm_ltp_ltq.try_divide(&p_lt).unwrap();
        let coef_q = lcm_ltp_ltq.try_divide(&q_lt).unwrap();

        let a = coef_p.mul(&p);
        let b = coef_q.mul(&q);
        let res = a.sub(&b);

        if res.is_zero() {
            res
        } else {
            res.divide_coefs(res.coef_gcd()).unwrap()
        }
    }

    pub fn groebner_basis(sys: &[Poly]) -> Vec<Poly> {
        let mut sys = sys.to_vec();

        // buchberger

        let mut combs = {
            let mut combs = vec![];
            for i in 0..sys.len() {
                for j in 0..sys.len() {
                    if i != j {
                        combs.push((sys[i].clone(), sys[j].clone()));
                    }
                }
            }

            combs
        };

        while let Some((a, b)) = combs.pop() {
            let s = s_poly(&a, &b);
            let (_, rem) = s.compound_divide(&sys);

            if !rem.is_zero() {
                for member in &sys {
                    combs.push((member.clone(), rem.clone()));
                }
                sys.push(rem);
            }
        }

        // reduce

        let mut keep = vec![];

        for i in 0..sys.len() {
            let mut divides_any = false;

            for j in 0..sys.len() {
                if i != j {
                    let i_lt = (Coef::new(1), sys[i].leading_term().1);
                    let j_lt = (Coef::new(1), sys[j].leading_term().1);
                    if let Some((_, m)) = monomial_div(&i_lt, &j_lt) {
                        if m.is_unit() {
                            divides_any = i > j;
                        } else {
                            divides_any = true;
                        }

                        if divides_any {
                            break;
                        }
                    }
                }
            }

            if !divides_any {
                keep.push(sys[i].clone());
            }
        }

        keep.sort_by(|p, q| p.leading_term().1.cmp(&q.leading_term().1).reverse());
        println!("sys: {:?}", keep);

        let mut keep2 = vec![];

        for (i, k) in keep.iter().enumerate() {
            let all_except = keep
                .iter()
                .enumerate()
                .filter_map(|(j, p)| if j != i { Some(p.clone()) } else { None })
                .collect::<Vec<_>>();

            let all_except_lcm = all_except
                .iter()
                .fold(Coef::new(1), |c, p| c.lcm(&p.coef_lcm()));

            let (_, rem) = k.mul_scalar(all_except_lcm).compound_divide(&all_except);
            if !rem.is_zero() {
                keep2.push(rem.divide_coefs(rem.coef_gcd()).unwrap());
            }
        }

        for p in keep2.iter_mut() {
            if !p.leading_term().0.is_positive() {
                *p = p.mul_scalar(-1);
            }
        }

        keep2
    }

    #[test]
    fn groebner_basis_validation() {
        let sys = [
            poly!(x + y ^ 2 + z),
            poly!(x - y + 3 * z + 5),
            poly!(x - 2 * y + 3),
        ];

        let gb = groebner_basis(&sys);

        let correct = [
            poly!(9 * z ^ 2 + 7 * z - 3),
            poly!(x + 6 * z + 7),
            poly!(y + 3 * z + 2),
        ];

        println!("{:?}", gb);
        println!("{:?}", correct);

        for (g, c) in gb.iter().zip(correct.iter()) {
            assert_eq!(g, c, "{:?} = {:?}", g, c);
        }

        let sys = [
            poly!(x ^ 2 * y + 1),
            poly!(2 * x + y * z - 1),
            poly!(x - y ^ 2 * z ^ 2 + 1),
        ];

        let gb = groebner_basis(&sys);

        let correct = [poly!(4 * x - 5), poly!(25 * y + 16), poly!(32 * z - 75)];

        println!("{:?}", gb);
        println!("{:?}", correct);

        for (g, c) in gb.iter().zip(correct.iter()) {
            assert_eq!(g, c, "{:?} = {:?}", g, c);
        }
    }
}
