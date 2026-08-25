use super::poly::{
    Poly,
    coef::Coef,
    mono::{Mono, Pow, Var},
};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Count {
    num: Poly,
    den: Coef,
}

impl Count {
    pub fn zero() -> Self {
        Self {
            num: Poly::zero(),
            den: 1.into(),
        }
    }

    pub fn constant<T: Into<Coef>>(c: T) -> Self {
        Self {
            num: Poly::constant(c),
            den: 1.into(),
        }
    }

    pub fn var(v: Var, p: Pow) -> Self {
        Self {
            num: Poly::var(v, p),
            den: 1.into(),
        }
    }

    pub fn new<T: Into<Coef>>(num: Poly, den: T) -> Self {
        let den = den.into();
        assert!(!den.is_zero());

        let (num, den) = if !den.is_positive() {
            (num.mul_scalar(-1), -den)
        } else {
            (num, den)
        };
        let g = num.coef_gcd().gcd(&den);
        let num = num.divide_coefs(g).unwrap();

        Self { num, den: den / g }
    }

    pub fn num(&self) -> &Poly {
        &self.num
    }

    pub fn den(&self) -> Coef {
        self.den
    }

    pub fn is_zero(&self) -> bool {
        self.num.is_zero()
    }

    pub fn as_poly(&self) -> Option<&Poly> {
        (self.den == 1.into()).then_some(&self.num)
    }

    pub fn as_constant(&self) -> Option<(Coef, Coef)> {
        self.num.as_constant().map(|c| (c, self.den))
    }

    pub fn add(&self, rhs: &Self) -> Self {
        let g = self.den.gcd(&rhs.den);
        let lcm = self.den / g * rhs.den;
        let num = self
            .num
            .mul_scalar(lcm / self.den)
            .add(&rhs.num.mul_scalar(lcm / rhs.den));
        Self::new(num, lcm).debug_checked()
    }

    pub fn neg(&self) -> Self {
        Self {
            num: self.num.neg(),
            den: self.den,
        }
        .debug_checked()
    }

    pub fn sub(&self, rhs: &Self) -> Self {
        self.add(&rhs.neg())
    }

    pub fn mul(&self, rhs: &Self) -> Self {
        Self::new(self.num.mul(&rhs.num), self.den * rhs.den)
    }

    pub fn mul_scalar<T: Into<Coef>>(&self, c: T) -> Self {
        Self::new(self.num.mul_scalar(c), self.den)
    }

    pub fn div_scalar<T: Into<Coef>>(&self, d: T) -> Self {
        Self::new(self.num.clone(), self.den * d.into())
    }

    pub fn eval(&self, point: impl FnMut(Var) -> i128) -> (Coef, Coef) {
        let n = self.num.eval(point);
        let g = n.gcd(&self.den);
        (n / g, self.den / g)
    }

    pub fn eval_int(&self, point: impl FnMut(Var) -> i128) -> Option<Coef> {
        let (num, den) = self.eval(point);
        if den == 1.into() { Some(num) } else { None }
    }

    pub fn substitute(&self, v: Var, q: &Poly) -> Self {
        Self::new(self.num.substitute(v, q), self.den)
    }

    fn assert_canonical(&self) {
        self.num.assert_canonical();
        assert!(
            self.den.is_positive(),
            "denominator {:?} not positive",
            self.den
        );
        assert_eq!(
            self.num.coef_gcd().gcd(&self.den),
            1.into(),
            "fraction not reduced: content {:?} vs den {:?}",
            self.num.coef_gcd(),
            self.den
        );
    }

    fn debug_checked(self) -> Self {
        #[cfg(debug_assertions)]
        self.assert_canonical();
        self
    }

    pub fn sum_below(&self, var: Var, hi: &Poly) -> Self {
        assert_eq!(hi.degree_in(var), 0);
        let kmax = self.num().degree_in(var);
        if kmax == 0 {
            return Self::zero();
        }

        let kmax = kmax as usize;

        let mut groups: Vec<Vec<(Coef, Mono)>> = vec![Vec::new(); kmax + 1];
        for (c, m) in self.num().terms().iter().rev() {
            let k = m.degree_in(var) as usize;
            let rest = Mono::new(m.exps().iter().copied().filter(|&(v, _)| v != var));
            groups[k].push((*c, rest));
        }

        let mut falling_factorials: Vec<Poly> = Vec::with_capacity(kmax + 1);
        falling_factorials.push(hi.clone());
        for j in 1..=kmax {
            let factor = hi.sub(&Poly::constant(Coef::new(j as i128)));
            falling_factorials.push(falling_factorials[j - 1].mul(&factor));
        }

        let s2 = stirling2(kmax);
        let mut total = Self::zero();
        for (k, group) in groups.into_iter().enumerate() {
            let c_k = Poly::from_terms(group);
            if c_k.is_zero() {
                continue;
            }

            let mut a_k = Self::zero();
            for (j, &s) in s2[k].iter().enumerate() {
                if !s.is_zero() {
                    a_k = a_k.add(&Self::new(
                        falling_factorials[j].mul_scalar(s),
                        Coef::new((j + 1) as i128),
                    ));
                }
            }
            total = total.add(&Self::new(c_k, 1).mul(&a_k));
        }
        total.div_scalar(self.den())
    }

    pub fn sum_range(&self, var: Var, lo: &Poly, hi: &Poly) -> Self {
        self.sum_below(var, hi).sub(&self.sum_below(var, lo))
    }
}

fn stirling2(kmax: usize) -> Vec<Vec<Coef>> {
    let mut s2: Vec<Vec<Coef>> = Vec::with_capacity(kmax + 1);
    s2.push(vec![Coef::new(1)]);
    for k in 1..=kmax {
        let mut row = Vec::with_capacity(k + 1);
        row.push(Coef::new(0));
        for j in 1..=k {
            let above = if j < k { s2[k - 1][j] } else { Coef::new(0) };
            let diag = s2[k - 1][j - 1];
            row.push((Coef::new(j as i128) * above) + diag);
        }
        s2.push(row);
    }
    s2
}

#[cfg(test)]
mod tests {
    use super::{Count, stirling2};
    use crate::solver::poly::{coef::Coef, mono::Var, poly};

    #[test]
    fn ratio_reduce() {
        let half_n = Count::new(poly!(2 * x), 4);
        assert_eq!(half_n.num(), &poly!(x));
        assert_eq!(half_n.den(), 2.into());
        assert_eq!(Count::new(poly!(2 * x + 2), 2), Count::new(poly!(x + 1), 1));
        assert_eq!(Count::new(poly!(2 * x), -1), Count::new(poly!(-2 * x), 1));
    }

    #[test]
    fn arith_sanity() {
        // N/2 * N/3 = N^2/6; N/2 − N/2 = 0
        let (a, b) = (Count::new(poly!(x), 2), Count::new(poly!(x), 3));
        assert_eq!(a.add(&b), Count::new(poly!(5 * x), 6));
        assert_eq!(a.sub(&a), Count::new(poly!(), 1))
    }

    #[test]
    fn stirling_triangle() {
        // OEIS A008277
        assert_eq!(
            stirling2(5),
            vec![
                Coef::vec(&[1]),
                Coef::vec(&[0, 1]),
                Coef::vec(&[0, 1, 1]),
                Coef::vec(&[0, 1, 3, 1]),
                Coef::vec(&[0, 1, 7, 6, 1]),
                Coef::vec(&[0, 1, 15, 25, 10, 1]),
            ]
        );
    }

    #[test]
    fn faulhaber() {
        // Σ_{0<=i<B} i^3 = B^2(B−1)^2/4
        let cube = Count::new(poly!(i ^ 3), 1);
        let s = cube.sum_below('i' as Var, &poly!(b));
        let b2 = poly!(b ^ 2);
        let bm1 = poly!(b - 1);
        assert_eq!(s, Count::new(b2.mul(&bm1.pow(2)), 4));

        // (Σ i)^2 = Σ i^3
        let triangle = Count::new(poly!(i), 1).sum_below('i' as Var, &poly!(b));
        assert_eq!(triangle.mul(&triangle), s);
    }
}
