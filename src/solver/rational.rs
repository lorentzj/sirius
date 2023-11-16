use std::{
    cmp::{Ord, Ordering, PartialOrd},
    ops,
};

use crate::solver::field;

// overflow-safe 127 bit rational type
// auto-simplifying
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Rat {
    pub num: i64,
    pub den: i64,
}

impl Rat {
    pub fn new(val: i64) -> Rat {
        Rat { num: val, den: 1 }
    }

    pub fn try_int(&self) -> Option<i64> {
        if self.den == 1 {
            Some(self.num)
        } else {
            None
        }
    }

    pub fn is_zero(&self) -> bool {
        self.num == 0
    }
}

impl PartialOrd<Rat> for Rat {
    // Required method
    fn partial_cmp(&self, other: &Rat) -> Option<Ordering> {
        if self == other {
            Some(Ordering::Equal)
        } else if (self.num < 0 && other.num >= 0) || (self.num == 0 && other.num > 0) {
            Some(Ordering::Less)
        } else if (self.num > 0 && other.num <= 0) || (self.num == 0 && other.num < 0) {
            Some(Ordering::Greater)
        } else {
            let mut self_clone = *self;
            let mut other_clone = *other;
            loop {
                let lhs = match self_clone.num.checked_mul(other_clone.den) {
                    Some(v) => v,
                    None => {
                        if self_clone.num > other_clone.den {
                            self_clone.num >>= 1;
                            self_clone.den >>= 1;
                        } else {
                            other_clone.num >>= 1;
                            other_clone.den >>= 1;
                        }

                        continue;
                    }
                };

                let rhs = match self_clone.den.checked_mul(other_clone.num) {
                    Some(v) => v,
                    None => {
                        if self_clone.den > other_clone.num {
                            self_clone.num >>= 1;
                            self_clone.den >>= 1;
                        } else {
                            other_clone.num >>= 1;
                            other_clone.den >>= 1;
                        }

                        continue;
                    }
                };

                return Some(lhs.cmp(&rhs));
            }
        }
    }
}

impl Ord for Rat {
    fn cmp(&self, other: &Rat) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl From<i64> for Rat {
    fn from(val: i64) -> Self {
        Self { num: val, den: 1 }
    }
}

impl From<Rat> for f64 {
    fn from(val: Rat) -> f64 {
        val.num as f64 / val.den as f64
    }
}

impl TryFrom<Rat> for i64 {
    type Error = ();

    fn try_from(val: Rat) -> Result<i64, Self::Error> {
        if val.den == 1 {
            Ok(val.num)
        } else {
            Err(())
        }
    }
}

impl field::Zero for Rat {
    fn zero() -> Self {
        Self { num: 0, den: 1 }
    }

    fn is_zero(&self) -> bool {
        self.num == 0
    }
}

impl field::One for Rat {
    fn one() -> Self {
        Self { num: 1, den: 1 }
    }
}

impl ops::Add<Rat> for Rat {
    type Output = Self;

    fn add(mut self, mut rhs: Self) -> Self {
        loop {
            let den_gcd = gcd(self.den, rhs.den);

            let lhs_num = match (rhs.den / den_gcd).checked_mul(self.num) {
                Some(v) => v,
                None => {
                    if self.num == i64::MIN
                        || (rhs.den != i64::MIN && self.num.abs() > rhs.den.abs())
                    {
                        self.num >>= 1;
                        self.den >>= 1;
                    } else {
                        rhs.num >>= 1;
                        rhs.den >>= 1;
                    }
                    continue;
                }
            };

            let rhs_num = match (self.den / den_gcd).checked_mul(rhs.num) {
                Some(v) => v,
                None => {
                    if self.den == i64::MIN
                        || (rhs.num != i64::MIN && self.den.abs() > rhs.num.abs())
                    {
                        self.num >>= 1;
                        self.den >>= 1;
                    } else {
                        rhs.num >>= 1;
                        rhs.den >>= 1;
                    }

                    continue;
                }
            };

            let num = match lhs_num.checked_add(rhs_num) {
                Some(v) => v,
                None => {
                    if self.num == i64::MIN
                        || (rhs.num != i64::MIN && self.num.abs() > rhs.num.abs())
                    {
                        self.num >>= 1;
                        self.den >>= 1;
                    } else {
                        rhs.num >>= 1;
                        rhs.den >>= 1;
                    }

                    continue;
                }
            };

            let den = match (self.den / den_gcd).checked_mul(rhs.den) {
                Some(v) => v,
                None => {
                    if self.den == i64::MIN
                        || (rhs.den != i64::MIN && self.den.abs() > rhs.den.abs())
                    {
                        self.num >>= 1;
                        self.den >>= 1;
                    } else {
                        rhs.num >>= 1;
                        rhs.den >>= 1;
                    }

                    continue;
                }
            };

            let new_gcd = gcd(num, den).abs();

            if den > 0 {
                return Self {
                    num: num / new_gcd,
                    den: den / new_gcd,
                };
            } else {
                return Self {
                    num: -num / new_gcd,
                    den: -den / new_gcd,
                };
            }
        }
    }
}

impl ops::Sub<Rat> for Rat {
    type Output = Self;

    fn sub(self, mut rhs: Self) -> Self {
        if rhs.num == i64::MIN {
            rhs.num >>= 1;
            rhs.den >>= 1;
        }

        rhs.num *= -1;

        self + rhs
    }
}

impl ops::Mul<Rat> for Rat {
    type Output = Self;

    fn mul(mut self, mut rhs: Self) -> Self {
        loop {
            if self.num == i64::MIN || self.den == i64::MIN {
                self.num >>= 1;
                self.den >>= 1;
            }

            if rhs.num == i64::MIN || rhs.den == i64::MIN {
                rhs.num >>= 1;
                rhs.den >>= 1;
            }

            let lhs_gcd = gcd(self.num, rhs.den);
            let rhs_gcd = gcd(rhs.num, self.den);

            let num = (self.num / lhs_gcd).checked_mul(rhs.num / rhs_gcd);
            let den = (self.den / rhs_gcd).checked_mul(rhs.den / lhs_gcd);

            if let (Some(num), Some(den)) = (num, den) {
                return Self { num, den };
            } else if self.num == i64::MIN
                || (rhs.num != i64::MIN && self.num.abs() > rhs.num.abs())
            {
                self.num >>= 1;
                self.den >>= 1;
            } else {
                rhs.num >>= 1;
                rhs.den >>= 1;
            }
        }
    }
}

impl ops::Mul<i64> for Rat {
    type Output = Self;

    fn mul(mut self, rhs: i64) -> Self {
        if self.den % rhs == 0 {
            self.den /= rhs;
        } else {
            self.num *= rhs;
        }

        self
    }
}

impl ops::Div<Rat> for Rat {
    type Output = Self;

    fn div(mut self, mut rhs: Self) -> Self {
        loop {
            if self.num == i64::MIN || self.den == i64::MIN {
                self.num >>= 1;
                self.den >>= 1;
            }

            if rhs.num == i64::MIN || rhs.den == i64::MIN {
                rhs.num >>= 1;
                rhs.den >>= 1;
            }

            let num_gcd = gcd(self.num, rhs.num);
            let den_gcd = gcd(rhs.den, self.den);

            let num = (self.num / num_gcd).checked_mul(rhs.den / den_gcd);
            let den = (self.den / den_gcd).checked_mul(rhs.num / num_gcd);

            if let (Some(num), Some(den)) = (num, den) {
                return Self { num, den };
            } else if self.num == i64::MIN
                || (rhs.den != i64::MIN && self.num.abs() > rhs.den.abs())
            {
                self.num >>= 1;
                self.den >>= 1;
            } else {
                rhs.num >>= 1;
                rhs.den >>= 1;
            }
        }
    }
}

impl std::string::ToString for Rat {
    fn to_string(&self) -> String {
        f64::from(*self).to_string()
    }
}

// Euclid's algorithm
pub fn gcd(mut a: i64, mut b: i64) -> i64 {
    let mut shift = 0;
    if a == 0 || b == 0 {
        return 1;
    }

    while (a | b) & 0xF == 0 {
        a >>= 4;
        b >>= 4;
        shift += 4;
    }

    if (a | b) & 0x3 == 0 {
        a >>= 2;
        b >>= 2;
        shift += 2;
    }

    if (a | b) & 0x1 == 0 {
        a >>= 1;
        b >>= 1;
        shift += 1;
    }

    let mut t: i64;

    while b != 0 {
        t = b;
        b = a % b;
        a = t;
    }

    a << shift
}

#[cfg(test)]
mod tests {
    use super::gcd;
    use super::Rat;
    use rand::prelude::*;
    use std::cmp::Ordering;

    #[test]
    fn arith() {
        let a = Rat::new(1);
        let b = Rat::new(2);
        assert_eq!((a + b).num, 3);
        assert_eq!((a + b).den, 1);

        assert_eq!((a - b).num, -1);
        assert_eq!((a + b).den, 1);

        assert_eq!(((a + b) * (a - b) + b).num, -1);
    }

    #[test]
    fn overflow() {
        let a = Rat {
            num: (i64::MAX >> 1) + 1,
            den: i64::MAX,
        };
        let b = Rat {
            num: (i64::MAX >> 1) + 3,
            den: i64::MAX,
        };
        let c = a + b;
        assert_eq!(1., f64::from(c));
        assert_eq!(f64::from(a) + f64::from(b), f64::from(c));
    }

    #[test]
    fn gcd_shifts() {
        let a = 16 * 74;
        let b = 16 * 91;

        assert_eq!(16, gcd(a, b));
    }

    #[test]
    fn ordering() {
        let a = Rat::from(2) / Rat::from(3);
        let b = Rat::from(1) / Rat::from(2);

        assert_eq!(Ordering::Greater, a.cmp(&b));

        let a = Rat::from(4);
        let b = Rat::from(5);
        let c = Rat::from(-6);

        assert_eq!(Ordering::Less, a.cmp(&b));
        assert_eq!(Ordering::Less, c.cmp(&b));
    }

    #[test]
    fn overflow_fuzz() {
        let mut rng = SmallRng::seed_from_u64(1);

        fn create_random_rat(rng: &mut SmallRng) -> Rat {
            Rat::from(rng.next_u64() as i64) / Rat::from(rng.next_u64() as i64)
        }

        for _ in 0..1000 {
            let a = create_random_rat(&mut rng);
            let b = create_random_rat(&mut rng);
            let c = create_random_rat(&mut rng);

            let af = f64::from(a);
            let bf = f64::from(b);
            let cf = f64::from(c);

            let d = a + b + c;
            let df = af + bf + cf;

            let tol = 0.001;

            assert!(
                (f64::from(d) - df).abs() < tol,
                "{} {} {} {} {}",
                af,
                bf,
                cf,
                df,
                f64::from(d)
            );
        }
    }
}
