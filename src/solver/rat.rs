use num::{Zero, BigInt, BigRational, ToPrimitive};
use std::ops;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Rat(pub BigRational);

impl Rat {
    pub fn from(i: i64) -> Self {
        Self(BigRational::from_integer(BigInt::from(i)))
    }

    pub fn one() -> Self {
        Rat::from(1)
    }

    pub fn zero() -> Self {
        Rat::from(0)
    }

    pub fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}

impl ops::Add<Rat> for Rat {
    type Output = Self;

    fn add(self, rhs: Rat) -> Rat {
        Rat(self.0 + rhs.0)
    }
}

impl ops::Sub<Rat> for Rat {
    type Output = Self;

    fn sub(self, rhs: Rat) -> Rat {
        Rat(self.0 - rhs.0)
    }
}

impl ops::Mul<Rat> for Rat {
    type Output = Self;

    fn mul(self, rhs: Rat) -> Rat {
        Rat(self.0 * rhs.0)
    }
}

impl ops::Div<Rat> for Rat {
    type Output = Self;

    fn div(self, rhs: Rat) -> Rat {
        Rat(self.0 / rhs.0)
    }
}

impl fmt::Display for Rat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.to_f64().unwrap_or(f64::NAN))
    }
}