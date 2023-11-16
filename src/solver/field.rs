use std::{cmp::Ord, fmt::Debug, ops};

// ordered field type
pub trait Field = Clone
    + Debug
    + ToString
    + Eq
    + Ord
    + From<i64>
    + Into<f64>
    + ops::Add<Output = Self>
    + ops::Sub<Output = Self>
    + ops::Mul<Output = Self>
    + ops::Mul<i64, Output = Self>
    + ops::Div<Output = Self>
    + Zero
    + One;

pub trait Zero {
    fn zero() -> Self;
    fn is_zero(&self) -> bool;
}

pub trait One {
    fn one() -> Self;
}
