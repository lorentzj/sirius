use fmt::Write;
use std::fmt;

use crate::solver::poly::{
    coef::Coef,
    mono::{Mono, Var},
};

use super::Poly;

impl fmt::Debug for Poly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.display_with(None))
    }
}

impl Poly {
    pub fn display_with(&self, var_names: Option<&[&str]>) -> String {
        let mut buf = String::new();

        if self.is_zero() {
            write!(buf, "0").unwrap();
        }

        for (i, (c, m)) in self.terms().iter().enumerate() {
            write_term(&mut buf, m, *c, i == 0, var_names).unwrap();
        }

        buf
    }
}

fn write_term<T: Write>(
    f: &mut T,
    m: &Mono,
    c: Coef,
    first: bool,
    names: Option<&[&str]>,
) -> fmt::Result {
    let neg = !c.is_positive();
    if first {
        if neg {
            f.write_str("-")?;
        }
    } else {
        f.write_str(if neg { " - " } else { " + " })?;
    }

    let mag = c.abs();
    if m.is_unit() {
        write!(f, "{mag:?}")
    } else {
        if mag != 1.into() {
            write!(f, "{mag:?}*")?;
        }
        write_monomial(f, m, names)
    }
}

fn write_monomial<T: Write>(f: &mut T, m: &Mono, names: Option<&[&str]>) -> fmt::Result {
    for (i, &(v, e)) in m.exps().iter().enumerate() {
        if i > 0 {
            f.write_str("*")?;
        }
        write_var(f, v, names)?;
        if e > 1 {
            write!(f, "^{e}")?;
        }
    }
    Ok(())
}

fn write_var<T: Write>(f: &mut T, v: Var, names: Option<&[&str]>) -> fmt::Result {
    match names.and_then(|s| s.get(v as usize)) {
        Some(name) => f.write_str(name),
        None => match char::from_u32(v) {
            Some(c @ 'a'..='z') => write!(f, "{}", c),
            _ => write!(f, "x{}", v),
        },
    }
}
