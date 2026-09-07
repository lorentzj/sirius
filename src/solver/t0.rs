//! Try to prove a goal given only a non-negative variable list.

use crate::solver::Cmp;
use crate::solver::poly::{Poly, Var, poly};

/// Try to prove a goal given only a non-negative variable list.
pub fn prove(cmp: Cmp, lhs: &Poly, rhs: &Poly, nonneg: &dyn Fn(Var) -> bool) -> bool {
    match cmp {
        Cmp::Eq => lhs == rhs,
        Cmp::Ne => (lhs.sub(rhs)).as_constant().is_some_and(|c| !c.is_zero()),
        Cmp::Le => less_or_eq(lhs, rhs, nonneg),
        Cmp::Lt => less_or_eq(&(lhs.add(&poly!(1))), rhs, nonneg),
        Cmp::Ge => less_or_eq(rhs, lhs, nonneg),
        Cmp::Gt => less_or_eq(&(rhs.add(&poly!(1))), lhs, nonneg),
    }
}

fn less_or_eq(lhs: &Poly, rhs: &Poly, nonneg: &dyn Fn(Var) -> bool) -> bool {
    lhs == rhs || rhs.sub(lhs).always_nonneg(nonneg)
}

#[cfg(test)]
mod tests {
    use super::{Cmp, Var, poly, prove};

    fn all_but_a(v: Var) -> bool {
        v != 'a' as Var
    }

    #[test]
    fn nonnegative_differences_prove_leq() {
        assert!(prove(Cmp::Le, &poly!(n), &poly!(n + 1), &all_but_a));
        assert!(prove(Cmp::Le, &poly!(0), &poly!(n), &all_but_a));
        assert!(prove(Cmp::Le, &poly!(n), &poly!(2 * n), &all_but_a));

        assert!(!prove(Cmp::Lt, &poly!(0), &poly!(n), &all_but_a));
        assert!(!prove(Cmp::Le, &poly!(1), &poly!(n), &all_but_a));
    }

    #[test]
    fn rigid_sizes_not_nonneg() {
        assert!(!prove(Cmp::Le, &poly!(0), &poly!(a), &all_but_a));
    }
}
