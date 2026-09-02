//! Subtyping. An `Ind(P)` value is in `[0, P)` and a `Size(p)` is exactly `p`, so most
//! coercions come down to a polynomial obligation rather than a structural match.

use crate::error::error_at;
use crate::parser::Pos;
use crate::solver::poly::Poly;
use crate::solver::z3::{Cmp, Constraint};

use super::check::FnChecker;
use super::ty::Type;

impl FnChecker<'_> {
    /// Check that `found` can stand in for `want`, reporting one error if it cannot.
    pub fn coerce(&mut self, found: &Type, want: &Type, pos: &Pos<impl Sized>, ctx: &str) {
        let mut detail = None;
        if !self.coerce_inner(found, want, &mut detail) {
            let names = self.names();
            let mut msg = format!(
                "{ctx}: expected \"{}\", found \"{}\"",
                want.render(&names),
                found.render(&names)
            );
            if let Some(detail) = detail {
                msg.push_str(&format!("; {detail}"));
            }
            self.errors.push(error_at!(Type, pos, "{msg}"));
        }
    }

    fn coerce_inner(&mut self, found: &Type, want: &Type, detail: &mut Option<String>) -> bool {
        match (found, want) {
            (Type::Error, _) | (_, Type::Error) => true,
            (a, b) if a == b => true,
            (Type::Size(p), Type::Ind(bound)) => {
                self.obligation(p, Cmp::Ge, &Poly::zero(), detail)
                    && self.obligation(p, Cmp::Lt, bound, detail)
            }
            (Type::Ind(p), Type::Ind(bound)) => self.obligation(p, Cmp::Le, bound, detail),
            (Type::Size(p), Type::Size(q)) => self.obligation(p, Cmp::Eq, q, detail),
            (Type::Size(_) | Type::Ind(_), Type::I64) => true,
            (
                Type::Array {
                    elem: found_elem,
                    shape: found_shape,
                },
                Type::Array {
                    elem: want_elem,
                    shape: want_shape,
                },
            ) if found_shape.len() == want_shape.len() => {
                for (f, w) in found_shape.iter().zip(want_shape) {
                    if !self.obligation(f, Cmp::Eq, w, detail) {
                        return false;
                    }
                }
                self.coerce_inner(found_elem, want_elem, detail)
            }
            (Type::Tuple(found), Type::Tuple(want)) if found.len() == want.len() => found
                .iter()
                .zip(want)
                .all(|(f, w)| self.coerce_inner(f, w, detail)),
            (Type::Option(found), Type::Option(want)) => self.coerce_inner(found, want, detail),
            _ => false,
        }
    }

    fn obligation(
        &mut self,
        lhs: &Poly,
        cmp: Cmp,
        rhs: &Poly,
        detail: &mut Option<String>,
    ) -> bool {
        match self.prove_quiet(&Constraint::new(lhs.clone(), cmp, rhs.clone())) {
            None => true,
            Some(failure) => {
                detail.get_or_insert(failure);
                false
            }
        }
    }

    /// Least common supertype, used to give an array literal one element type.
    pub fn join(&self, lhs: &Type, rhs: &Type) -> Option<Type> {
        if lhs == rhs {
            return Some(lhs.clone());
        }
        match (lhs, rhs) {
            (Type::Error, t) | (t, Type::Error) => Some(t.clone()),
            (Type::Ind(p), Type::Ind(q)) => Some(if self.t0(Cmp::Le, p, q) {
                Type::Ind(q.clone())
            } else if self.t0(Cmp::Le, q, p) {
                Type::Ind(p.clone())
            } else {
                Type::I64
            }),
            (
                Type::Array {
                    elem: lhs_elem,
                    shape: lhs_shape,
                },
                Type::Array {
                    elem: rhs_elem,
                    shape: rhs_shape,
                },
            ) if lhs_shape == rhs_shape => Some(Type::new_arr(
                self.join(lhs_elem, rhs_elem)?,
                lhs_shape.clone(),
            )),
            _ if lhs.is_int_like() && rhs.is_int_like() => Some(Type::I64),
            _ => None,
        }
    }
}
