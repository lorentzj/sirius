//! Bounded-degree positivity certificates for non-linear goals.
//!
//! Given the code:
//! ```text
//! fn blocks{A, B}(arr: f32[A*B]) -> f32[A, B]:
//!     for i from 0 to A:
//!         for j from 0 to B:
//!             yield arr[i*B + j]
//! ```
//!
//! The solver is given the goal `i*B + j < A*B` to verify the `arr` access. Linearization
//! replaces `i*B` and `A*B` with fresh atoms, so [`t1`](super::t1) cannot solve the system using the constraint `i < A`.
//!
//! The goal and all other [`Constraint`]s (except [`Ne`](super::Cmp::Ne), a conjunction for [`Eq`](super::Cmp::Eq)) can be written as non-negative facts:
//!
//! $$iB + j < AB \iff AB - iB - j - 1 \geq 0$$
//!
//! Then the goal is a non-negative combination of products of facts already given:
//!
//! $$AB - iB - j - 1 = B(A - i - 1) + 1(B - j - 1)$$
//!
//! Or generally,
//!
//! $$s \cdot \text{goal} = \sum_{k} c_kp_k$$
//! for some constant integers $c\geq0$, products $p$ of non-negative facts, and a scale factor $s$  to permit rational certificates. This is a [Handelman](https://projecteuclid.org/journals/pacific-journal-of-mathematics/volume-132/issue-1/Representing-polynomials-by-positive-linear-functions-on-compact-convex-polyhedra/pjm/1102689794.full)
//! representation (in this case, degree-2).
//!
//! This module searches for such solutions to non-linear systems. We do so by generating $p$ candidates using non-negative monomial divisors of the goal and non-negative facts from context $h$. For example, supposing these six candidate products are generated:
//!
//! |        |                                                  |
//! | ------ | ------------------------------------------------ |
//! | $p_1$  | $1 = 1$                                          |
//! | $p_2$  | $h_1 = A - i - 1$                                |
//! | $p_3$  | $h_2 = B - j - 1$                                |
//! | $p_4$  | $Bh_1 = AB - iB - B$                             |
//! | $p_5$  | $Bh_2 = B^2 - jB - B$                            |
//! | $p_6$  | $h_1h_2 = AB − jA − A − iB + ij + i − B + j + 1$ |
//!
//! We can construct a linear system of equations in $c$ and $s$ which builds the goal, monomial by monomial. Satisfying each equation means selecting $c$ such that the sum-of-products has the correct $M$ term; $\sum_k c_k \cdot \text{coef}_{p_k}(M) = s \cdot \text{coef}\_{\text{goal}}(M)$.
//!
//! |  $M$  | $c_1$ | $c_2$ | $c_3$ | $c_4$ | $c_5$ | $c_6$ | $=s \cdot$ |
//! | ----- | ----- | ----- | ----- | ----- | ----- | ----- | ---------- |
//! | $AB$  |       |       |       |  $1$  |       |  $1$  |        $1$ |
//! | $iB$  |       |       |       | $-1$  |       | $-1$  |       $-1$ |
//! | $j$   |       |       | $-1$  |       |       |  $1$  |       $-1$ |
//! | $1$   |  $1$  | $-1$  | $-1$  |       |       |  $1$  |       $-1$ |
//! | $A$   |       |  $1$  |       |       |       | $-1$  |            |
//! | $i$   |       | $-1$  |       |       |       |  $1$  |            |
//! | $B$   |       |       |  $1$  | $-1$  | $-1$  | $-1$  |            |
//! | $B^2$ |       |       |       |       |  $1$  |       |            |
//! | $jB$  |       |       |       |       | $-1$  |       |            |
//! | $jA$  |       |       |       |       |       | $-1$  |            |
//! | $ij$  |       |       |       |       |       |  $1$  |            |
//!
//! This system can be solved in [`t1`](super::t1), giving $c_3=1, c_4=1$, $s=1$, $\text{rest}=0$. The final sum is $p_3 + p_4 = \text{goal}$.
//!
//! ```
//! use sirius::solver::{Constraint, Cmp, poly::{poly, Var}, t1::Z3, t2::prove};
//!
//! let mut z3 = Z3::new_cli(None).unwrap();
//! let goal = Constraint::new(poly!(i*B + j), Cmp::Lt, poly!(A*B));
//! let facts = [
//!     Constraint::new(poly!(i), Cmp::Lt, poly!(A)),
//!     Constraint::new(poly!(j), Cmp::Lt, poly!(B)),
//! ];
//! let nonneg = [
//!     'i' as Var,
//!     'j' as Var,
//!     'A' as Var,
//!     'B' as Var,
//! ];
//!
//! assert!(prove(&mut z3, &facts, &goal, &nonneg));
//! ```

use crate::solver::poly::coef::Coef;
use crate::solver::poly::{Mono, Poly, Var};
use crate::solver::{Cmp, Constraint, Verdict, t1::Z3};

/// Facts considered, starting with non-negative [`Var`]s, after filtering to those sharing a variable with the goal. [`Cmp::Eq`] constraints generate two facts.
pub const MAX_FACTS: usize = 12;
/// Monomial multipliers considered.
pub const MAX_MULTIPLIERS: usize = 16;

/// Try to prove nonlinear `goal` from `facts`.
pub fn prove(z3: &mut Z3, facts: &[Constraint], goal: &Constraint, nonneg: &[Var]) -> bool {
    let Some(goals) = goal_diffs(goal) else {
        return false;
    };

    // a loop's `i >= 0` lowers to the same hypothesis as `i` being non-negative, so dedup before
    // the MAX_FACTS cut or half the budget goes on repeats
    let mut hypotheses: Vec<Poly> = vec![];
    for &v in nonneg {
        hypotheses.push(Poly::var(v, 1));
    }
    for f in facts {
        hypotheses.extend(fact_diffs(f));
    }
    let mut seen: Vec<Poly> = vec![];
    hypotheses.retain(|h| {
        let fresh = !seen.contains(h);
        if fresh {
            seen.push(h.clone());
        }
        fresh
    });

    goals
        .iter()
        .all(|d| certify(z3, &hypotheses, nonneg, d).is_some())
}

/// `d >= 0` goals whose conjunction is equivalent to `c`. `!=` has no such form.
fn goal_diffs(c: &Constraint) -> Option<Vec<Poly>> {
    let fwd = c.rhs.sub(&c.lhs);
    let back = c.lhs.sub(&c.rhs);
    Some(match c.cmp {
        Cmp::Le => vec![fwd],
        Cmp::Lt => vec![fwd.sub(&Poly::constant(1))],
        Cmp::Ge => vec![back],
        Cmp::Gt => vec![back.sub(&Poly::constant(1))],
        Cmp::Eq => vec![fwd, back],
        Cmp::Ne => return None,
    })
}

/// `f >= 0` facts implied by `c`.
fn fact_diffs(c: &Constraint) -> Vec<Poly> {
    let fwd = c.rhs.sub(&c.lhs);
    let back = c.lhs.sub(&c.rhs);
    match c.cmp {
        Cmp::Le => vec![fwd],
        Cmp::Lt => vec![fwd.sub(&Poly::constant(1))],
        Cmp::Ge => vec![back],
        Cmp::Gt => vec![back.sub(&Poly::constant(1))],
        Cmp::Eq => vec![fwd, back],
        Cmp::Ne => vec![],
    }
}

/// Look for non-negative integers `c_k` and a scale `s >= 1` with `s*d == Σ c_k · p_k`, where each
/// `p_k` is a non-negative product built from the hypotheses. Since every `p_k >= 0`, that forces
/// `d >= 0`. The scale keeps rational certificates in reach without leaving integer arithmetic.
fn certify(
    solver: &mut Z3,
    hypotheses: &[Poly],
    nonneg: &[Var],
    d: &Poly,
) -> Option<Vec<(usize, i128)>> {
    // a hypothesis sharing no variable with the goal cannot contribute a needed monomial, and
    // only inflates the search
    let goal_vars = d.vars();
    let relevant: Vec<&Poly> = hypotheses
        .iter()
        .filter(|h| h.vars().iter().any(|v| goal_vars.contains(v)) || h.vars().is_empty())
        .take(MAX_FACTS)
        .collect();

    // A row of a tiled access needs a monomial multiplier, not just another fact: the certificate
    // for a three-deep nest is `B*C·(A-i-1) + C·(B-j-1) + (C-k-1)`. The multipliers that can
    // appear are divisors of the goal's own monomials, so take those rather than enumerating
    // every monomial up to some degree.
    let multipliers = goal_multipliers(d, nonneg);

    let mut products: Vec<Poly> = vec![];
    for m in &multipliers {
        let m = Poly::term(1, m.clone());
        products.push(m.clone());
        for h in &relevant {
            products.push(m.mul(h));
        }
    }
    for i in 0..relevant.len() {
        for j in i..relevant.len() {
            products.push(relevant[i].mul(relevant[j]));
        }
    }

    // unknowns: products.len() coefficients, then the scale
    let scale = products.len() as Var;
    let mut system = vec![Constraint::new(
        Poly::var(scale, 1),
        Cmp::Ge,
        Poly::constant(1),
    )];

    let mut monos: Vec<Mono> = vec![];
    for p in products.iter().chain(std::iter::once(d)) {
        for (_, m) in p.terms() {
            if !monos.contains(&m) {
                monos.push(m);
            }
        }
    }

    for m in &monos {
        let mut lhs = Poly::zero();
        for (k, p) in products.iter().enumerate() {
            let c = coef_of(p, m);
            if !c.is_zero() {
                lhs = lhs.add(&Poly::var(k as Var, 1).mul_scalar(c));
            }
        }
        let rhs = Poly::var(scale, 1).mul_scalar(coef_of(d, m));
        system.push(Constraint::new(lhs, Cmp::Eq, rhs));
    }

    // the coefficients are the non-negative ones; the scale is bounded below by its own fact
    let nonneg: Vec<Var> = (0..products.len() as Var).collect();
    let names: Vec<String> = (0..=products.len()).map(|k| format!("c{k}")).collect();
    let names: Vec<&str> = names.iter().map(String::as_str).collect();

    // `entails_lia` refutes by exhibiting a model, so asking it to derive a contradiction from the
    // system is how we ask whether the system is satisfiable
    let contradiction = Constraint::new(Poly::zero(), Cmp::Eq, Poly::constant(1));
    match solver.entails_lia(&system, &contradiction, &nonneg, &names) {
        Verdict::RefutedAt(point) => Some(
            point
                .iter()
                .filter(|(v, c)| *v < scale && *c != 0)
                .map(|(v, c)| (*v as usize, *c))
                .collect(),
        ),
        _ => None,
    }
}

/// Divisors of the goal's monomials, restricted to variables known non-negative -- multiplying a
/// hypothesis by one of these keeps it non-negative. Always includes `1`.
fn goal_multipliers(d: &Poly, nonneg: &[Var]) -> Vec<Mono> {
    let mut out = vec![Mono::unit()];
    for (_, m) in d.terms() {
        let exps: Vec<(Var, u32)> = m
            .exps()
            .iter()
            .copied()
            .filter(|(v, _)| nonneg.contains(v))
            .collect();

        // every way of lowering each exponent, `1` included
        let mut divisors: Vec<Vec<(Var, u32)>> = vec![vec![]];
        for &(v, e) in &exps {
            divisors = divisors
                .iter()
                .flat_map(|base| {
                    (0..=e).map(move |k| {
                        let mut next = base.clone();
                        if k > 0 {
                            next.push((v, k));
                        }
                        next
                    })
                })
                .collect();
            if divisors.len() > MAX_MULTIPLIERS {
                break;
            }
        }

        for exps in divisors {
            let mono = Mono::new(exps);
            if !out.contains(&mono) {
                out.push(mono);
            }
        }
        if out.len() >= MAX_MULTIPLIERS {
            break;
        }
    }
    out
}

fn coef_of(p: &Poly, m: &Mono) -> Coef {
    p.terms()
        .iter()
        .find(|(_, pm)| pm == m)
        .map(|(c, _)| *c)
        .unwrap_or(Coef::new(0))
}

#[cfg(test)]
mod tests {
    use super::prove;
    use crate::solver::poly::{Var, poly};
    use crate::solver::{Cmp, Constraint, Z3};

    fn nonneg() -> Vec<Var> {
        vec!['a' as Var, 'b' as Var, 'i' as Var, 'j' as Var]
    }

    #[test]
    fn strided_access() {
        let mut s = Z3::new_cli(None).unwrap();
        // i < A, B > 0  |-  i*B < A*B
        let facts = [
            Constraint::new(poly!(i), Cmp::Lt, poly!(a)),
            Constraint::new(poly!(b), Cmp::Gt, poly!(0)),
        ];
        let goal = Constraint::new(poly!(b * i), Cmp::Lt, poly!(a * b));
        assert!(prove(&mut s, &facts, &goal, &nonneg()));
    }

    #[test]
    fn tiled_access() {
        let mut s = Z3::new_cli(None).unwrap();
        // i < A, j < B, B > 0  |-  i*B + j < A*B
        let facts = [
            Constraint::new(poly!(i), Cmp::Lt, poly!(a)),
            Constraint::new(poly!(j), Cmp::Lt, poly!(b)),
            Constraint::new(poly!(b), Cmp::Gt, poly!(0)),
        ];
        let goal = Constraint::new(poly!(b * i + j), Cmp::Lt, poly!(a * b));
        assert!(prove(&mut s, &facts, &goal, &nonneg()));
    }

    #[test]
    fn unsound_variants_are_refused() {
        let mut s = Z3::new_cli(None).unwrap();

        // without B > 0 the goal is false at B = 0
        let facts = [Constraint::new(poly!(i), Cmp::Lt, poly!(a))];
        let goal = Constraint::new(poly!(b * i), Cmp::Lt, poly!(a * b));
        assert!(!prove(&mut s, &facts, &goal, &nonneg()));

        // A*B >= A + B is false at A = B = 1
        let goal = Constraint::new(poly!(a * b), Cmp::Ge, poly!(a + b));
        assert!(!prove(&mut s, &[], &goal, &nonneg()));
    }

    #[test]
    fn handelman_boundary() {
        let mut s = Z3::new_cli(None).unwrap();

        // perfect square, but would require SDP dark magic
        let goal = Constraint::new(poly!(a ^ 2 - 2 * a * b + b ^ 2), Cmp::Ge, poly!(0));
        assert!(!prove(&mut s, &[], &goal, &nonneg()));
    }
}
