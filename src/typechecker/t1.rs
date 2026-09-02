use super::t0;
use crate::solver::poly::{Mono, Poly, Var};
use crate::solver::z3::{Constraint, Solver, Verdict};

pub struct Linearized {
    pub facts: Vec<Constraint>,
    pub goal: Constraint,
    pub nonneg: Vec<Var>,
    pub pure_linear: bool,
    pub names: Vec<String>,
}

impl Linearized {
    pub fn new(
        facts: &[Constraint],
        goal: &Constraint,
        vars: &[&str],
        nonneg: &dyn Fn(Var) -> bool,
    ) -> Self {
        let base = vars.len() as u32;
        let mut atoms: Vec<Mono> = Vec::new();
        let mut linearize_poly = |p: &Poly| -> Poly {
            let terms = p.terms().into_iter().map(|(c, m)| {
                if m.total_degree() <= 1 {
                    (c, m.clone())
                } else {
                    let idx = match atoms.iter().position(|a| *a == m) {
                        Some(i) => i,
                        None => {
                            atoms.push(m.clone());
                            atoms.len() - 1
                        }
                    };
                    (c, Mono::new([(base + idx as u32, 1)]))
                }
            });
            Poly::from_terms(terms)
        };

        let mut linearize_constraint = |c: &Constraint| Constraint {
            lhs: linearize_poly(&c.lhs),
            cmp: c.cmp,
            rhs: linearize_poly(&c.rhs),
        };

        let facts: Vec<Constraint> = facts.iter().map(&mut linearize_constraint).collect();
        let goal = linearize_constraint(goal);

        let mut nonneg_vars: Vec<Var> = (0..base).filter(|&v| nonneg(v)).collect();
        let mut names: Vec<String> = vars.iter().map(|&s| s.into()).collect();
        for (i, m) in atoms.iter().enumerate() {
            // An atom over nonneg variables is nonneg
            if m.exps().iter().all(|&(v, _)| nonneg(v)) {
                nonneg_vars.push(base + i as u32);
            }
            let rendered: Vec<String> = m
                .exps()
                .iter()
                .map(|&(v, e)| {
                    let name = vars.get(v as usize).copied().unwrap_or("?");
                    if e > 1 {
                        format!("{name}^{e}")
                    } else {
                        name.to_string()
                    }
                })
                .collect();
            names.push(rendered.join("*"));
        }

        Self {
            pure_linear: atoms.is_empty(),
            facts,
            goal,
            nonneg: nonneg_vars,
            names,
        }
    }

    pub fn entails_lia(&self, solver: &mut Solver) -> Verdict {
        if self.facts.contains(&self.goal) {
            return Verdict::Proved;
        }

        if t0::prove(self.goal.cmp, &self.goal.lhs, &self.goal.rhs, &|v| {
            self.nonneg.contains(&v)
        }) {
            return Verdict::Proved;
        }

        let name_strs: Vec<_> = self.names.iter().map(String::as_str).collect();
        solver.entails_lia(&self.facts, &self.goal, &self.nonneg, &name_strs)
    }
}

pub fn prove(
    solver: &mut Solver,
    facts: &[Constraint],
    goal: &Constraint,
    names: &[String],
    nonneg: &[Var],
) -> Option<String> {
    let names: Vec<&str> = names.iter().map(String::as_str).collect();
    let lin = Linearized::new(facts, goal, &names, &|v| nonneg.contains(&v));

    match lin.entails_lia(solver) {
        Verdict::Proved => None,
        Verdict::Refuted(model) => {
            let mut msg = format!("cannot prove \"{}\"", goal.display_with(&names));

            // a model over linearized atoms need not be a real counterexample, so only quote
            // one when nothing was abstracted away
            if lin.pure_linear {
                let mut goal_vars = goal.lhs.vars();
                goal_vars.extend(goal.rhs.vars());
                let point: Vec<String> = model
                    .iter()
                    .filter(|(v, _)| goal_vars.contains(v))
                    .map(|(v, val)| format!("{} = {val}", names.get(*v as usize).unwrap_or(&"?")))
                    .collect();
                if !point.is_empty() {
                    msg.push_str(&format!("; counterexample: {}", point.join(", ")));
                }
            }
            Some(msg)
        }
        Verdict::Unknown => Some(format!("cannot decide \"{}\"", goal.display_with(&names))),
    }
}

#[cfg(test)]
mod tests {
    use super::Linearized;
    use crate::solver::poly::{Poly, poly};
    use crate::solver::z3::{Cmp, Constraint, Solver, Verdict};

    #[test]
    fn linearized() {
        let vars = ["N", "M"];
        let nm = Poly::var(0u32, 1).mul(&Poly::var(1u32, 1));

        let fact = Constraint {
            lhs: nm.clone(),
            cmp: Cmp::Ge,
            rhs: poly!(4),
        };
        let goal = Constraint {
            lhs: poly!(4),
            cmp: Cmp::Le,
            rhs: nm,
        };

        let lin = Linearized::new(&[fact], &goal, &vars, &|_| true);
        assert!(!lin.pure_linear);
        assert_eq!(lin.facts[0].lhs, lin.goal.rhs);
        assert_eq!(lin.facts[0].lhs, Poly::var(2u32, 1));

        assert!(lin.nonneg.contains(&2));
        assert_eq!(lin.names[2], "N*M");
        assert_eq!(
            lin.entails_lia(&mut Solver::new(None).unwrap()),
            Verdict::Proved
        );
    }
}
