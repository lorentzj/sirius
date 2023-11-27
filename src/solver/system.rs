use super::{
    poly::mono::{grevlex, monomial_div},
    rational::Rat,
    Constraint, Poly, Truth, C,
};
use crate::error::{Error, ErrorType};

use std::collections::HashSet;

#[derive(Clone, PartialEq)]
pub enum ToZero {
    Eq,
    Neq,
    Gt,
    GtEq,
}

#[derive(Clone)]
pub struct R(pub Poly, pub ToZero);

#[derive(Clone, Debug)]
pub struct Relation {
    pub provenance: Vec<Constraint>,
    pub data: R,
}

impl std::fmt::Debug for R {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            R(a, ToZero::Eq) => write!(f, "{a:?} = 0"),
            R(a, ToZero::Neq) => write!(f, "{a:?} != 0"),
            R(a, ToZero::GtEq) => write!(f, "{a:?} >= 0"),
            R(a, ToZero::Gt) => write!(f, "{a:?} > 0"),
        }
    }
}

impl Relation {
    pub fn check(&self) -> Truth {
        if let Some(v) = self.data.0.get_constant_val() {
            match &self.data.1 {
                ToZero::Eq => {
                    if v.is_zero() {
                        Truth::True
                    } else {
                        Truth::False
                    }
                }
                ToZero::Neq => {
                    if v.is_zero() {
                        Truth::False
                    } else {
                        Truth::True
                    }
                }
                ToZero::Gt => {
                    if v > Rat::from(0) {
                        Truth::True
                    } else {
                        Truth::False
                    }
                }
                ToZero::GtEq => {
                    if v >= Rat::from(0) {
                        Truth::True
                    } else {
                        Truth::False
                    }
                }
            }
        } else {
            Truth::Undetermined
        }
    }
}

pub struct System(pub Vec<Relation>);

impl System {
    pub fn new<'a, I>(cs: I) -> Self
    where
        I: IntoIterator<Item = &'a Constraint>,
    {
        let mut parts = vec![];
        for c in cs {
            parts.push(Relation {
                provenance: vec![c.clone()],
                data: match c.data.clone() {
                    C::Eq(a, b) => R(a - b, ToZero::Eq),
                    C::NEq(a, b) => R(a - b, ToZero::Neq),
                    C::Gt(a, b) => R(a - b, ToZero::Gt),
                    C::GtEq(a, b) => R(a - b, ToZero::GtEq),
                },
            });
        }

        System(parts)
    }

    // return degrees of freedom
    pub fn groebner_basis(&mut self) -> usize {
        // buchberger

        let mut eq_z: Vec<_> = self
            .0
            .iter()
            .filter(|x| x.data.1 == ToZero::Eq)
            .cloned()
            .collect();

        let mut combs = {
            let mut combs = vec![];
            for i in 0..eq_z.len() {
                for j in 0..eq_z.len() {
                    if i != j {
                        combs.push((eq_z[i].clone(), eq_z[j].clone()));
                    }
                }
            }

            combs
        };

        while let Some((a, mut b)) = combs.pop() {
            let s = Poly::s_poly(a.data.0, b.data.0);
            let (_, rem) = s.compound_divide(&eq_z.iter().map(|s| s.data.0.clone()).collect());

            if !rem.is_zero() {
                let mut rem_provenance = a.provenance.clone();
                rem_provenance.append(&mut b.provenance);

                let rem = Relation {
                    provenance: rem_provenance,
                    data: R(rem, ToZero::Eq),
                };

                for member in &eq_z {
                    combs.push((member.clone(), rem.clone()));
                }
                eq_z.push(rem);
            }
        }

        // reduce

        let mut keep = vec![];

        for i in 0..eq_z.len() {
            let mut divides_any = false;

            for j in 0..eq_z.len() {
                if i != j {
                    let i_lt = eq_z[i].data.0.lt_mono();
                    let j_lt = eq_z[j].data.0.lt_mono();
                    if let Some(m) = monomial_div(&i_lt, &j_lt) {
                        if m.vars.is_empty() {
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
                keep.push(eq_z[i].clone());
            }
        }

        let mut keep2 = vec![];

        for (i, k) in keep.iter().enumerate() {
            let (_, rem) = k.data.0.compound_divide(
                &keep
                    .iter()
                    .enumerate()
                    .filter_map(|(j, p)| if j != i { Some(p.data.0.clone()) } else { None })
                    .collect(),
            );
            keep2.push(Relation {
                data: R(rem.norm(), ToZero::Eq),
                provenance: k.provenance.clone(),
            });
        }

        keep2.sort_by(|p, q| grevlex(&p.data.0.lt_mono(), &q.data.0.lt_mono()).reverse());
        let deg_freedom = keep2.len();

        let mut new_rels: Vec<_> = self
            .0
            .iter()
            .filter(|x| x.data.1 != ToZero::Eq)
            .cloned()
            .collect();
        new_rels.extend(keep2);

        self.0 = new_rels;
        deg_freedom
    }

    pub fn apply_subs(&mut self) {
        let mut new_rels: Vec<Relation> = self.0.clone();
        let mut subbed: HashSet<String> = HashSet::new();
        let mut to_remove: Vec<usize> = vec![];

        for (i, rel) in self.0.iter().enumerate() {
            if rel.data.1 == ToZero::Eq {
                for var in rel.data.0.var_list() {
                    if let Some(new_sub_val) = rel.data.0.solve_for(&var) {
                        if !subbed.contains(&var) {
                            subbed.insert(var.clone());
                            for new_rel in &mut new_rels {
                                let old_rel_val = new_rel.data.0.clone();
                                new_rel.data.0 =
                                    new_rel.data.0.eval_poly(&var, new_sub_val.clone());
                                if old_rel_val != new_rel.data.0 {
                                    for c in &rel.provenance {
                                        if !new_rel.provenance.contains(c) {
                                            new_rel.provenance.push(c.clone());
                                        }
                                    }
                                }
                            }

                            to_remove.push(i);
                            break;
                        }
                    }
                }
            }
        }

        new_rels = new_rels
            .into_iter()
            .enumerate()
            .filter_map(|(i, x)| {
                if to_remove.contains(&i) {
                    None
                } else {
                    Some(x)
                }
            })
            .collect();

        *self = System(new_rels);
    }

    pub fn check(&mut self) -> Vec<Error> {
        let mut new_rels = vec![];
        let mut errors: Vec<Error> = vec![];

        self.apply_subs();

        if !errors.is_empty() {
            return errors;
        }

        for rel in &self.0 {
            match rel.check() {
                Truth::True => (),
                Truth::False => {
                    errors.push(Error::new(
                        ErrorType::Constraint,
                        format!(
                            "cannot satisfy constraint \"{:?} {:?}\"",
                            rel.data, rel.provenance
                        ),
                        rel.provenance[0].start,
                        rel.provenance[0].end,
                    ));
                }
                Truth::Undetermined => new_rels.push(rel.clone()),
            }
        }

        *self = System(new_rels);

        let mut dedup_errors = vec![];
        for error in &errors {
            if !dedup_errors.contains(error) {
                dedup_errors.push(error.clone());
            }
        }

        dedup_errors
    }

    pub fn gen_eq_zs(&self) -> Vec<Poly> {
        self.0
            .iter()
            .filter_map(|s| {
                if s.data.1 == ToZero::Eq {
                    Some(s.data.0.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
    }

    pub fn free_eq_vars(&self) -> HashSet<String> {
        let mut vs = HashSet::new();

        for rel in &self.0 {
            if rel.data.1 == ToZero::Eq {
                for var in rel.data.0.var_list() {
                    if crate::typechecker::types::Type::ind_var_is_free(&var) {
                        vs.insert(var);
                    }
                }
            }
        }

        vs
    }
}

#[cfg(test)]
mod tests {
    use super::System;
    use crate::{poly, solver::Constraint};

    #[test]
    fn gb() {
        let mut sys = System::new(
            vec![
                Constraint::new_eq(poly!(x + y ^ 2 + z), poly!(0)),
                Constraint::new_eq(poly!(x - y + 3 * z + 5), poly!(0)),
                Constraint::new_eq(poly!(x - 2 * y + 3), poly!(0)),
            ]
            .iter(),
        );

        assert_eq!(3, sys.groebner_basis());

        assert_eq!(
            "[9z^2 + 7z - 3, x + 6z + 7, y + 3z + 2]",
            format!(
                "{:?}",
                sys.0.iter().map(|s| s.data.0.clone()).collect::<Vec<_>>()
            )
        );
    }
}
