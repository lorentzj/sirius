use serde::{Serialize, Serializer};
use std::cmp::Ordering;
use std::fmt;

use crate::parser::TypeArg;
use crate::solver::poly::Poly;
use crate::solver::rational::Rat;
use crate::solver::Constraint;

#[derive(PartialEq, Clone)]
pub enum Type {
    Unknown,
    Void,
    F64,
    I64(Option<Poly<Rat>>),
    Bool,
    Tuple(Vec<Type>),
    Function(Vec<TypeArg>, Vec<Type>, Box<Type>),
    TypeVar(String),
    ForAll(usize),
}

pub type Substitution = (usize, Type);

impl Type {
    pub fn forall_vars(&self) -> Vec<usize> {
        match self {
            Type::Tuple(v) => {
                let mut vars = vec![];
                for t in v {
                    vars.extend(t.forall_vars())
                }

                vars.dedup();
                vars
            }
            Type::Function(_, i, o) => {
                let mut vars = vec![];
                for t in i {
                    vars.extend(t.forall_vars())
                }

                vars.extend(o.forall_vars());

                vars.dedup();
                vars
            }
            Type::ForAll(i) => {
                vec![*i]
            }
            _ => vec![],
        }
    }

    pub fn substitute(&self, sub: &Substitution) -> Type {
        let (var, a) = sub;
        match self {
            Type::Tuple(v) => Type::Tuple(v.iter().map(|x| x.substitute(sub)).collect()),
            Type::Function(t_args, i, o) => Type::Function(
                t_args.clone(),
                i.iter().map(|x| x.substitute(sub)).collect(),
                Box::new(o.substitute(sub)),
            ),
            Type::ForAll(i) => {
                if i == var {
                    match a {
                        Type::ForAll(ai) => Type::ForAll(*i.min(ai)),
                        _ => a.clone(),
                    }
                } else {
                    Type::ForAll(*i)
                }
            }
            _ => self.clone(),
        }
    }

    pub fn instantiate_fn(&self, vars: &Vec<TypeArg>, subs: &[Type]) -> Type {
        match self {
            Type::Tuple(v) => Type::Tuple(v.iter().map(|t| t.instantiate_fn(vars, subs)).collect()),
            Type::Function(t_args, i, o) => Type::Function(
                t_args.clone(),
                i.iter().map(|t| t.instantiate_fn(vars, subs)).collect(),
                Box::new(o.instantiate_fn(vars, subs)),
            ),
            Type::TypeVar(name) => match vars.iter().position(|n| &n.name() == name) {
                Some(i) => subs[i].clone(),
                None => self.clone(),
            },
            _ => self.clone(),
        }
    }

    pub fn unify(
        &self,
        other: &Type,
        allow_demote: bool,
        allow_promote: &mut Option<&mut usize>,
    ) -> Option<(Vec<Substitution>, Vec<Constraint>)> {
        if self == other {
            return Some((vec![], vec![]));
        }

        match self {
            Type::Unknown => Some((vec![], vec![])),
            Type::ForAll(self_i) => match other {
                Type::ForAll(other_i) => match self_i.cmp(other_i) {
                    Ordering::Less => Some((vec![(*self_i, other.clone())], vec![])),
                    Ordering::Greater => Some((vec![(*other_i, self.clone())], vec![])),
                    Ordering::Equal => Some((vec![], vec![])),
                },
                _ => Some((vec![(*self_i, other.clone())], vec![])),
            },
            _ => match other {
                Type::Unknown => Some((vec![], vec![])),
                Type::Void => {
                    if matches!(self, Type::Void) {
                        Some((vec![], vec![]))
                    } else {
                        None
                    }
                }
                Type::Bool => {
                    if matches!(self, Type::Bool) {
                        Some((vec![], vec![]))
                    } else {
                        None
                    }
                }
                Type::F64 => {
                    if matches!(self, Type::F64) {
                        Some((vec![], vec![]))
                    } else {
                        None
                    }
                }
                Type::TypeVar(l_name) => {
                    if let Type::TypeVar(r_name) = self {
                        if l_name == r_name {
                            Some((vec![], vec![]))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                Type::I64(Some(other_i)) => match self {
                    Type::I64(Some(self_i)) => Some((
                        vec![],
                        vec![Constraint::new_eq(other_i.clone(), self_i.clone())],
                    )),
                    Type::I64(None) => allow_promote.as_mut().map(|var| {
                        (
                            vec![],
                            vec![Constraint::new_eq(
                                other_i.clone(),
                                Poly::var(usize_name(**var), 1),
                            )],
                        )
                    }),
                    _ => None,
                },
                Type::I64(None) => match self {
                    Type::I64(None) => Some((vec![], vec![])),
                    Type::I64(_) => {
                        if allow_demote {
                            Some((vec![], vec![]))
                        } else {
                            None
                        }
                    }
                    _ => None,
                },
                Type::ForAll(i) => Some((vec![(*i, self.clone())], vec![])),
                Type::Tuple(l_types) => {
                    if let Type::Tuple(r_types) = self {
                        if l_types.len() != r_types.len() {
                            None
                        } else {
                            let mut subs: Vec<Substitution> = vec![];
                            let mut constraints = vec![];
                            for (l_type, r_type) in l_types.iter().zip(r_types) {
                                let (mut inner_subs, mut inner_constraints) =
                                    Type::unify(l_type, r_type, allow_demote, allow_promote)?;
                                let mut combined_subs = vec![];
                                for (s_var, s) in &subs {
                                    for (i_s_var, i_s) in &inner_subs {
                                        if s_var == i_s_var {
                                            let (comb_subs, comb_constraints) =
                                                s.unify(i_s, allow_demote, allow_promote)?;
                                            combined_subs.extend(comb_subs);
                                            inner_constraints.extend(comb_constraints);
                                        }
                                    }
                                }
                                inner_subs.extend(combined_subs);
                                subs.extend(inner_subs);
                                constraints.extend(inner_constraints);
                            }

                            Some((subs, constraints))
                        }
                    } else {
                        None
                    }
                }
                Type::Function(_, l_i, l_o) => {
                    if let Type::Function(_, r_i, r_o) = self {
                        if l_i.len() != r_i.len() {
                            None
                        } else {
                            let mut subs: Vec<Substitution> = vec![];
                            let mut constraints = vec![];
                            for (l_type, r_type) in l_i.iter().zip(r_i) {
                                let (mut inner_subs, mut inner_constraints) =
                                    Type::unify(l_type, r_type, allow_demote, allow_promote)?;
                                let mut combined_subs = vec![];
                                for (s_var, s) in &subs {
                                    for (i_s_var, i_s) in &inner_subs {
                                        if s_var == i_s_var {
                                            let (comb_subs, comb_constraints) =
                                                s.unify(i_s, allow_demote, allow_promote)?;
                                            combined_subs.extend(comb_subs);
                                            inner_constraints.extend(comb_constraints);
                                        }
                                    }
                                }
                                inner_subs.extend(combined_subs);
                                subs.extend(inner_subs);
                                constraints.extend(inner_constraints);
                            }

                            let (mut o_subs, mut o_constraints) =
                                Type::unify(l_o, r_o, allow_demote, allow_promote)?;
                            let mut combined_subs = vec![];

                            for (s_var, s) in &subs {
                                for (o_s_var, o_s) in &o_subs {
                                    if s_var == o_s_var {
                                        let (comb_subs, comb_constraints) =
                                            s.unify(o_s, allow_demote, allow_promote)?;
                                        combined_subs.extend(comb_subs);
                                        o_constraints.extend(comb_constraints);
                                    }
                                }
                            }
                            o_subs.extend(combined_subs);
                            subs.extend(o_subs);
                            constraints.extend(o_constraints);
                            Some((subs, constraints))
                        }
                    } else {
                        None
                    }
                }
            },
        }
    }

    pub fn new_free_ind(curr_ind_forall_var: &mut usize, universal: bool) -> Type {
        *curr_ind_forall_var += 1;
        let mut name = "'".to_string() + &usize_name(*curr_ind_forall_var - 1);
        if !universal {
            name = name.to_uppercase();
        }
        Type::I64(Some(Poly::var(name, 1)))
    }

    pub fn ind_var_is_universal(ind_var_name: &str) -> bool {
        let mut cs = ind_var_name.chars();
        match cs.next() {
            Some('\'') => cs.next().unwrap().is_lowercase(),
            _ => true,
        }
    }

    pub fn promote_inds(&self, curr_ind_forall_var: &mut usize) -> Type {
        match self {
            Type::I64(None) => Type::new_free_ind(curr_ind_forall_var, true),
            Type::Tuple(inner) => {
                let mut new_inner = vec![];
                for t in inner.iter() {
                    new_inner.push(t.promote_inds(curr_ind_forall_var))
                }
                Type::Tuple(new_inner)
            }
            _ => self.clone(),
        }
    }

    pub fn allow_possible_promotion(&self, curr_forall_var: &mut usize) -> Type {
        match self {
            Type::I64(None) => {
                *curr_forall_var += 1;
                Type::ForAll(*curr_forall_var - 1)
            }
            Type::Tuple(inner) => {
                let mut new_inner = vec![];
                for t in inner.iter() {
                    new_inner.push(t.allow_possible_promotion(curr_forall_var))
                }
                Type::Tuple(new_inner)
            }
            _ => self.clone(),
        }
    }

    pub fn demote_inds(&self) -> Type {
        match self {
            Type::I64(_) => Type::I64(None),
            Type::Tuple(inner) => {
                let mut new_inner = vec![];

                for t in inner.iter() {
                    new_inner.push(t.demote_inds());
                }
                Type::Tuple(new_inner)
            }
            _ => self.clone(),
        }
    }
}

impl Serialize for Type {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(&format!("{self:?}"))
    }
}

fn usize_name(mut x: usize) -> String {
    let mut res = String::new();

    loop {
        res.insert(0, ((x % 26 + 97) as u8) as char);
        x /= 26;
        if x == 0 {
            break;
        }
        x -= 1;
    }

    res
}

fn priv_print(t: &Type) -> String {
    match t {
        Type::Unknown => "unknown".into(),
        Type::Void => "void".into(),
        Type::F64 => "f64".into(),
        Type::I64(ind) => match ind {
            Some(ind) => format!("i64(p={ind:?})"),
            None => "i64".into(),
        },
        Type::Bool => "bool".into(),
        Type::Tuple(v) => {
            if v.is_empty() {
                "()".into()
            } else {
                let mut res = "(".to_string();
                for t in v {
                    res.push_str(&priv_print(t));
                    res.push(',');
                    res.push(' ');
                }
                res.pop();
                res.pop();
                res.push(')');
                res
            }
        }
        Type::Function(_, i, o) => {
            let mut res = "".to_string();
            if i.len() == 1 {
                res.push_str(&format!("{}->", &priv_print(&i[0])));
            } else {
                res.push('(');
                for t in i {
                    res.push_str(&priv_print(t));
                    res.push(',');
                    res.push(' ');
                }

                if !i.is_empty() {
                    res.pop();
                    res.pop();
                }
                res.push_str(")->");
            }

            res.push_str(&priv_print(o));

            res
        }
        Type::TypeVar(var) => var.clone(),
        Type::ForAll(i) => format!("'{}", usize_name(*i)),
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let forall_vars = self.forall_vars();
        if !forall_vars.is_empty() && !matches!(self, Type::ForAll(_)) {
            write!(f, "forall ")?;
            for (i, var) in forall_vars.iter().enumerate() {
                write!(f, "{}", usize_name(*var))?;
                if i < forall_vars.len() - 1 {
                    write!(f, ", ")?;
                }
            }

            write!(f, " . ")?;
        }
        write!(f, "{}", priv_print(self))
    }
}

#[cfg(test)]
mod tests {
    use super::{usize_name, Type};
    use crate::solver::{poly::Poly, rational::Rat};

    #[test]
    fn usize_name_test() {
        assert_eq!(usize_name(0), "a");
        assert_eq!(usize_name(1), "b");
        assert_eq!(usize_name(25), "z");
        assert_eq!(usize_name(26), "aa");
        assert_eq!(usize_name(27), "ab");
        assert_eq!(usize_name(28), "ac");
        assert_eq!(usize_name(700), "zy");
        assert_eq!(usize_name(701), "zz");
        assert_eq!(usize_name(702), "aaa");
        assert_eq!(usize_name(703), "aab");
    }

    #[test]
    fn print_types() {
        assert_eq!(format!("{:?}", Type::Unknown), "unknown");
        assert_eq!(format!("{:?}", Type::Void), "void");
        assert_eq!(format!("{:?}", Type::Bool), "bool");
        assert_eq!(format!("{:?}", Type::F64), "f64");
        assert_eq!(format!("{:?}", Type::I64(None)), "i64");
        assert_eq!(
            format!("{:?}", Type::I64(Some(Poly::constant(Rat::from(1))))),
            "i64(p=1)"
        );
        assert_eq!(format!("{:?}", Type::TypeVar("T".to_string())), "T");
        assert_eq!(
            format!(
                "{:?}",
                Type::Function(vec![], vec![Type::I64(None)], Box::new(Type::Bool))
            ),
            "i64->bool"
        );
        assert_eq!(format!("{:?}", Type::ForAll(0)), "'a");
        assert_eq!(format!("{:?}", Type::Bool), "bool");
        assert_eq!(
            format!(
                "{:?}",
                Type::Tuple(vec![Type::Bool, Type::ForAll(0), Type::ForAll(1)])
            ),
            "forall a, b . (bool, 'a, 'b)"
        );

        assert_eq!(
            format!(
                "{:?}",
                Type::Tuple(vec![Type::Bool, Type::ForAll(0), Type::ForAll(1)])
                    .substitute(&(0, Type::F64))
            ),
            "forall b . (bool, f64, 'b)"
        );

        assert_eq!(
            format!(
                "{:?}",
                Type::Tuple(vec![Type::Bool, Type::ForAll(0), Type::ForAll(1)])
                    .substitute(&(1, Type::I64(None)))
                    .substitute(&(0, Type::F64))
            ),
            "(bool, f64, i64)"
        );

        assert_eq!(
            format!(
                "{:?}",
                Type::Function(
                    vec![],
                    vec![
                        Type::Tuple(vec![Type::ForAll(0), Type::ForAll(0), Type::ForAll(0)]),
                        Type::Function(vec![], vec![Type::ForAll(0)], Box::new(Type::ForAll(1)))
                    ],
                    Box::new(Type::Tuple(vec![
                        Type::ForAll(1),
                        Type::ForAll(1),
                        Type::ForAll(1)
                    ]))
                )
            ),
            "forall a, b . (('a, 'a, 'a), 'a->'b)->('b, 'b, 'b)"
        )
    }

    #[test]
    fn unify() {
        let a = Type::ForAll(0);
        let b = Type::Bool;

        assert_eq!(
            vec![(0, Type::Bool)],
            a.unify(&b, false, &mut None).unwrap().0
        );
    }
}
