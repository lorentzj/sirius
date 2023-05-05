use serde::{Serialize, Serializer};
use std::cmp::Ordering;
use std::fmt;

use super::ind::Ind;
use crate::error::{Error, ErrorType};

#[derive(PartialEq, Clone)]
pub enum Type {
    Unknown,
    Void,
    F64,
    I64(Option<Ind>),
    Bool,
    Tuple(Vec<Type>),
    Function(Vec<String>, Vec<Type>, Box<Type>),
    TypeVar(String),
    ForAll(usize),
}

#[derive(Serialize, Clone, PartialEq, Eq, Debug)]
pub enum C {
    Eq(Ind, Ind),
}

#[derive(Serialize, Clone, Debug)]
pub struct Constraint {
    pub start: usize,
    pub data: C,
    pub end: usize,
}

impl PartialEq for Constraint {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl Constraint {
    pub fn new_eq(lhs: Ind, rhs: Ind) -> Self {
        Constraint {
            start: 0,
            data: C::Eq(lhs, rhs),
            end: 0,
        }
    }

    pub fn apply_pos(cs: &mut Vec<Self>, start: usize, end: usize) {
        for c in cs {
            c.start = start;
            c.end = end;
        }
    }
}

impl Eq for Constraint {}

type Substitution = (usize, Type);

#[derive(Debug)]
pub struct Substitutions(Vec<Substitution>);

impl Substitutions {
    pub fn push(
        &mut self,
        start: usize,
        sub: Substitution,
        end: usize,
    ) -> Result<Vec<Constraint>, Error> {
        let (sub_i, sub_t) = sub;
        for (curr_sub_i, curr_sub_t) in self.0.iter() {
            if *curr_sub_i == sub_i {
                match curr_sub_t.unify(&sub_t) {
                    Some((_, cs)) => return Ok(cs),
                    None => {
                        return Err(Error::new(
                            ErrorType::Type,
                            format!("cannot unify types \"{curr_sub_t:?}\" and \"{sub_t:?}\""),
                            start,
                            end,
                        ))
                    }
                }
            }
        }

        self.0.push((sub_i, sub_t));

        Ok(vec![])
    }

    pub fn extend(
        &mut self,
        start: usize,
        subs: Substitutions,
        end: usize,
    ) -> Result<Vec<Constraint>, Error> {
        let mut constraints = vec![];
        for sub in subs.0 {
            constraints.extend(self.push(start, sub, end)?);
        }

        Ok(constraints)
    }

    pub fn iter(&self) -> std::slice::Iter<Substitution> {
        self.0.iter()
    }

    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl Default for Substitutions {
    fn default() -> Self {
        Self::new()
    }
}

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

    pub fn instantiate_fn(&self, vars: &[String], subs: &[Type]) -> Type {
        match self {
            Type::Tuple(v) => Type::Tuple(v.iter().map(|t| t.instantiate_fn(vars, subs)).collect()),
            Type::Function(t_args, i, o) => Type::Function(
                t_args.clone(),
                i.iter().map(|t| t.instantiate_fn(vars, subs)).collect(),
                Box::new(o.instantiate_fn(vars, subs)),
            ),
            Type::TypeVar(name) => match vars.iter().position(|n| n == name) {
                Some(i) => subs[i].clone(),
                None => self.clone(),
            },
            _ => self.clone(),
        }
    }

    pub fn unify(&self, other: &Type) -> Option<(Substitutions, Vec<Constraint>)> {
        match self {
            Type::Unknown => Some((Substitutions::new(), vec![])),
            Type::ForAll(self_i) => match other {
                Type::ForAll(other_i) => match self_i.cmp(other_i) {
                    Ordering::Greater => {
                        Some((Substitutions(vec![(*self_i, other.clone())]), vec![]))
                    }
                    Ordering::Less => Some((Substitutions(vec![(*other_i, self.clone())]), vec![])),
                    Ordering::Equal => Some((Substitutions::new(), vec![])),
                },
                _ => Some((Substitutions(vec![(*self_i, other.clone())]), vec![])),
            },
            _ => match other {
                Type::Unknown => Some((Substitutions::new(), vec![])),
                Type::Void => {
                    if matches!(self, Type::Void) {
                        Some((Substitutions::new(), vec![]))
                    } else {
                        None
                    }
                }
                Type::Bool => {
                    if matches!(self, Type::Bool) {
                        Some((Substitutions::new(), vec![]))
                    } else {
                        None
                    }
                }
                Type::F64 => {
                    if matches!(self, Type::F64) {
                        Some((Substitutions::new(), vec![]))
                    } else {
                        None
                    }
                }
                Type::TypeVar(l_name) => {
                    if let Type::TypeVar(r_name) = self {
                        if l_name == r_name {
                            Some((Substitutions::new(), vec![]))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                Type::I64(Some(other_i)) => match self {
                    Type::I64(Some(self_i)) => Some((
                        Substitutions::new(),
                        vec![Constraint::new_eq(other_i.clone(), self_i.clone())],
                    )),
                    _ => None,
                },
                Type::I64(None) => {
                    if matches!(self, Type::I64(_)) {
                        Some((Substitutions::new(), vec![]))
                    } else {
                        None
                    }
                }
                Type::ForAll(i) => Some((Substitutions(vec![(*i, self.clone())]), vec![])),
                Type::Tuple(l_types) => {
                    if let Type::Tuple(r_types) = self {
                        if l_types.len() != r_types.len() {
                            None
                        } else {
                            let mut subs = Substitutions::new();
                            let mut constraints = vec![];
                            for (l_type, r_type) in l_types.iter().zip(r_types) {
                                let (inner_subs, inner_constraints) = Type::unify(l_type, r_type)?;
                                if subs.extend(0, inner_subs, 0).is_err() {
                                    return None;
                                }
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
                            let mut subs = Substitutions::new();
                            let mut constraints = vec![];
                            for (l_type, r_type) in l_i.iter().zip(r_i) {
                                let (inner_subs, inner_constraints) = Type::unify(l_type, r_type)?;

                                if subs.extend(0, inner_subs, 0).is_err() {
                                    return None;
                                }
                                constraints.extend(inner_constraints);
                            }

                            let (o_subs, o_constraints) = Type::unify(l_o, r_o)?;
                            constraints.extend(o_constraints);

                            if subs.extend(0, o_subs, 0).is_err() {
                                None
                            } else {
                                Some((subs, constraints))
                            }
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
        if universal {
            name = name.to_uppercase();
        }
        Type::I64(Some(Ind::var(&name)))
    }

    pub fn ind_var_is_universal(ind_var_name: &str) -> bool {
        let mut cs = ind_var_name.chars();
        match cs.next() {
            Some('\'') => cs.next().unwrap().is_lowercase(),
            _ => true,
        }
    }

    pub fn promote_inds(&mut self, curr_ind_forall_var: &mut usize) {
        match self {
            Type::I64(None) => *self = Type::new_free_ind(curr_ind_forall_var, true),
            Type::Tuple(inner) => {
                for t in inner.iter_mut() {
                    t.promote_inds(curr_ind_forall_var)
                }
            }
            _ => (),
        }
    }

    pub fn demote_inds(&mut self) {
        match self {
            Type::I64(_) => *self = Type::I64(None),
            Type::Tuple(inner) => {
                for t in inner.iter_mut() {
                    t.demote_inds()
                }
            }
            _ => (),
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
            Some(ind) => format!("i64(ind={ind:?})"),
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
    use super::{usize_name, Ind, Type};

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
            format!("{:?}", Type::I64(Some(Ind::constant(1)))),
            "i64(ind=1)"
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
}
