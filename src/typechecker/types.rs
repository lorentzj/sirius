use serde::Serialize;
use std::{cmp::Ordering, fmt};

use super::ind::Ind;

#[allow(dead_code)]
#[derive(PartialEq, Serialize, Clone)]
enum Type {
    Unknown,
    Void,
    F64,
    I64(Option<Ind>),
    Bool,
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    TypeVar(String),
    ForAll(u8),
}

#[allow(dead_code)]
impl Type {
    fn forall_count(&self) -> u8 {
        match self {
            Type::Unknown => 0,
            Type::Void => 0,
            Type::F64 => 0,
            Type::I64 { .. } => 0,
            Type::Bool => 0,
            Type::Tuple(v) => {
                let mut n = 0;

                for t in v {
                    n = n.max(t.forall_count());
                }

                n
            }
            Type::Function(i, o) => {
                let mut n = 0;

                for t in i {
                    n = n.max(t.forall_count());
                }

                n = n.max(o.forall_count());

                n
            }
            Type::TypeVar(_) => 0,
            Type::ForAll(i) => *i + 1,
        }
    }

    fn substitute(&self, var: u8, a: &Type) -> Type {
        match self {
            Type::Tuple(v) => Type::Tuple(v.iter().map(|x| x.substitute(var, a)).collect()),
            Type::Function(i, o) => Type::Function(
                i.iter().map(|x| x.substitute(var, a)).collect(),
                Box::new(o.substitute(var, a)),
            ),
            Type::ForAll(i) => match i.cmp(&var) {
                Ordering::Equal => a.clone(),
                Ordering::Greater => Type::ForAll(i - 1),
                Ordering::Less => Type::ForAll(*i),
            },
            _ => self.clone(),
        }
    }

    // fn unify(&self, a: &Type) -> Option<Type> {
    //     match a {
    //         Type::Unknown => Some(Type::Unknown),
    //         _ => match self {
    //             Type::Unknown => Some(Type::Unknown),
    //             Type::Void => if matches!(a, Type::Void) {
    //                 Some(Type::Void)
    //             } else {
    //                 None
    //             },
    //             Type::F64 => if matches!(a, Type::F64) {
    //                 Some(Type::F64)
    //             } else {
    //                 None
    //             },
    //             Type::I64 { nat: l_nat } => {
    //                 if let Type::I64 { nat: r_nat } = a {
    //                     if let Some(r_nat) = r_nat {
    //                         if let Some(l_nat) = l_nat {
    //                             if *l_nat == *r_nat {
    //                                 Some(Type::I64 { nat: Some(*l_nat) })
    //                             } else {
    //                                 None
    //                             }
    //                         } else {
    //                             Some(Type::I64 { nat: Some(*r_nat) })
    //                         }
    //                     } else {
    //                         if let Some(l_nat) = l_nat {
    //                             Some(Type::I64 { nat: Some(*l_nat) })
    //                         } else {
    //                             Some(Type::I64 { nat: None })
    //                         }
    //                     }
    //                 } else {
    //                     None
    //                 }
    //             }
    //         }
    //     }
    // }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let forall = self.forall_count();
        if forall > 0 && !matches!(self, Type::ForAll(_)) {
            write!(f, "forall ")?;
            for i in 0..forall {
                write!(f, "{}", (i + 97) as char)?;
                if i < forall - 1 {
                    write!(f, ", ")?;
                }
            }

            write!(f, " . ")?;
        }

        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Void => write!(f, "void"),
            Type::F64 => write!(f, "f64"),
            Type::I64(ind) => match ind {
                Some(ind) => write!(f, "i64(ind={ind:?})"),
                None => write!(f, "i64"),
            },
            Type::Bool => write!(f, "bool"),
            Type::Tuple(v) => {
                if v.is_empty() {
                    write!(f, "())")
                } else {
                    let mut res = "(".to_string();
                    for t in v {
                        res.push_str(&format!("{t:?}"));
                        res.push(',');
                        res.push(' ');
                    }
                    res.pop();
                    res.pop();
                    res.push(')');
                    write!(f, "{res}")
                }
            }
            Type::Function(i, o) => {
                let mut res = "".to_string();
                if i.len() == 1 {
                    res.push_str(&format!("{:?}->", i[0]));
                } else {
                    res.push('(');
                    for t in i {
                        res.push_str(&format!("{t:?}"));
                        res.push(',');
                        res.push(' ');
                    }

                    if !i.is_empty() {
                        res.pop();
                        res.pop();
                    }
                    res.push_str(")->");
                }

                res.push_str(&format!("{o:?}"));

                write!(f, "{res}")
            }
            Type::TypeVar(var) => write!(f, "{var}"),
            Type::ForAll(i) => write!(f, "{}", (*i + 97) as char),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Ind;
    use super::Type;

    #[test]
    fn print_types() {
        assert_eq!(format!("{:?}", Type::Unknown), "unknown");
        assert_eq!(format!("{:?}", Type::Void), "void");
        assert_eq!(format!("{:?}", Type::Bool), "bool");
        assert_eq!(format!("{:?}", Type::F64), "f64");
        assert_eq!(format!("{:?}", Type::I64(None)), "i64");
        assert_eq!(
            format!("{:?}", Type::I64(Some(Ind::constant(1).unwrap()))),
            "i64(ind=1)"
        );
        assert_eq!(format!("{:?}", Type::TypeVar("T".to_string())), "T");
        assert_eq!(
            format!(
                "{:?}",
                Type::Function(vec![Type::I64(None)], Box::new(Type::Bool))
            ),
            "i64->bool"
        );
        assert_eq!(format!("{:?}", Type::ForAll(0)), "a");
        assert_eq!(format!("{:?}", Type::Bool), "bool");
        assert_eq!(
            format!(
                "{:?}",
                Type::Tuple(vec![Type::Bool, Type::ForAll(0), Type::ForAll(1)])
            ),
            "forall a, b . (bool, a, b)"
        );

        assert_eq!(
            format!(
                "{:?}",
                Type::Tuple(vec![Type::Bool, Type::ForAll(0), Type::ForAll(1)])
                    .substitute(0, &Type::F64)
            ),
            "forall a . (bool, f64, a)"
        );

        assert_eq!(
            format!(
                "{:?}",
                Type::Tuple(vec![Type::Bool, Type::ForAll(0), Type::ForAll(1)])
                    .substitute(1, &Type::I64(None))
                    .substitute(0, &Type::F64)
            ),
            "(bool, f64, i64)"
        );
    }
}
