use serde::Serialize;
//use std::collections::HashMap;
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
    ForAll(usize),
}

#[allow(dead_code)]
impl Type {
    fn forall_count(&self) -> usize {
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

    fn substitute(&self, var: usize, a: &Type) -> Type {
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

    // fn unify(&mut self, a: &mut Type, l_sub: &mut HashMap<u8, Type>, r_sub: &mut HashMap<u8, Type>) -> bool {
    //     match a {
    //         Type::Unknown => {
    //             *self = Type::Unknown;
    //             true
    //         },
    //         Type::ForAll(i) => {
    //             r_sub.insert(*i, self.clone());
    //             true
    //         }
    //         _ => match self {
    //             Type::Unknown => true,
    //             Type::Void => if matches!(a, Type::Void) {
    //                 true
    //             } else {
    //                 false
    //             },
    //             Type::Bool => if matches!(a, Type::Bool) {
    //                 true
    //             } else {
    //                 false
    //             },
    //             Type::F64 => if matches!(a, Type::F64) {
    //                 true
    //             } else {
    //                 false
    //             },
    //             Type::TypeVar(l_name) => if let Type::TypeVar(r_name) = a {
    //                 if l_name == r_name {
    //                     Some(Type::TypeVar(l_name.into()))
    //                 } else {
    //                     None
    //                 }
    //             } else {
    //                 None
    //             },
    //             Type::I64(l_nat) => {
    //                 if let Type::I64(r_nat) = a {
    //                     if let Some(r_nat) = r_nat {
    //                         if let Some(l_nat) = l_nat {
    //                             if *l_nat == *r_nat {
    //                                 Some(Type::I64 (Some(l_nat.clone())))
    //                             } else {
    //                                 Some(Type::I64(None))
    //                             }
    //                         } else {
    //                             Some(Type::I64(Some(r_nat.clone())))
    //                         }
    //                     } else {
    //                         if let Some(l_nat) = l_nat {
    //                             Some(Type::I64(Some(l_nat.clone())))
    //                         } else {
    //                             Some(Type::I64(None))
    //                         }
    //                     }
    //                 } else {
    //                     None
    //                 }
    //             },
    //             Type::ForAll(i) => {
    //                 l_sub.insert(*i, a.clone());
    //                 Some(a.clone())
    //             },
    //             Type::Tuple(l_types) => {
    //                 if let Type::Tuple(r_types) = a {
    //                     if l_types.len() != r_types.len() {
    //                         None
    //                     } else {
    //                         for (l_type, r_type) in l_types.iter_mut().zip(r_types) {

    //                         }
    //                     }
    //                 } else {
    //                     None
    //                 }
    //             },
    //             Type::Function(_, _) => todo!(),
    //         }
    //     }
    // }
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

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let forall = self.forall_count();
        if forall > 0 && !matches!(self, Type::ForAll(_)) {
            write!(f, "forall ")?;
            for i in 0..forall {
                write!(f, "{}", usize_name(i))?;
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
            Type::ForAll(i) => write!(f, "{}", usize_name(*i)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::usize_name;
    use super::Ind;
    use super::Type;

    #[test]
    fn usize_name_test() {
        assert_eq!(usize_name(0), "a");
        assert_eq!(usize_name(1), "b");
        assert_eq!(usize_name(25), "z");
        assert_eq!(usize_name(26), "aa");
        assert_eq!(usize_name(27), "ab");
        assert_eq!(usize_name(28), "ac");
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
