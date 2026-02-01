use crate::solver::Poly;

#[derive(PartialEq, Clone)]
pub struct FunctionType {
    pub p_args: Vec<String>,
    pub args: Vec<Type>,
    pub ret: Type,
}

#[derive(PartialEq, Clone)]
pub enum Type {
    Unknown,
    Void,
    F64,
    I64(Option<Poly>),
    Bool,
    Tuple(Vec<Type>),
    Function(Box<FunctionType>),
    Array(Box<Type>, Vec<Poly>),
    ForAll(usize),
}

impl Type {
    pub fn forall_vars(&self, seen: &mut Vec<usize>) {
        match self {
            Type::Tuple(v) => {
                for t in v {
                    t.forall_vars(seen)
                }
            }
            Type::Function(f) => {
                for t in &f.args {
                    t.forall_vars(seen)
                }

                f.ret.forall_vars(seen);
            }
            Type::ForAll(i) => {
                if !seen.contains(i) {
                    seen.push(*i);
                }
            }
            _ => (),
        }
    }

    pub fn fmt(&self, p_vars: &[String]) -> String {
        let mut res = String::new();
        let mut forall_vars = vec![];
        self.forall_vars(&mut forall_vars);
        if !forall_vars.is_empty() {
            res.push_str("forall ");
            for (i, var) in forall_vars.iter().enumerate() {
                res.push_str(&usize_name(*var));
                if i < forall_vars.len() - 1 {
                    res.push_str(", ");
                }
            }
            res.push_str(" . ");
        }

        res.push_str(&priv_print(self, p_vars));
        res
    }

    pub fn fmt_novars(&self) -> String {
        self.fmt(&[])
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

fn priv_print(t: &Type, p_vars: &[String]) -> String {
    match t {
        Type::Unknown => "unknown".into(),
        Type::Void => "void".into(),
        Type::F64 => "f64".into(),
        Type::I64(ind) => match ind {
            Some(ind) => format!("i64(p={})", ind.format(p_vars)),
            None => "i64".into(),
        },
        Type::Bool => "bool".into(),
        Type::Tuple(v) => {
            if v.is_empty() {
                "()".into()
            } else {
                let mut res = "(".to_string();
                for t in v {
                    res.push_str(&priv_print(t, p_vars));
                    res.push(',');
                    res.push(' ');
                }
                res.pop();
                res.pop();
                res.push(')');
                res
            }
        }
        Type::Function(f) => {
            let mut res = "".to_string();
            if f.args.len() == 1 {
                res.push_str(&format!("{}->", &priv_print(&f.args[0], p_vars)));
            } else {
                res.push('(');
                for t in &f.args {
                    res.push_str(&priv_print(t, p_vars));
                    res.push(',');
                    res.push(' ');
                }

                if !f.args.is_empty() {
                    res.pop();
                    res.pop();
                }
                res.push_str(")->");
            }

            res.push_str(&priv_print(&f.ret, p_vars));

            res
        }
        Type::Array(t, dims) => {
            let mut res = "".to_string();
            res.push_str(&priv_print(t, p_vars));
            res.push('[');
            for dim in dims {
                res.push_str(&format!("{dim:?}"));
                res.push(',');
                res.push(' ');
            }
            res.pop();
            res.pop();
            res.push(']');

            res
        }
        Type::ForAll(i) => format!("'{}", usize_name(*i)),
    }
}

#[cfg(test)]
mod tests {
    use super::{FunctionType, Poly, Type, usize_name};
    use crate::solver::Rat;

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
        assert_eq!(Type::Unknown.fmt_novars(), "unknown");
        assert_eq!(Type::Void.fmt_novars(), "void");
        assert_eq!(Type::Bool.fmt_novars(), "bool");
        assert_eq!(Type::F64.fmt_novars(), "f64");
        assert_eq!(Type::I64(None).fmt_novars(), "i64");
        assert_eq!(
            Type::I64(Some(Poly::constant(Rat::from(1)))).fmt_novars(),
            "i64(p=1)"
        );
        assert_eq!(
            Type::Function(Box::new(FunctionType {
                p_args: vec![],
                args: vec![Type::I64(None)],
                ret: Type::Bool,
            }))
            .fmt_novars(),
            "i64->bool"
        );

        assert_eq!(
            Type::Function(Box::new(FunctionType {
                p_args: vec![],
                args: vec![
                    Type::Tuple(vec![Type::ForAll(0), Type::ForAll(0), Type::ForAll(0)]),
                    Type::Function(Box::new(FunctionType {
                        p_args: vec![],
                        args: vec![Type::ForAll(0)],
                        ret: Type::ForAll(1),
                    }))
                ],
                ret: Type::Tuple(vec![Type::ForAll(1), Type::ForAll(1), Type::ForAll(1)]),
            }))
            .fmt_novars(),
            "forall a, b . (('a, 'a, 'a), 'a->'b)->('b, 'b, 'b)"
        )
    }
}
