use super::{Type, types::T};
use crate::error::{Error, Errors, error_at};
use crate::parser::ast::{AD, E, Expr, Function};
use crate::parser::lexer::Op;
use crate::parser::{Pos, UnaryOp};
use crate::solver::poly::Poly;

fn scalars(name: &str) -> Option<T> {
    match name {
        "f32" => Some(T::F32),
        "i32" => Some(T::I32),
        "bool" => Some(T::Bool),
        "null" => Some(T::Null),
        "void" => Some(T::Void),
        _ => None,
    }
}

pub fn annotation(ann: &Expr, p_vars: &[String]) -> Result<Type, Error> {
    match &ann.data {
        E::Ident(s) => {
            if let Some(t) = scalars(s) {
                Ok(Type::new_at(t, ann))
            } else if let Some(p_position) = p_vars.iter().position(|v| v == s) {
                Ok(Type::size(Poly::var(p_position as u32, 1), ann))
            } else {
                Err(error_at!(ann, Type, format!("unknown type \"{}\"", s)))
            }
        }
        E::Int(i) => Ok(Type::size(Poly::constant(*i as i128), ann)),
        E::Tuple(inner) => {
            let mut types = vec![];
            for e in inner {
                types.push(annotation(e, p_vars)?);
            }
            Ok(Type::new_at(T::Tuple(types), ann))
        }
        E::Access(t, dims) => {
            let parsed_t = annotation(t, p_vars)?;
            let mut parsed_dims = vec![];
            for dim in dims {
                match &dim.data {
                    AD::Point(e) => {
                        let p = annotation(e, p_vars)?;
                        if let Type {
                            data: T::Size(poly),
                            ..
                        } = p
                        {
                            parsed_dims.push(poly);
                        } else {
                            return Err(error_at!(e, Type, "dimension must be a poly expression"));
                        }
                    }
                    AD::Range(_, _) => {
                        return Err(error_at!(
                            dim,
                            Type,
                            "ranges not supported in type annotations"
                        ));
                    }
                }
            }
            if let Type {
                data:
                    T::Array {
                        elem: inner_t,
                        shape: mut start_dims,
                    },
                ..
            } = parsed_t
            {
                // array dimensions are cumulative
                // so extend the existing dimensions with the new ones
                // e.g. a[10][20] == a[10, 20]
                start_dims.extend(parsed_dims);
                Ok(Type::new_at(
                    T::Array {
                        elem: Box::new(*inner_t),
                        shape: start_dims,
                    },
                    ann,
                ))
            } else {
                Ok(Type::new_at(
                    T::Array {
                        elem: Box::new(parsed_t),
                        shape: parsed_dims,
                    },
                    ann,
                ))
            }
        }
        E::BinOp(lhs, op, rhs) => {
            let lhs_t = annotation(lhs, p_vars)?;
            let rhs_t = annotation(rhs, p_vars)?;

            let (lhs_poly, rhs_poly) = match (lhs_t.data, rhs_t.data) {
                (T::Size(l), T::Size(r)) => (l, r),
                (T::Size(_), _) => {
                    return Err(error_at!(rhs, Type, "operand must be a poly expression"));
                }
                (_, _) => {
                    return Err(error_at!(lhs, Type, "operand must be a poly expression"));
                }
            };

            match op {
                Op::Add => Ok(Type::size(lhs_poly.add(&rhs_poly), ann)),
                Op::Sub => Ok(Type::size(lhs_poly.sub(&rhs_poly), ann)),
                Op::Mul => Ok(Type::size(lhs_poly.mul(&rhs_poly), ann)),
                Op::Exp => match rhs_poly.as_constant() {
                    Some(rhs_const) => {
                        if rhs_const.is_zero() {
                            Ok(Type::size(Poly::constant(1), ann))
                        } else if !rhs_const.is_positive() {
                            Err(error_at!(rhs, Type, "power must be non-negative"))
                        } else {
                            Ok(Type::size(lhs_poly.pow(rhs_const.get() as u32), ann))
                        }
                    }
                    None => Err(error_at!(
                        rhs,
                        Type,
                        "power must be a constant poly expression"
                    )),
                },
                _ => Err(error_at!(
                    ann,
                    Type,
                    format!("invalid binary operation \"{op:?}\" in type annotation")
                )),
            }
        }
        E::FnCall(fun, p_args, args) => match &fun.data {
            E::Ident(ident) => {
                if ident == "Ind" {
                    if !p_args.is_empty() {
                        return Err(error_at!(ann, Type, "cannot pass typevar here"));
                    }

                    if args.len() != 1 {
                        return Err(error_at!(ann, Type, "Ind expects one argument"));
                    }

                    let inner_t = annotation(&args[0], p_vars)?;

                    if let T::Size(p) = inner_t.data {
                        Ok(Type::ind(p.clone(), ann))
                    } else {
                        Err(error_at!(ann, Type, "Ind expects size argument"))
                    }
                } else {
                    Err(error_at!(ann, Type, "invalid type annotation"))
                }
            }
            _ => Err(error_at!(ann, Type, "invalid type annotation")),
        },
        E::UnOp(UnaryOp::Option, inner) => {
            let inner_t = annotation(inner, p_vars)?;
            Ok(Type::new_at(T::Option(Box::new(inner_t)), ann))
        }
        _ => Err(error_at!(ann, Type, "invalid type annotation")),
    }
}

pub fn fun_type(fun: &Function) -> Result<Type, Errors> {
    let p_vars: Vec<String> = Pos::inner_collect(&fun.type_args);

    let mut args = vec![];
    let mut p_constraints = vec![];
    let mut errors = vec![];

    for (name, ann) in fun.type_constraints.iter() {
        match annotation(ann, &p_vars) {
            Ok(Type {
                data: T::Size(p),
                end,
                ..
            }) => {
                p_constraints.push(Pos::new(name.start, (name.data.clone(), p), end));
            }
            Ok(_) => {
                errors.push(error_at!(ann, Type, "constraint must be a poly expression"));
            }
            Err(e) => {
                errors.push(e);
            }
        }
    }

    for (_, ann) in fun.args.iter() {
        match annotation(ann, &p_vars) {
            Ok(t) => {
                args.push(t);
            }
            Err(e) => {
                errors.push(e);
                args.push(Type::new_at(T::Error, ann));
            }
        }
    }

    let ret_t = match &fun.ret {
        Some(ret) => match annotation(ret, &p_vars) {
            Ok(t) => t,
            Err(e) => {
                errors.push(e);
                Type::new_at(T::Void, ret)
            }
        },
        None => Type::new_at(T::Void, &fun.name),
    };

    if errors.is_empty() {
        Ok(Type::new_fn(fun.type_args.clone(), args, ret_t, &fun.name))
    } else {
        Err(errors)
    }
}

#[cfg(test)]
mod tests {
    use super::annotation;
    use crate::parser::parse_expr;

    #[test]
    fn anns() {
        let ann = annotation(
            &parse_expr("f32[B, 2*A]").unwrap(),
            &["A".into(), "B".into()],
        );
        assert_eq!(
            format!("{:?}", ann),
            "Ok(Array { elem: F32 @ (0, 1), shape: [x1, 2*x0] } @ (0, 8))"
        );

        let ann = annotation(&parse_expr("(i32, f32[3]?)").unwrap(), &[]);
        assert_eq!(
            format!("{:?}", ann),
            "Ok(Tuple([I32 @ (1, 2), Option(Array { elem: F32 @ (3, 4), shape: [3] } @ (3, 7)) @ (3, 8)]) @ (0, 9))"
        );
    }
}
