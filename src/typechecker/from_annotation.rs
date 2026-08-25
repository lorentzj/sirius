use super::{Type, types::T};
use crate::error::{Error, Errors};
use crate::parser::Pos;
use crate::parser::ast::{AD, E, Expr, Function};
use crate::parser::lexer::Op;
use crate::solver::poly::Poly;

fn standard_type(name: &str) -> Option<T> {
    match name {
        "f32" => Some(T::F32),
        "i32" => Some(T::I32),
        "bool" => Some(T::Bool),
        "void" => Some(T::Void),
        _ => None,
    }
}

pub fn annotation(ann: &Expr, p_vars: &Vec<String>) -> Result<Type, Error> {
    match &ann.data {
        E::Ident(s) => {
            if let Some(t) = standard_type(s) {
                Ok(Type::new_at(t, ann))
            } else if let Some(p_position) = p_vars.iter().position(|v| v == s) {
                Ok(Type::poly(Poly::var(p_position as u64, 1), ann))
            } else {
                Err(ann.type_error(&format!("unknown type \"{}\"", s)))
            }
        }
        E::Int(i) => Ok(Type::poly(Poly::constant(*i as i128), ann)),
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
                            data: T::Poly(poly),
                            ..
                        } = p
                        {
                            parsed_dims.push(poly);
                        } else {
                            return Err(e.type_error("dimension must be a poly expression"));
                        }
                    }
                    AD::Range(_, _) => {
                        return Err(dim.type_error("ranges not supported in type annotations"));
                    }
                }
            }
            if let Type {
                data: T::Array(inner_t, mut start_dims),
                ..
            } = parsed_t
            {
                // array dimensions are cumulative
                // so extend the existing dimensions with the new ones
                // e.g. a[10][20] == a[10, 20]
                start_dims.extend(parsed_dims);
                Ok(Type::new_at(T::Array(Box::new(*inner_t), start_dims), ann))
            } else {
                Ok(Type::new_at(T::Array(Box::new(parsed_t), parsed_dims), ann))
            }
        }
        E::BinOp(lhs, op, rhs) => {
            let lhs_t = annotation(lhs, p_vars)?;
            let rhs_t = annotation(rhs, p_vars)?;

            let (lhs_poly, rhs_poly) = match (lhs_t.data, rhs_t.data) {
                (T::Poly(l), T::Poly(r)) => (l, r),
                (T::Poly(_), _) => {
                    return Err(rhs.type_error("operand must be a poly expression"));
                }
                (_, _) => {
                    return Err(lhs.type_error("operand must be a poly expression"));
                }
            };

            match op {
                Op::Add => Ok(Type::poly(lhs_poly.add(&rhs_poly), ann)),
                Op::Sub => Ok(Type::poly(lhs_poly.sub(&rhs_poly), ann)),
                Op::Mul => Ok(Type::poly(lhs_poly.mul(&rhs_poly), ann)),
                Op::Exp => match rhs_poly.get_constant() {
                    Some(mut rhs_const) => {
                        if rhs_const == 0 {
                            Ok(Type::poly(Poly::constant(1), ann))
                        } else if rhs_const < 0 {
                            Err(rhs.type_error("power must be non-negative"))
                        } else if rhs_const > 16 {
                            Err(rhs.type_error("power must be less than or equal to 16"))
                        } else {
                            let mut acc = Poly::constant(1);
                            let mut base = (*lhs_poly).clone();
                            while rhs_const > 0 {
                                if rhs_const & 1 == 1 {
                                    acc = acc.mul(&base);
                                }
                                rhs_const >>= 1;
                                if rhs_const > 0 {
                                    base = base.mul(&base);
                                }
                            }

                            Ok(Type::poly(acc, ann))
                        }
                    }
                    None => Err(rhs.type_error("power must be a constant poly expression")),
                },
                _ => Err(ann.type_error(&format!(
                    "invalid binary operation \"{op:?}\" in type annotation"
                ))),
            }
        }
        _ => Err(ann.type_error("invalid type annotation")),
    }
}

pub fn fun_type(fun: &Function) -> Result<Type, Errors> {
    let p_vars = Pos::inner_collect(&fun.type_args);

    let mut args = vec![];
    let mut p_constraints = vec![];
    let mut errors = vec![];

    for (name, ann) in fun.type_constraints.iter() {
        match annotation(ann, &p_vars) {
            Ok(Type {
                data: T::Poly(p),
                end,
                ..
            }) => {
                p_constraints.push(Pos::new(name.start, (name.data.clone(), p), end));
            }
            Ok(_) => {
                errors.push(ann.type_error("constraint must be a poly expression"));
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

    Ok(Type::new_fn(
        fun.type_args.clone(),
        p_constraints,
        args,
        ret_t,
        &fun.name,
    ))
}
