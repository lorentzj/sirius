use super::Type;
use crate::error::{Error, Errors};
use crate::parser::Pos;
use crate::parser::ast::{AD, E, Expr, Function};
use crate::parser::lexer::Op;
use crate::solver::Poly;

fn standard_type(name: &str) -> Option<Type> {
    match name {
        "f64" => Some(Type::F64),
        "i64" => Some(Type::I64(None)),
        "bool" => Some(Type::Bool),
        "void" => Some(Type::Void),
        _ => None,
    }
}

pub fn annotation(ann: &Expr, p_vars: &Vec<String>) -> Result<Type, Error> {
    match &ann.data {
        E::Ident(s) => {
            if let Some(t) = standard_type(s) {
                Ok(t)
            } else if let Some(p_position) = p_vars.iter().position(|v| v == s) {
                Ok(Type::I64(Some(Poly::var(p_position, 1))))
            } else {
                Err(ann.type_error(&format!("unknown type \"{}\"", s)))
            }
        }
        E::Int(i) => Ok(Type::I64(Some(Poly::constant_int(*i)))),
        E::Tuple(inner) => {
            let mut types = vec![];
            for e in inner {
                types.push(annotation(e, p_vars)?);
            }
            Ok(Type::Tuple(types))
        }
        E::Access(t, dims) => {
            let parsed_t = annotation(t, p_vars)?;
            let mut parsed_dims = vec![];
            for dim in dims {
                match &dim.data {
                    AD::Point(e) => {
                        let p = annotation(e, p_vars)?;
                        if let Type::I64(Some(poly)) = p {
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
            if let Type::Array(inner_t, mut start_dims) = parsed_t {
                // array dimensions are cumulative
                // so extend the existing dimensions with the new ones
                // e.g. a[10][20] == a[10, 20]
                start_dims.extend(parsed_dims);
                Ok(Type::Array(Box::new(*inner_t), start_dims))
            } else {
                Ok(Type::Array(Box::new(parsed_t), parsed_dims))
            }
        }
        E::BinOp(lhs, op, rhs) => {
            let lhs_t = annotation(lhs, p_vars)?;
            let rhs_t = annotation(rhs, p_vars)?;

            let (lhs_poly, rhs_poly) = match (lhs_t, rhs_t) {
                (Type::I64(Some(l)), Type::I64(Some(r))) => (l, r),
                (Type::I64(Some(_)), _) => {
                    return Err(rhs.type_error("operand must be a poly expression"));
                }
                (_, _) => {
                    return Err(lhs.type_error("operand must be a poly expression"));
                }
            };

            match op {
                Op::Add => Ok(Type::I64(Some(lhs_poly + rhs_poly))),
                Op::Sub => Ok(Type::I64(Some(lhs_poly - rhs_poly))),
                Op::Mul => Ok(Type::I64(Some(lhs_poly * rhs_poly))),
                Op::Exp => match rhs_poly.get_constant_val() {
                    Some(rhs_const) => {
                        let rhs_const: i64 = match rhs_const.0.to_integer().try_into() {
                            Ok(i) => i,
                            Err(_) => {
                                return Err(rhs.type_error("power must be an integer"));
                            }
                        };

                        if rhs_const == 0 {
                            Ok(Type::I64(Some(Poly::constant_int(1))))
                        } else if rhs_const < 0 {
                            Err(rhs.type_error("power must be non-negative"))
                        } else if rhs_const > 16 {
                            Err(rhs.type_error("power must be less than or equal to 16"))
                        } else {
                            let mut result = Poly::constant_int(1);
                            for _ in 0..rhs_const {
                                result = result.mul_ref(&lhs_poly);
                            }
                            Ok(Type::I64(Some(result)))
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
    let mut errors = vec![];
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
                Type::Void
            }
        },
        None => Type::Void,
    };

    Ok(Type::new_fn(p_vars, args, ret_t))
}
