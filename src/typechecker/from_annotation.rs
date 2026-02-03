use crate::error::Error;
use super::Type;
use crate::parser::ast::{AD, E, Expr};
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
            } else if p_vars.contains(s) {
                Ok(Type::I64(Some(Poly::var(
                    p_vars.iter().position(|v| v == s).unwrap(),
                    1,
                ))))
            } else {
                Err(Error::type_from_expr(ann, &format!("unknown type \"{}\"", s)))
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
                            return Err(Error::type_from_expr(e, "dimension must be a poly expression"));
                        }
                    }
                    AD::Range(_, _) => {
                        return Err(Error::type_from_expr(dim, "ranges not supported in type annotations"));
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
                    return Err(Error::type_from_expr(rhs, "operand must be a poly expression"));
                }
                (_, _) => {
                    return Err(Error::type_from_expr(lhs, "operand must be a poly expression"));
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
                                return Err(Error::type_from_expr(rhs, "power must be an integer"));
                            }
                        };

                        if rhs_const == 0 {
                            Ok(Type::I64(Some(Poly::constant_int(1))))
                        } else if rhs_const < 0 {
                            Err(Error::type_from_expr(rhs, "power must be non-negative"))
                        } else if rhs_const > 16 {
                            Err(Error::type_from_expr(rhs, "power must be less than or equal to 16"))
                        } else {
                            let mut result = Poly::constant_int(1);
                            for _ in 0..rhs_const {
                                result = result.mul_ref(&lhs_poly);
                            }
                            Ok(Type::I64(Some(result)))
                        }
                    }
                    None => Err(Error::type_from_expr(rhs, "power must be a constant poly expression")),
                },
                _ => Err(Error::type_from_expr(ann, &format!("invalid binary operation \"{op:?}\" in type annotation"))),
            }
        }
        _ => Err(Error::type_from_expr(ann, "invalid type annotation")),
    }
}
