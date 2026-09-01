use super::Ty;
use crate::error::{Error, error_at};
use crate::parser::UnaryOp;
use crate::parser::ast::{AD, E, Expr};
use crate::parser::lexer::{ArithOp, Op};
use crate::solver::poly::Poly;

fn scalars(name: &str) -> Option<Ty> {
    match name {
        "f32" => Some(Ty::F32),
        "f64" => Some(Ty::F64),
        "i64" => Some(Ty::I64),
        "bool" => Some(Ty::Bool),
        _ => None,
    }
}

pub fn annotation(ann: &Expr, p_vars: &[&str]) -> Result<Ty, Error> {
    match &ann.data {
        E::Ident(s) => {
            if let Some(t) = scalars(s) {
                Ok(t)
            } else if let Some(p_position) = p_vars.iter().position(|v| v == s) {
                Ok(Ty::Size(Poly::var(p_position as u32, 1)))
            } else {
                Err(error_at!(Type, ann, "unknown type \"{}\"", s))
            }
        }
        E::Int(i) => Ok(Ty::Size(Poly::constant(*i))),
        E::Tuple(inner) => {
            let mut types = vec![];
            for e in inner {
                types.push(annotation(e, p_vars)?);
            }
            Ok(Ty::Tuple(types))
        }
        E::Access(t, dims) => {
            let parsed_t = annotation(t, p_vars)?;
            let mut parsed_dims = vec![];
            for dim in dims {
                match &dim.data {
                    AD::Point(e) => {
                        let p = annotation(e, p_vars)?;
                        if let Ty::Size(poly) = p {
                            parsed_dims.push(poly);
                        } else {
                            return Err(error_at!(Type, e, "dimension must be a poly expression"));
                        }
                    }
                    AD::Range(_, _) => {
                        return Err(error_at!(
                            Type,
                            dim,
                            "ranges not supported in type annotations"
                        ));
                    }
                }
            }
            if let Ty::Array {
                elem: inner_t,
                shape: mut start_dims,
            } = parsed_t
            {
                // array dimensions are cumulative
                // so extend the existing dimensions with the new ones
                // a[10][20] == a[10, 20]
                start_dims.extend(parsed_dims);
                Ok(Ty::new_arr(*inner_t, start_dims))
            } else {
                Ok(Ty::new_arr(parsed_t, parsed_dims))
            }
        }
        E::BinOp(lhs, op, rhs) => {
            let lhs_t = annotation(lhs, p_vars)?;
            let rhs_t = annotation(rhs, p_vars)?;

            let (lhs_poly, rhs_poly) = match (lhs_t, rhs_t) {
                (Ty::Size(l), Ty::Size(r)) => (l, r),
                (Ty::Size(_), _) => {
                    return Err(error_at!(Type, rhs, "operand must be a poly expression"));
                }
                (_, _) => {
                    return Err(error_at!(Type, lhs, "operand must be a poly expression"));
                }
            };

            match op {
                Op::Arith(ArithOp::Add) => Ok(Ty::Size(lhs_poly.add(&rhs_poly))),
                Op::Arith(ArithOp::Sub) => Ok(Ty::Size(lhs_poly.sub(&rhs_poly))),
                Op::Arith(ArithOp::Mul) => Ok(Ty::Size(lhs_poly.mul(&rhs_poly))),
                Op::Arith(ArithOp::Exp) => match rhs_poly.as_constant() {
                    Some(rhs_const) => {
                        if rhs_const.is_zero() {
                            Ok(Ty::Size(Poly::constant(1)))
                        } else if !rhs_const.is_positive() {
                            Err(error_at!(Type, rhs, "power must be non-negative"))
                        } else {
                            Ok(Ty::Size(lhs_poly.pow(rhs_const.get() as u32)))
                        }
                    }
                    None => Err(error_at!(
                        Type,
                        rhs,
                        "power must be a constant poly expression"
                    )),
                },
                _ => Err(error_at!(
                    Type,
                    ann,
                    "invalid binary operation \"{op:?}\" in type annotation"
                )),
            }
        }
        E::FnCall(fun, p_args, args) => match &fun.data {
            E::Ident(ident) => {
                if ident == "Ind" {
                    if !p_args.is_empty() {
                        return Err(error_at!(Type, ann, "cannot pass typevar here"));
                    }

                    if args.len() != 1 {
                        return Err(error_at!(Type, ann, "Ind expects one argument"));
                    }

                    let inner_t = annotation(&args[0], p_vars)?;

                    if let Ty::Size(p) = inner_t {
                        Ok(Ty::Ind(p.clone()))
                    } else {
                        Err(error_at!(Type, ann, "Ind expects size argument"))
                    }
                } else {
                    Err(error_at!(Type, ann, "invalid type annotation"))
                }
            }
            _ => Err(error_at!(Type, ann, "invalid type annotation")),
        },
        E::UnOp(UnaryOp::Option, inner) => {
            let inner_t = annotation(inner, p_vars)?;
            Ok(Ty::Option(Box::new(inner_t)))
        }
        _ => Err(error_at!(Type, ann, "invalid type annotation")),
    }
}

#[cfg(test)]
mod tests {
    use super::annotation;
    use crate::parser::parse_expr;

    #[test]
    fn anns() {
        let ann = annotation(&parse_expr("f32[B, 2*A]").unwrap(), &["A", "B"]);
        assert_eq!(
            format!("{:?}", ann),
            "Ok(Array { elem: F32, shape: [x1, 2*x0] })"
        );

        let ann = annotation(&parse_expr("(i64, f32[3]?)").unwrap(), &[]);
        assert_eq!(
            format!("{:?}", ann),
            "Ok(Tuple([I64, Option(Array { elem: F32, shape: [3] })]))"
        );
    }
}
