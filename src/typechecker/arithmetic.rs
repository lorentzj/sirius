use super::types::Type;
use crate::error::{Error, ErrorType};
use crate::lexer::Op;

pub fn is_arith(op: &Op) -> bool {
    matches!(
        op,
        &Op::Add
            | &Op::Sub
            | &Op::Mul
            | &Op::Div
            | &Op::Exp
            | &Op::Greater
            | &Op::Less
            | &Op::GreaterOrEq
            | &Op::LessOrEq
    )
}

pub fn arith_coerce(
    start: usize,
    lhs: &Type,
    op: &Op,
    rhs: &Type,
    end: usize,
) -> Result<Type, Error> {
    if lhs == &Type::Unknown || rhs == &Type::Unknown {
        return Ok(Type::Unknown);
    }

    let op_type = if op == &Op::Greater || op == &Op::Less {
        "comparison"
    } else {
        "arithmetic"
    };

    if op_type.eq("comparison") && (lhs == &Type::Bool || rhs == &Type::Bool) {
        Err(Error::new(
            ErrorType::Type,
            format!("cannot apply comparison operator to \"{:?}\"", Type::Bool),
            start,
            end,
        ))
    } else if lhs != &Type::F64 && !matches!(lhs, Type::I64(_)) && lhs != &Type::Bool {
        Err(Error::new(
            ErrorType::Type,
            format!("cannot apply {op_type} operator to \"{lhs:?}\""),
            start,
            end,
        ))
    } else if rhs != &Type::F64 && !matches!(rhs, Type::I64(_)) && rhs != &Type::Bool {
        Err(Error::new(
            ErrorType::Type,
            format!("cannot apply {op_type} operator to \"{rhs:?}\""),
            start,
            end,
        ))
    } else if lhs == &Type::F64 || rhs == &Type::F64 {
        match op {
            Op::Greater | Op::Less | Op::GreaterOrEq | Op::LessOrEq => Ok(Type::Bool),
            _ => Ok(Type::F64),
        }
    } else {
        match op {
            Op::Greater | Op::Less | Op::GreaterOrEq | Op::LessOrEq => Ok(Type::Bool),
            Op::Exp => Ok(Type::F64),
            _ => {
                if let Type::I64(Some(lval)) = lhs {
                    if let Type::I64(Some(rval)) = rhs {
                        Ok(Type::I64(Some(match op {
                            Op::Add => lval.clone() + rval.clone(),
                            Op::Sub => lval.clone() - rval.clone(),
                            Op::Mul => lval.clone() * rval.clone(),
                            Op::Div => {
                                let (qs, rem) = lval.clone().compound_divide(vec![rval.clone()]);

                                if let Some(0) = rem.get_constant_val() {
                                    qs[0].clone()
                                } else {
                                    return Ok(Type::I64(None));
                                }
                            }
                            _ => panic!(),
                        })))
                    } else {
                        Ok(Type::I64(None))
                    }
                } else {
                    Ok(Type::I64(None))
                }
            }
        }
    }
}
