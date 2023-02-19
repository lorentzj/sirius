use super::Type;
use crate::error::{Error, ErrorType};
use crate::lexer::Op;

pub fn arith_coerce(
    start: usize,
    lhs: Type,
    op: &Op,
    rhs: Type,
    end: usize,
) -> Result<Type, Error> {
    let op_type = if op == &Op::Greater || op == &Op::Less {
        "comparison"
    } else {
        "arithmetic"
    };

    if op_type.eq("comparison") && (lhs == Type::Bool || rhs == Type::Bool) {
        Err(Error::new(
            ErrorType::TypeError,
            format!("cannot apply comparison operator to '{:?}'", Type::Bool),
            start,
            end,
        ))
    } else if lhs != Type::F64 && !lhs.is_int() && lhs != Type::Bool {
        Err(Error::new(
            ErrorType::TypeError,
            format!("cannot apply {op_type} operator to '{lhs:?}'"),
            start,
            end,
        ))
    } else if rhs != Type::F64 && !rhs.is_int() && rhs != Type::Bool {
        Err(Error::new(
            ErrorType::TypeError,
            format!("cannot apply {op_type} operator to '{rhs:?}'"),
            start,
            end,
        ))
    } else if lhs == Type::F64 || rhs == Type::F64 {
        match op {
            Op::Greater | Op::Less => Ok(Type::Bool),
            _ => Ok(Type::F64),
        }
    } else {
        match op {
            Op::Greater | Op::Less => Ok(Type::Bool),
            Op::Exp => Ok(Type::F64),
            _ => {
                if let Type::I64 { nat: Some(lnat) } = lhs {
                    if let Type::I64 { nat: Some(rnat) } = rhs {
                        let res = match op {
                            Op::Add => (lnat as i64) + (rnat as i64),
                            Op::Sub => (lnat as i64) - (rnat as i64),
                            Op::Mul => (lnat as i64) * (rnat as i64),
                            Op::Div => (lnat as i64) / (rnat as i64),
                            _ => panic!(),
                        };

                        if res >= 0 && res <= usize::MAX as i64 {
                            Ok(Type::I64 {
                                nat: Some(res as usize),
                            })
                        } else {
                            Ok(Type::I64 { nat: None })
                        }
                    } else {
                        Ok(Type::I64 { nat: None })
                    }
                } else {
                    Ok(Type::I64 { nat: None })
                }
            }
        }
    }
}
