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

    if lhs != Type::F64 && lhs != Type::I64 && lhs != Type::Bool {
        Err(Error::new(
            ErrorType::TypeError,
            format!("cannot apply {op_type} operator to '{lhs:?}'"),
            start,
            end,
        ))
    } else if rhs != Type::F64 && rhs != Type::I64 && rhs != Type::Bool {
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
            _ => Ok(Type::I64),
        }
    }
}
