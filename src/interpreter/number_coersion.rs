use super::Value;
use crate::lexer::Op;

fn coerce_to_float(lhs: &Value, rhs: &Value) -> Option<(f64, f64)> {
    match lhs {
        Value::F64(lhs) => match rhs {
            Value::F64(rhs) => Some((*lhs, *rhs)),
            Value::I64(rhs) => Some((*lhs, *rhs as f64)),
            Value::Bool(rhs) => Some((*lhs, if *rhs { 1. } else { 0. })),
            _ => unreachable!(),
        },
        Value::I64(lhs) => match rhs {
            Value::F64(rhs) => Some((*lhs as f64, *rhs)),
            Value::I64(_) => None,
            Value::Bool(_) => None,
            _ => unreachable!(),
        },
        Value::Bool(lhs) => match rhs {
            Value::F64(rhs) => Some((if *lhs { 1. } else { 0. }, *rhs)),
            Value::I64(_) => None,
            Value::Bool(_) => None,
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

fn coerce_to_int(lhs: &Value, rhs: &Value) -> (i64, i64) {
    match lhs {
        Value::I64(lhs) => match rhs {
            Value::I64(rhs) => (*lhs, *rhs),
            Value::Bool(rhs) => (*lhs, if *rhs { 1 } else { 0 }),
            _ => unreachable!(),
        },
        Value::Bool(lhs) => match rhs {
            Value::I64(rhs) => (if *lhs { 1 } else { 0 }, *rhs),
            Value::Bool(rhs) => (if *lhs { 1 } else { 0 }, if *rhs { 1 } else { 0 }),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

pub fn arith_coerce<'a>(lhs: Value<'a>, op: &Op, rhs: Value<'a>) -> Value<'a> {
    if let Some((lhs, rhs)) = coerce_to_float(&lhs, &rhs) {
        match &op {
            Op::Add => Value::F64(lhs + rhs),
            Op::Sub => Value::F64(lhs - rhs),
            Op::Mul => Value::F64(lhs * rhs),
            Op::Div => Value::F64(lhs / rhs),
            Op::Exp => Value::F64(lhs.powf(rhs)),
            Op::Greater => Value::Bool(lhs > rhs),
            Op::GreaterOrEq => Value::Bool(lhs >= rhs),
            Op::Less => Value::Bool(lhs < rhs),
            Op::LessOrEq => Value::Bool(lhs <= rhs),
            _ => unreachable!(),
        }
    } else {
        let (lhs, rhs) = coerce_to_int(&lhs, &rhs);
        match &op {
            Op::Add => Value::I64(lhs + rhs),
            Op::Sub => Value::I64(lhs - rhs),
            Op::Mul => Value::I64(lhs * rhs),
            Op::Div => Value::I64(lhs / rhs),
            Op::Exp => Value::F64((lhs as f64).powf(rhs as f64)),
            Op::Greater => Value::Bool(lhs > rhs),
            Op::GreaterOrEq => Value::Bool(lhs >= rhs),
            Op::Less => Value::Bool(lhs < rhs),
            Op::LessOrEq => Value::Bool(lhs <= rhs),
            _ => unreachable!(),
        }
    }
}
