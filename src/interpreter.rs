use std::collections::HashSet;

use crate::parser::{Expression, Statement};

use crate::error::{Error, ErrorType};
use crate::lexer::Op;

use crate::stack;

use serde::Serialize;

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum Value {
    Float(f64),
    Bool(bool),
    Tuple(Vec<Value>),
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct InterpreterOutput {
    pub output: String,
    pub error: Option<Error>,
    pub defined_idents: HashSet<String>,
}

fn execute_op(lhs: Value, rhs: Value, op: &Op) -> Result<Value, Error> {
    match &op {
        Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Exp => {
            let coerced_lhs_float = match lhs {
                Value::Float(v) => Some(v),
                Value::Bool(v) => Some(if v { 1. } else { 0. }),
                _ => None,
            }
            .unwrap();

            let coerced_rhs_float = match rhs {
                Value::Float(v) => Some(v),
                Value::Bool(v) => Some(if v { 1. } else { 0. }),
                _ => None,
            }
            .unwrap();

            match &op {
                Op::Add => Ok(Value::Float(coerced_lhs_float + coerced_rhs_float)),
                Op::Sub => Ok(Value::Float(coerced_lhs_float - coerced_rhs_float)),
                Op::Mul => Ok(Value::Float(coerced_lhs_float * coerced_rhs_float)),
                Op::Div => {
                    if coerced_rhs_float == 0. {
                        Err(Error::new(
                            ErrorType::RuntimeError,
                            "division by zero".into(),
                            0,
                            0,
                        ))
                    } else {
                        Ok(Value::Float(coerced_lhs_float / coerced_rhs_float))
                    }
                }

                Op::Exp => Ok(Value::Float(coerced_lhs_float.powf(coerced_rhs_float))),

                _ => panic!(),
            }
        }

        Op::And | Op::Or => {
            let coerced_lhs_bool = match lhs {
                Value::Float(v) => Some(v == 0.),
                Value::Bool(v) => Some(v),
                _ => None,
            }
            .unwrap();

            let coerced_rhs_bool = match rhs {
                Value::Float(v) => Some(v == 0.),
                Value::Bool(v) => Some(v),
                _ => None,
            }
            .unwrap();

            match &op {
                Op::And => Ok(Value::Bool(coerced_lhs_bool && coerced_rhs_bool)),
                Op::Or => Ok(Value::Bool(coerced_lhs_bool || coerced_rhs_bool)),
                _ => panic!(),
            }
        }

        Op::Comma => Err(Error::new(
            ErrorType::InternalError,
            "comma should have created tuple".into(),
            0,
            0,
        )),

        Op::Dot => Err(Error::new(
            ErrorType::NotImplementedError,
            "have not implemented dot operator".into(),
            0,
            0,
        )),
    }
}

pub fn interpret_expression(
    expression: &Expression,
    frame: &stack::Frame<Value>,
) -> Result<Value, Error> {
    match expression {
        Expression::Float { val, .. } => Ok(Value::Float(*val)),
        Expression::Bool { val, .. } => Ok(Value::Bool(*val)),
        Expression::BinOp {
            start,
            lhs,
            op,
            rhs,
            end,
        } => {
            let (lhs, rhs) = (
                interpret_expression(lhs, frame)?,
                interpret_expression(rhs, frame)?,
            );
            match execute_op(lhs, rhs, op) {
                Ok(v) => Ok(v),
                Err(mut error) => {
                    error.start = *start;
                    error.end = *end;
                    Err(error)
                }
            }
        }

        Expression::Identifier { start, name, end } => match frame.get(&name.clone()) {
            Some(val) => Ok(val.to_owned()),
            None => Err(Error::new(
                ErrorType::UnboundIdentifierError,
                format!("identifier '{name}' is not bound"),
                *start,
                *end,
            )),
        },

        Expression::Tuple { inner, .. } => {
            let mut members = vec![];
            for expression in inner {
                members.push(interpret_expression(expression, frame)?);
            }
            Ok(Value::Tuple(members))
        }

        Expression::OpenTuple { start, end, .. } => Err(Error::new(
            ErrorType::InternalError,
            "open tuple".into(),
            *start,
            *end,
        )),
    }
}

pub fn print_value(value: Value) -> String {
    match value {
        Value::Float(val) => val.to_string(),
        Value::Bool(val) => val.to_string(),
        Value::Tuple(v) => {
            if v.is_empty() {
                "()".into()
            } else {
                let mut out = "(".to_string();
                for val in v {
                    out.push_str(&print_value(val));
                    out.push(',');
                    out.push(' ');
                }
                out.pop();
                out.pop();
                out.push(')');
                out
            }
        }
    }
}

pub fn interpret(ast: &[Statement], frame: Option<&mut stack::Frame<Value>>) -> InterpreterOutput {
    let mut empty_frame = stack::Frame::<Value>::new();
    let frame = match frame {
        Some(frame) => frame,
        None => &mut empty_frame,
    };

    frame.push_scope();

    let mut output = String::new();
    let mut defined_idents: HashSet<String> = HashSet::new();

    for statement in ast {
        match statement {
            Statement::Let { name, val, .. } => match interpret_expression(val, frame) {
                Ok(value) => {
                    frame.insert(name.clone(), value);
                    defined_idents.insert(name.clone());
                }
                Err(error) => {
                    return InterpreterOutput {
                        output,
                        error: Some(error),
                        defined_idents,
                    }
                }
            },
            Statement::Print { val } => match interpret_expression(val, frame) {
                Ok(value) => {
                    output.push_str(&print_value(value));
                    output.push('\n');
                }
                Err(error) => {
                    return InterpreterOutput {
                        output,
                        error: Some(error),
                        defined_idents,
                    }
                }
            },
            Statement::If { cond, inner } => match interpret_expression(cond, frame) {
                Ok(value) => {
                    if Value::Bool(true) == value {
                        let inner_output = interpret(inner, Some(frame));

                        output.push_str(&inner_output.output);

                        if inner_output.error.is_some() {
                            return InterpreterOutput {
                                output,
                                error: inner_output.error,
                                defined_idents,
                            };
                        }

                        frame.pop_scope();
                    }
                }
                Err(error) => {
                    return InterpreterOutput {
                        output,
                        error: Some(error),
                        defined_idents,
                    }
                }
            },
        }
    }

    InterpreterOutput {
        output,
        error: None,
        defined_idents,
    }
}
