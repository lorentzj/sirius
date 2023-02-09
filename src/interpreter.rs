use crate::parser::{Expression, ParserOutput, Statement};

use crate::error::{Error, ErrorType};
use crate::lexer::Op;

use serde::Serialize;

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum Value {
    Number(f64),
    Tuple(Vec<Value>),
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct InterpreterOutput {
    output: String,
    error: Option<Error>,
}

pub type Context = std::collections::HashMap<String, Value>;

fn execute_op(lhs: Value, rhs: Value, op: &Op) -> Result<Value, Error> {
    if let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) {
        match &op {
            Op::Add => Ok(Value::Number(lhs + rhs)),
            Op::Sub => Ok(Value::Number(lhs - rhs)),
            Op::Mul => Ok(Value::Number(lhs * rhs)),
            Op::Div => {
                if rhs == 0. {
                    Err(Error::new(
                        ErrorType::RuntimeError,
                        "division by zero".into(),
                        0,
                        0,
                    ))
                } else {
                    Ok(Value::Number(lhs / rhs))
                }
            }
            Op::Exp => Ok(Value::Number(lhs.powf(rhs))),

            Op::Dot => Err(Error::new(
                ErrorType::NotImplementedError,
                "haven't implemented dot operator yet...".into(),
                0,
                0,
            )),

            Op::Comma => Err(Error::new(
                ErrorType::InternalError,
                "comma should have created tuple".into(),
                0,
                0,
            )),
        }
    } else {
        Err(Error::new(
            ErrorType::NotImplementedError,
            "cannot apply operator to tuple".into(),
            0,
            0,
        ))
    }
}

pub fn interpret_expression(expression: &Expression, context: &Context) -> Result<Value, Error> {
    match expression {
        Expression::Constant { val, .. } => Ok(Value::Number(*val)),
        Expression::BinOp {
            start,
            lhs,
            op,
            rhs,
            end,
        } => {
            let (lhs, rhs) = (
                interpret_expression(lhs, context)?,
                interpret_expression(rhs, context)?,
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

        Expression::Identifier { start, name, end } => match context.get(&name.clone()) {
            Some(val) => Ok(val.to_owned()),
            None => Err(Error::new(
                ErrorType::UnboundIdentifierError,
                format!("identifier '{}' is not bound", name),
                *start,
                *end,
            )),
        },

        Expression::Tuple { inner, .. } => {
            let mut members = vec![];
            for expression in inner {
                members.push(interpret_expression(expression, context)?);
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
        Value::Number(x) => x.to_string(),
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

pub fn interpret(parser_output: &ParserOutput) -> InterpreterOutput {
    let mut output = String::new();
    let mut context = Context::new();

    if !parser_output.errors.is_empty() {
        return InterpreterOutput {
            output: "".into(),
            error: None,
        };
    }

    for statement in &parser_output.ast {
        match statement {
            Statement::Let { name, val, .. } => match interpret_expression(val, &context) {
                Ok(value) => {
                    context.insert(name.clone(), value);
                }
                Err(error) => {
                    return InterpreterOutput {
                        output,
                        error: Some(error),
                    }
                }
            },
            Statement::Print(val) => match interpret_expression(val, &context) {
                Ok(value) => {
                    output.push_str(&print_value(value));
                    output.push('\n');
                }
                Err(error) => {
                    return InterpreterOutput {
                        output,
                        error: Some(error),
                    }
                }
            },
        }
    }

    InterpreterOutput {
        output,
        error: None,
    }
}
