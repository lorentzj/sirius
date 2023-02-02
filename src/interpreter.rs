use crate::parser::{Expression, ExpressionData, Statement, TokenID};

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

fn execute_op(lhs: Value, rhs: Value, op: Op) -> Result<Value, Error> {
    if let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) {
        match op {
            Op::Plus => Ok(Value::Number(lhs + rhs)),
            Op::Minus => Ok(Value::Number(lhs - rhs)),
            Op::Mul => Ok(Value::Number(lhs * rhs)),
            Op::Div => {
                if rhs == 0. {
                    Err(Error {
                        error_type: ErrorType::RuntimeError,
                        message: "division by zero".to_string(),
                        tokens: vec![],
                    })
                } else {
                    Ok(Value::Number(lhs / rhs))
                }
            }
            Op::Exp => Ok(Value::Number(lhs.powf(rhs))),

            Op::Dot => Err(Error::new(
                ErrorType::NotImplementedError,
                "haven't implemented dot operator yet...".to_string(),
                vec![],
            )),

            Op::Comma => Err(Error {
                error_type: ErrorType::InternalError,
                message: "comma should have created tuple".to_string(),
                tokens: vec![],
            }),
        }
    } else {
        Err(Error {
            error_type: ErrorType::NotImplementedError,
            message: "cannot apply operator to tuple".to_string(),
            tokens: vec![],
        })
    }
}

pub fn interpret_expression(expression: Expression, context: &Context) -> Result<Value, Error> {
    match expression.data {
        ExpressionData::Constant(val) => Ok(Value::Number(val)),
        ExpressionData::BinaryOp(lhs, op, rhs) => {
            let lhs_start = lhs.start;
            let rhs_end = rhs.end;
            let (lhs, rhs) = (
                interpret_expression(*lhs, context)?,
                interpret_expression(*rhs, context)?,
            );
            match execute_op(lhs, rhs, op) {
                Ok(v) => Ok(v),
                Err(mut error) => {
                    let tokens: Vec<TokenID> = (lhs_start..rhs_end).into_iter().collect();
                    error.tokens.extend_from_slice(&tokens);
                    Err(error)
                }
            }
        }

        ExpressionData::Identifier(name) => match context.get(&name) {
            Some(val) => Ok(val.to_owned()),
            None => Err(Error::new(
                ErrorType::UnboundIdentifierError,
                format!("identifier '{}' is not bound", name),
                vec![expression.start],
            )),
        },

        ExpressionData::Tuple(v) => {
            let mut acc = vec![];
            for expression in v {
                acc.push(interpret_expression(expression, context)?);
            }
            Ok(Value::Tuple(acc))
        }
    }
}

pub fn print_value(value: Value) -> String {
    match value {
        Value::Number(x) => x.to_string(),
        Value::Tuple(v) => {
            if v.is_empty() {
                "()".to_string()
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

pub fn interpret(statements: Vec<Statement>) -> InterpreterOutput {
    let mut output = "".to_string();
    let mut context = Context::new();

    for statement in statements {
        match statement {
            Statement::Let(name, _, expression) => match interpret_expression(expression, &context)
            {
                Ok(value) => {
                    context.insert(name, value);
                }
                Err(error) => {
                    return InterpreterOutput {
                        output,
                        error: Some(error),
                    }
                }
            },
            Statement::Print(expression) => match interpret_expression(expression, &context) {
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
