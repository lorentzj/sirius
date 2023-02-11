use std::collections::{HashMap, HashSet};

use crate::error::{Error, ErrorType};
use crate::lexer::Op;
use crate::parser::{Expression, Function, Statement, AST};
use crate::stack::Frame;

use serde::Serialize;

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum Value {
    Float(f64),
    Bool(bool),
    Tuple(Vec<Value>),
}

#[derive(Debug, PartialEq, Serialize)]
pub struct InterpreterOutput {
    pub value: Option<Value>,
    pub stdout: String,
    pub error: Option<Error>,
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
                Op::Div => Ok(Value::Float(coerced_lhs_float / coerced_rhs_float)),
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

        Op::Comma => panic!(),

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
    frame: &Frame<Value>,
    ast: &HashMap<String, Function>,
    output: &mut InterpreterOutput,
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
                interpret_expression(lhs, frame, ast, output)?,
                interpret_expression(rhs, frame, ast, output)?,
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

        Expression::Identifier { name, .. } => match frame.get(&name.clone()) {
            Some(val) => Ok(val.to_owned()),
            None => panic!(),
        },

        Expression::Tuple { inner, .. } => {
            let mut members = vec![];
            for expression in inner {
                members.push(interpret_expression(expression, frame, ast, output)?);
            }
            Ok(Value::Tuple(members))
        }

        Expression::FnCall { name, args, .. } => match ast.get(name) {
            Some(function) => {
                let mut inner_frame = Frame::<Value>::default();
                inner_frame.push_scope();
                for (arg, (arg_name, _)) in args.iter().zip(function.args.iter()) {
                    inner_frame.insert(
                        arg_name.clone(),
                        interpret_expression(arg, frame, ast, output)?,
                    );
                }

                interpret_block(&function.inner, Some(&mut inner_frame), ast, output);

                Ok(output.value.clone().unwrap())
            }

            None => panic!(),
        },

        Expression::OpenTuple { .. } => panic!(),
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

fn interpret_block(
    block: &[Statement],
    frame: Option<&mut Frame<Value>>,
    ast: &AST,
    output: &mut InterpreterOutput,
) {
    let mut empty_frame = Frame::<Value>::default();
    let frame = match frame {
        Some(frame) => frame,
        None => &mut empty_frame,
    };

    frame.push_scope();

    let mut defined_idents: HashSet<String> = HashSet::new();

    for statement in block {
        match statement {
            Statement::Let { name, val, .. } => match interpret_expression(val, frame, ast, output)
            {
                Ok(value) => {
                    frame.insert(name.clone(), value);
                    defined_idents.insert(name.clone());
                }
                Err(error) => {
                    output.error = Some(error);
                    return;
                }
            },
            Statement::Print { val, .. } => match interpret_expression(val, frame, ast, output) {
                Ok(value) => {
                    output.stdout.push_str(&print_value(value));
                    output.stdout.push('\n');
                }
                Err(error) => {
                    output.error = Some(error);
                    return;
                }
            },
            Statement::If { cond, inner, .. } => {
                match interpret_expression(cond, frame, ast, output) {
                    Ok(value) => {
                        if Value::Bool(true) == value {
                            interpret_block(inner, Some(frame), ast, output);

                            if output.error.is_some() {
                                return;
                            }

                            frame.pop_scope();
                        }
                    }
                    Err(error) => {
                        output.error = Some(error);
                        return;
                    }
                }
            }

            Statement::Return { val, .. } => {
                if let Some(val) = val {
                    match interpret_expression(val, frame, ast, output) {
                        Ok(value) => {
                            output.value = Some(value);
                            return;
                        }
                        Err(error) => {
                            output.error = Some(error);
                            return;
                        }
                    }
                } else {
                    return;
                }
            }
        }
    }
}

pub fn interpret(ast: &AST) -> InterpreterOutput {
    let mut output = InterpreterOutput {
        stdout: String::new(),
        value: None,
        error: None,
    };

    match ast.get("main") {
        Some(function) => {
            interpret_block(&function.inner, None, ast, &mut output);
            output
        }
        None => panic!(),
    }
}
