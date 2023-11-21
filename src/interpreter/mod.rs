use crate::error::{Error, ErrorType};
use crate::lexer::Op;
use crate::parser::UnaryOp;
use crate::parser::{Expression, Statement, AST, E, S};
use crate::scope::Scope;
use std::collections::HashMap;

use serde::Serialize;

mod number_coersion;
mod stdlib_impl;
mod value;

pub type ExternalFunctionPointer<'a> = fn(Vec<Value<'a>>) -> Option<Value<'a>>;

use value::print_value;
pub use value::Value;

#[derive(Serialize)]
pub struct InterpreterOutput<'a> {
    pub stdout: String,
    pub error: Option<Error>,

    #[serde(skip_serializing)]
    pub value: Option<Value<'a>>,
}

fn execute_bin_op<'a>(lhs: Value<'a>, rhs: Value<'a>, op: &Op) -> Value<'a> {
    match &op {
        Op::Add
        | Op::Sub
        | Op::Mul
        | Op::Div
        | Op::Exp
        | Op::Greater
        | Op::Less
        | Op::GreaterOrEq
        | Op::LessOrEq => number_coersion::arith_coerce(lhs, op, rhs),

        Op::Equal => match (lhs, rhs) {
            (Value::F64(lv), Value::F64(rv)) => Value::Bool(lv == rv),
            (Value::F64(lv), Value::I64(rv)) => Value::Bool(lv == rv as f64),
            (Value::I64(lv), Value::F64(rv)) => Value::Bool(lv as f64 == rv),
            (Value::I64(lv), Value::I64(rv)) => Value::Bool(lv == rv),
            (Value::Bool(lv), Value::Bool(rv)) => Value::Bool(lv == rv),
            (Value::Tuple(lvs), Value::Tuple(rvs)) => {
                for (lv, rv) in lvs.iter().zip(rvs.iter()) {
                    if let Value::Bool(false) = execute_bin_op(lv.clone(), rv.clone(), &Op::Equal) {
                        return Value::Bool(false);
                    }
                }
                Value::Bool(true)
            }
            _ => unreachable!(),
        },

        Op::NotEqual => match (lhs, rhs) {
            (Value::F64(lv), Value::F64(rv)) => Value::Bool(lv != rv),
            (Value::F64(lv), Value::I64(rv)) => Value::Bool(lv != rv as f64),
            (Value::I64(lv), Value::F64(rv)) => Value::Bool(lv as f64 != rv),
            (Value::Bool(lv), Value::Bool(rv)) => Value::Bool(lv != rv),
            (Value::Tuple(lvs), Value::Tuple(rvs)) => {
                for (lv, rv) in lvs.iter().zip(rvs.iter()) {
                    if let Value::Bool(true) = execute_bin_op(lv.clone(), rv.clone(), &Op::NotEqual)
                    {
                        return Value::Bool(true);
                    }
                }
                Value::Bool(false)
            }
            _ => unreachable!(),
        },

        Op::And | Op::Or => {
            let coerced_lhs_bool = match lhs {
                Value::Bool(v) => Some(v),
                _ => None,
            }
            .unwrap();

            let coerced_rhs_bool = match rhs {
                Value::Bool(v) => Some(v),
                _ => None,
            }
            .unwrap();

            match &op {
                Op::And => Value::Bool(coerced_lhs_bool && coerced_rhs_bool),
                Op::Or => Value::Bool(coerced_lhs_bool || coerced_rhs_bool),
                _ => unreachable!(),
            }
        }

        Op::Comma => unreachable!(),
        Op::Tick => unreachable!(),
        Op::Not => unreachable!(),
        Op::Apply => unreachable!(),
        Op::Dot => match lhs {
            Value::Tuple(v) => match rhs {
                Value::I64(i) => v[i as usize].clone(),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        },
    }
}

pub fn interpret_expression<'a>(
    expression: &Expression,
    scope: &mut Scope<Value<'a>>,
    globals: &HashMap<String, Value<'a>>,
    output: &mut InterpreterOutput<'a>,
) -> Value<'a> {
    match &expression.data {
        E::F64(val) => Value::F64(*val),
        E::I64(val) => Value::I64(*val),
        E::Bool(val) => Value::Bool(*val),
        E::BinaryOp { lhs, op, rhs } => {
            let (lhs, rhs) = (
                interpret_expression(lhs, scope, globals, output),
                interpret_expression(rhs, scope, globals, output),
            );
            execute_bin_op(lhs, rhs, op)
        }

        E::UnaryOp {
            op: UnaryOp::ArithNeg,
            inner,
        } => match interpret_expression(inner, scope, globals, output) {
            Value::F64(val) => Value::F64(-val),
            Value::I64(val) => Value::I64(-val),
            _ => unreachable!(),
        },

        E::UnaryOp {
            op: UnaryOp::BoolNeg,
            inner,
        } => match interpret_expression(inner, scope, globals, output) {
            Value::Bool(val) => Value::Bool(!val),
            _ => unreachable!(),
        },

        E::UnaryOp {
            op: UnaryOp::Tick, ..
        } => unreachable!(),

        E::Accessor { .. } => {
            unreachable!()
        }

        E::Ident { name, .. } => match scope.get(name) {
            Some(val) => val.clone(),
            None => match globals.get(name) {
                Some(val) => val.clone(),
                None => unreachable!(),
            },
        },

        E::Tuple(inner) => {
            let mut members = vec![];
            for e in inner {
                members.push(interpret_expression(e, scope, globals, output));
            }
            Value::Tuple(members)
        }
        E::FnCall { func, args } => match interpret_expression(func, scope, globals, output) {
            Value::Function(arg_names, body) => {
                let mut inner_scope = Scope::default();

                inner_scope.push();

                for (arg_name, arg) in arg_names.iter().zip(args.iter()) {
                    let arg_value = interpret_expression(arg, scope, globals, output);
                    inner_scope.insert(arg_name.clone(), arg_value);
                }

                interpret_block(body, &mut inner_scope, globals, output);

                output.value.as_ref().unwrap_or(&Value::Void).clone()
            }
            Value::ExternalFunction(function_pointer) => {
                let arg_values: Vec<Value> = args
                    .iter()
                    .map(|arg| interpret_expression(arg, scope, globals, output))
                    .collect();
                function_pointer(arg_values).unwrap()
            }
            _ => unreachable!(),
        },

        E::OpenTuple(_) => unreachable!(),
    }
}

fn interpret_block<'a>(
    block: &'a [Statement],
    scope: &mut Scope<Value<'a>>,
    globals: &HashMap<String, Value<'a>>,
    output: &mut InterpreterOutput<'a>,
) -> bool {
    scope.push();

    for statement in block {
        match &statement.data {
            S::Let { name, value, .. } => {
                let value = interpret_expression(value, scope, globals, output);
                scope.insert(name.inner.clone(), value);
            }
            S::Assign { place, value } => {
                let value = interpret_expression(value, scope, globals, output);
                scope.assign(&place.inner, value);
            }
            S::Print(val) => {
                let val = interpret_expression(val, scope, globals, output);
                output.stdout.push_str(&print_value(val));
                output.stdout.push('\n');
            }
            S::If {
                condition,
                true_inner,
                false_inner,
                ..
            } => {
                if let Value::Bool(true) = interpret_expression(condition, scope, globals, output) {
                    if !interpret_block(&true_inner.statements, scope, globals, output) {
                        scope.pop();
                        return false;
                    }
                } else if let Some(false_inner) = false_inner {
                    if !interpret_block(&false_inner.statements, scope, globals, output) {
                        scope.pop();
                        return false;
                    }
                }
            }

            S::For {
                iterator,
                from,
                to,
                inner,
                ..
            } => {
                let mut i_val = match interpret_expression(from, scope, globals, output) {
                    Value::I64(v) => v,
                    _ => unreachable!(),
                };

                let target = match interpret_expression(to, scope, globals, output) {
                    Value::I64(v) => v,
                    _ => unreachable!(),
                };

                scope.push();
                scope.insert(iterator.inner.clone(), Value::I64(i_val));

                while i_val < target {
                    if !interpret_block(&inner.statements, scope, globals, output) {
                        scope.pop();
                        scope.pop();
                        return false;
                    }

                    i_val += 1;
                    scope.pop();
                    scope.push();
                    scope.insert(iterator.inner.clone(), Value::I64(i_val));
                }

                scope.pop();
            }
            S::Return(val) => {
                if let Some(val) = val {
                    output.value = Some(interpret_expression(val, scope, globals, output))
                }

                scope.pop();
                return false;
            }
        }
    }

    scope.pop();
    true
}

pub fn interpret(ast: AST) -> InterpreterOutput<'static> {
    let mut output = InterpreterOutput {
        stdout: String::new(),
        value: None,
        error: None,
    };

    let mut globals = stdlib_impl::stdlib();

    for (name, function) in ast.iter() {
        globals.insert(
            name.clone(),
            Value::Function(function.arg_names(), &function.body.statements),
        );
    }

    let main_code = if let Some(Value::Function(_, body)) = globals.get("main") {
        body
    } else {
        return InterpreterOutput {
            stdout: String::new(),
            value: None,
            error: Some(Error::new(
                ErrorType::Runtime,
                "no entry point; define function \"main\"".into(),
                0,
                0,
            )),
        };
    };

    interpret_block(main_code, &mut Scope::default(), &globals, &mut output);

    InterpreterOutput {
        stdout: output.stdout,
        value: None,
        error: None,
    }
}
