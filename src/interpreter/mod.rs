use crate::error::{Error, ErrorType};
use crate::lexer::Op;
use crate::parser::UnaryOp;
use crate::stack::Frame;
use crate::stdlib::ExternalGlobals;
use crate::typechecker::{TypedAST, TypedExpression, TypedStatement};
use std::collections::{HashMap, HashSet};

use serde::Serialize;

mod number_coersion;
pub type ExternalFunctionPointer = fn(&[Value]) -> Option<Value>;

#[derive(Clone)]
pub enum Value {
    F64(f64),
    I64(i64),
    Bool(bool),
    Tuple(Vec<Value>),
    Function {
        arg_names: Vec<String>,
        statements: Vec<TypedStatement>,
    },
    ExternalFunction(ExternalFunctionPointer),
}

type Globals = HashMap<String, Value>;

#[derive(Serialize)]
pub struct InterpreterOutput {
    pub stdout: String,
    pub error: Option<Error>,

    #[serde(skip_serializing)]
    pub value: Option<Value>,
}

fn execute_bin_op(lhs: Value, rhs: Value, op: &Op) -> Value {
    match &op {
        Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Exp | Op::Greater | Op::Less => {
            number_coersion::arith_coerce(lhs, op, rhs)
        }

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
            _ => panic!(),
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
            _ => panic!(),
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
                _ => panic!(),
            }
        }

        Op::Comma => panic!(),
        Op::Tick => panic!(),
        Op::Not => panic!(),
        Op::Apply => panic!(),
        Op::Dot => panic!(),
    }
}

pub fn interpret_expression(
    expression: &TypedExpression,
    frame: &mut Frame<Value>,
    globals: &Globals,
    externals: &ExternalGlobals,
    output: &mut InterpreterOutput,
) -> Value {
    match expression {
        TypedExpression::F64 { val, .. } => Value::F64(*val),
        TypedExpression::I64 { val, .. } => Value::I64(*val),
        TypedExpression::Bool { val, .. } => Value::Bool(*val),
        TypedExpression::BinaryOp { lhs, op, rhs, .. } => {
            let (lhs, rhs) = (
                interpret_expression(lhs, frame, globals, externals, output),
                interpret_expression(rhs, frame, globals, externals, output),
            );
            execute_bin_op(lhs, rhs, op)
        }

        TypedExpression::UnaryOp {
            op: UnaryOp::ArithNeg,
            inner,
            ..
        } => match interpret_expression(inner, frame, globals, externals, output) {
            Value::F64(val) => Value::F64(-val),
            Value::I64(val) => Value::I64(-val),
            _ => panic!(),
        },

        TypedExpression::UnaryOp {
            op: UnaryOp::BoolNeg,
            inner,
            ..
        } => match interpret_expression(inner, frame, globals, externals, output) {
            Value::Bool(val) => Value::Bool(!val),
            _ => panic!(),
        },

        TypedExpression::UnaryOp {
            op: UnaryOp::Tick, ..
        } => panic!(),

        TypedExpression::Identifier { name, .. } => match frame.get(name) {
            Some(val) => val.clone(),
            None => match globals.get(name) {
                Some(val) => val.clone(),
                None => match externals.get(name) {
                    Some((_, val)) => val.clone(),
                    None => panic!(),
                },
            },
        },

        TypedExpression::Tuple { inner, .. } => {
            let mut members = vec![];
            for expression in inner {
                members.push(interpret_expression(
                    expression, frame, globals, externals, output,
                ));
            }
            Value::Tuple(members)
        }

        TypedExpression::FnCall { caller, args, .. } => {
            match interpret_expression(caller, frame, globals, externals, output) {
                Value::Function {
                    arg_names,
                    statements,
                } => {
                    let mut inner_frame = Frame::<Value>::default();
                    inner_frame.push_scope();
                    for (arg, arg_name) in args.iter().zip(arg_names.iter()) {
                        inner_frame.insert(
                            arg_name.clone(),
                            interpret_expression(arg, frame, globals, externals, output),
                        );
                    }

                    interpret_block(
                        &statements,
                        Some(&mut inner_frame),
                        globals,
                        externals,
                        output,
                    );

                    output.value.as_ref().unwrap().clone()
                }
                Value::ExternalFunction(function_pointer) => {
                    let arg_values: Vec<Value> = args
                        .iter()
                        .map(|arg| interpret_expression(arg, frame, globals, externals, output))
                        .collect();
                    function_pointer(&arg_values).unwrap()
                }
                _ => panic!(),
            }
        }
    }
}

pub fn print_value(value: Value) -> String {
    match value {
        Value::F64(val) => val.to_string(),
        Value::I64(val) => val.to_string(),
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
        Value::Function { .. } => "<function>".into(),
        Value::ExternalFunction { .. } => "<external_function>".into(),
    }
}

fn interpret_block(
    block: &[TypedStatement],
    frame: Option<&mut Frame<Value>>,
    globals: &Globals,
    externals: &ExternalGlobals,
    output: &mut InterpreterOutput,
) -> bool {
    let mut empty_frame = Frame::<Value>::default();
    let frame = match frame {
        Some(frame) => frame,
        None => &mut empty_frame,
    };

    frame.push_scope();

    let mut defined_idents: HashSet<String> = HashSet::new();

    for statement in block {
        match statement {
            TypedStatement::Let { name, val, .. } => {
                let val = interpret_expression(val, frame, globals, externals, output);
                frame.insert(name.clone(), val);
                defined_idents.insert(name.clone());
            }
            TypedStatement::Print { val, .. } => {
                let val = interpret_expression(val, frame, globals, externals, output);
                output.stdout.push_str(&print_value(val));
                output.stdout.push('\n');
            }
            TypedStatement::If {
                cond,
                true_inner,
                false_inner,
                ..
            } => {
                if let Value::Bool(true) =
                    interpret_expression(cond, frame, globals, externals, output)
                {
                    let cont = interpret_block(true_inner, Some(frame), globals, externals, output);
                    if !cont {
                        return false;
                    }
                } else if let Some(false_inner) = false_inner {
                    let cont =
                        interpret_block(false_inner, Some(frame), globals, externals, output);
                    if !cont {
                        return false;
                    }
                }

                frame.pop_scope();
            }

            TypedStatement::Return { val, .. } => {
                match val {
                    Some(val) => {
                        output.value =
                            Some(interpret_expression(val, frame, globals, externals, output))
                    }
                    None => (),
                }
                return false;
            }
        }
    }

    true
}

pub fn interpret(ast: TypedAST, externals: &ExternalGlobals) -> InterpreterOutput {
    let mut output = InterpreterOutput {
        stdout: String::new(),
        value: None,
        error: None,
    };

    let globals: Globals = ast
        .into_iter()
        .map(|(name, function)| {
            (
                name,
                Value::Function {
                    arg_names: function
                        .args
                        .iter()
                        .map(|(arg_name, _)| arg_name.clone())
                        .collect(),
                    statements: function.inner,
                },
            )
        })
        .collect();

    match globals.get("main") {
        Some(Value::Function { statements, .. }) => {
            interpret_block(statements, None, &globals, externals, &mut output);
            output
        }
        _ => {
            output.error = Some(Error::new(
                ErrorType::RuntimeError,
                "no entry point; define function 'main'".into(),
                0,
                0,
            ));
            output
        }
    }
}
