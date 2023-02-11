use std::fmt;

use crate::error::{Error, ErrorType};
use crate::lexer::Op;
use crate::parser::{Expression, Statement, AST};
use crate::stack::Frame;

#[derive(PartialEq, Clone)]
pub enum Type {
    F64,
    Bool,
    Tuple(Vec<Type>),
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::F64 => write!(f, "f64"),
            Type::Bool => write!(f, "bool"),
            Type::Tuple(v) => {
                if v.is_empty() {
                    write!(f, "())")
                } else {
                    let mut out = "(".to_string();
                    for t in v {
                        out.push_str(&format!("{t:?}"));
                        out.push(',');
                        out.push(' ');
                    }
                    out.pop();
                    out.pop();
                    out.push(')');
                    write!(f, "{out}")
                }
            }
        }
    }
}

pub fn annotation_type(annotation: &Expression) -> Result<Type, Error> {
    match &annotation {
        Expression::Identifier { start, name, end } => {
            if name.eq("f64") {
                Ok(Type::F64)
            } else if name.eq("bool") {
                Ok(Type::Bool)
            } else {
                Err(Error::new(
                    ErrorType::TypeError,
                    "only 'f64' and 'bool' are valid primitives".to_string(),
                    *start,
                    *end,
                ))
            }
        }

        Expression::Tuple { inner, .. } => {
            let mut members = vec![];
            for expression in inner {
                members.push(annotation_type(expression)?);
            }
            Ok(Type::Tuple(members))
        }

        _ => {
            let (start, end) = annotation.range();
            Err(Error::new(
                ErrorType::TypeError,
                "illegal expression in annotation".to_string(),
                start,
                end,
            ))
        }
    }
}

pub fn expression_type(expression: &Expression, frame: &Frame<Type>) -> Result<Type, Error> {
    let (start, end) = expression.range();

    match &expression {
        Expression::Identifier { name, .. } => match frame.get(name) {
            Some(t) => Ok(t.clone()),
            None => Err(Error::new(
                ErrorType::UnboundIdentifierError,
                format!("identifier '{name}' is not bound"),
                start,
                end,
            )),
        },
        Expression::Float { .. } => Ok(Type::F64),
        Expression::Bool { .. } => Ok(Type::Bool),
        Expression::Tuple { inner, .. } => {
            let mut members = vec![];
            for expression in inner {
                members.push(expression_type(expression, frame)?);
            }
            Ok(Type::Tuple(members))
        }

        Expression::OpenTuple { .. } => Err(Error::new(
            ErrorType::InternalError,
            "open tuple".into(),
            start,
            end,
        )),

        Expression::BinOp { lhs, rhs, op, .. } => {
            let lhs_type = expression_type(lhs, frame)?;
            let rhs_type = expression_type(rhs, frame)?;

            match &op {
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Exp => {
                    if let Type::Tuple(_) = lhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            "cannot apply operator to tuple".into(),
                            start,
                            end,
                        ))
                    } else if let Type::Tuple(_) = rhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            "cannot apply operator to tuple".into(),
                            start,
                            end,
                        ))
                    } else {
                        Ok(Type::F64)
                    }
                }
                Op::And | Op::Or => {
                    if let Type::Tuple(_) = lhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            "cannot apply operator to tuple".into(),
                            start,
                            end,
                        ))
                    } else if Type::F64 == lhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            "cannot apply boolean operator to float".into(),
                            start,
                            end,
                        ))
                    } else if let Type::Tuple(_) = rhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            "cannot apply operator to tuple".into(),
                            start,
                            end,
                        ))
                    } else if Type::F64 == rhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            "cannot apply boolean operator to float".into(),
                            start,
                            end,
                        ))
                    } else {
                        Ok(Type::Bool)
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
    }
}

fn typecheck_block(statements: &[Statement], frame: Option<&mut Frame<Type>>) -> Vec<Error> {
    let mut empty_frame = Frame::<Type>::default();
    let frame = match frame {
        Some(frame) => frame,
        None => &mut empty_frame,
    };

    frame.push_scope();
    let mut errors = vec![];

    for statement in statements {
        match statement {
            Statement::Let { name, ann, val } => match expression_type(val, frame) {
                Ok(t) => {
                    if let Some(ann) = ann {
                        match annotation_type(ann) {
                            Ok(ann_t) => {
                                if ann_t != t {
                                    let (val_start, val_end) = val.range();

                                    errors.push(Error::new(
                                        ErrorType::TypeError,
                                        format!("annotation '{ann_t:?}' does not match expression '{t:?}'"),
                                        val_start,
                                        val_end,
                                    ));
                                }
                            }
                            Err(error) => errors.push(error),
                        }
                    }
                    frame.insert(name.into(), t);
                }
                Err(error) => errors.push(error),
            },
            Statement::Print { val } => match expression_type(val, frame) {
                Ok(_) => (),
                Err(error) => errors.push(error),
            },

            Statement::If { cond, inner } => match expression_type(cond, frame) {
                Ok(t) => {
                    if t != Type::Bool {
                        let (cond_start, cond_end) = cond.range();

                        errors.push(Error::new(
                            ErrorType::TypeError,
                            format!("condition must be bool; found type '{t:?}'"),
                            cond_start,
                            cond_end,
                        ));
                    }

                    let mut inner_errors = typecheck_block(inner, Some(frame));
                    frame.pop_scope();
                    errors.append(&mut inner_errors);
                }
                Err(error) => errors.push(error),
            },
        }
    }

    errors
}

pub fn typecheck(ast: &AST) -> Vec<Error> {
    let mut errors: Vec<Error> = vec![];

    match ast.get("main") {
        Some(function) => {
            if let Some((_, ann)) = function.args.first() {
                errors.push(Error::new(
                    ErrorType::TypeError,
                    "main should have no arguments".into(),
                    ann.range().0 - 2,
                    function.args.last().unwrap().1.range().1,
                ));
            }

            if let Some(ann) = &function.return_type {
                let (ann_start, ann_end) = ann.range();
                errors.push(Error::new(
                    ErrorType::TypeError,
                    "main should have no return type".into(),
                    ann_start - 1,
                    ann_end,
                ));
            }
        }

        None => errors.push(Error::new(
            ErrorType::TypeError,
            "no entry point; define function 'main'".into(),
            0,
            0,
        )),
    }

    for (_, function) in ast.iter() {
        let mut frame = Frame::<Type>::default();
        frame.push_scope();
        for (name, ann) in &function.args {
            match annotation_type(ann) {
                Ok(ann) => {
                    frame.insert(name.clone(), ann);
                }
                Err(error) => {
                    errors.push(error);
                }
            }
        }

        errors.append(&mut typecheck_block(&function.inner, Some(&mut frame)));
    }

    errors
}
