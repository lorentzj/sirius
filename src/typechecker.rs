use std::collections::HashMap;
use std::fmt;

use crate::error::{cardinal, Error, ErrorType};
use crate::lexer::Op;
use crate::parser::{Expression, Statement, AST};
use crate::stack::Frame;

type GlobalFunctionTypes = HashMap<String, (Vec<Type>, Option<Type>)>;

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

pub fn expression_type(
    expression: &Expression,
    frame: &Frame<Type>,
    functions: &GlobalFunctionTypes,
) -> Result<Type, Error> {
    let (start, end) = expression.range();

    match &expression {
        Expression::Identifier { name, .. } => match frame.get(name) {
            Some(t) => Ok(t.clone()),
            None => {
                if functions.get(name).is_some() {
                    Err(Error::new(
                        ErrorType::TypeError,
                        "cannot use function as value".into(),
                        start,
                        end,
                    ))
                } else {
                    Err(Error::new(
                        ErrorType::UnboundIdentifierError,
                        format!("identifier '{name}' not found in scope"),
                        start,
                        end,
                    ))
                }
            }
        },
        Expression::Float { .. } => Ok(Type::F64),
        Expression::Bool { .. } => Ok(Type::Bool),
        Expression::Tuple { inner, .. } => {
            let mut members = vec![];
            for expression in inner {
                members.push(expression_type(expression, frame, functions)?);
            }
            Ok(Type::Tuple(members))
        }

        Expression::OpenTuple { .. } => panic!(),

        Expression::BinOp { lhs, rhs, op, .. } => {
            let lhs_type = expression_type(lhs, frame, functions)?;
            let rhs_type = expression_type(rhs, frame, functions)?;

            match &op {
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Exp => {
                    if let Type::Tuple(_) = lhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            format!("cannot apply arithmetic operator to '{lhs_type:?}'"),
                            start,
                            end,
                        ))
                    } else if let Type::Tuple(_) = rhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            format!("cannot apply arithmetic operator to '{rhs_type:?}'"),
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
                            format!("cannot apply boolean operator to '{lhs_type:?}'"),
                            start,
                            end,
                        ))
                    } else if Type::F64 == lhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            format!("cannot apply boolean operator to '{lhs_type:?}'"),
                            start,
                            end,
                        ))
                    } else if let Type::Tuple(_) = rhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            format!("cannot apply boolean operator to '{rhs_type:?}'"),
                            start,
                            end,
                        ))
                    } else if Type::F64 == rhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            format!("cannot apply boolean operator to '{rhs_type:?}'"),
                            start,
                            end,
                        ))
                    } else {
                        Ok(Type::Bool)
                    }
                }

                Op::Greater | Op::Less => {
                    if let Type::Tuple(_) = lhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            format!("cannot apply comparison operator to '{lhs_type:?}'"),
                            start,
                            end,
                        ))
                    } else if Type::Bool == lhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            format!("cannot apply comparison operator to '{lhs_type:?}'"),
                            start,
                            end,
                        ))
                    } else if let Type::Tuple(_) = rhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            format!("cannot apply comparison operator to '{rhs_type:?}'"),
                            start,
                            end,
                        ))
                    } else if Type::Bool == rhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            format!("cannot apply comparison operator to '{rhs_type:?}'"),
                            start,
                            end,
                        ))
                    } else {
                        Ok(Type::Bool)
                    }
                }

                Op::Equal => {
                    if lhs_type == rhs_type {
                        Ok(Type::Bool)
                    } else {
                        Err(Error::new(
                            ErrorType::TypeError,
                            format!(
                                "cannot check equality between '{lhs_type:?}' and '{rhs_type:?}'"
                            ),
                            start,
                            end,
                        ))
                    }
                }

                Op::Comma => panic!(),

                Op::Dot => Err(Error::new(
                    ErrorType::NotImplementedError,
                    "have not implemented dot operator".into(),
                    start,
                    end,
                )),
            }
        }

        Expression::FnCall {
            start,
            name,
            args,
            end,
        } => {
            if let Some((arg_types, return_type)) = functions.get(name) {
                if args.len() != arg_types.len() {
                    Err(Error::new(
                        ErrorType::TypeError,
                        format!(
                            "expected {} argument{} to function '{}'; got {}",
                            arg_types.len(),
                            if arg_types.len() == 1 { "" } else { "s" },
                            name,
                            args.len()
                        ),
                        *start,
                        *end,
                    ))
                } else {
                    for (i, arg) in args.iter().enumerate() {
                        let supplied_arg_type = expression_type(arg, frame, functions)?;
                        if supplied_arg_type != arg_types[i] {
                            let (arg_start, arg_end) = arg.range();
                            return Err(Error::new(
                                ErrorType::TypeError,
                                format!(
                                    "expected '{:?}' as {} argument to function '{}'; got '{:?}'",
                                    arg_types[i],
                                    cardinal(i + 1),
                                    name,
                                    supplied_arg_type
                                ),
                                arg_start,
                                arg_end,
                            ));
                        }
                    }

                    if let Some(return_type) = return_type {
                        Ok(return_type.clone())
                    } else {
                        Err(Error::new(
                            ErrorType::TypeError,
                            format!("function '{name}' has void return type"),
                            *start,
                            *end,
                        ))
                    }
                }
            } else if let Some(t) = frame.get(name) {
                Err(Error::new(
                    ErrorType::TypeError,
                    format!("cannot call '{t:?}' as function"),
                    *start,
                    *end,
                ))
            } else {
                Err(Error::new(
                    ErrorType::UnboundIdentifierError,
                    format!("function '{name}' not found in scope"),
                    *start,
                    *end,
                ))
            }
        }
    }
}

fn typecheck_block(
    statements: &[Statement],
    frame: Option<&mut Frame<Type>>,
    functions: &GlobalFunctionTypes,
    curr_function: &str,
) -> Vec<Error> {
    let mut empty_frame = Frame::default();
    let frame = match frame {
        Some(frame) => frame,
        None => &mut empty_frame,
    };

    frame.push_scope();
    let mut errors = vec![];

    for statement in statements {
        match statement {
            Statement::Let { name, ann, val, .. } => match expression_type(val, frame, functions) {
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
            Statement::Print { val, .. } => match expression_type(val, frame, functions) {
                Ok(_) => (),
                Err(error) => errors.push(error),
            },

            Statement::If {
                cond,
                true_inner,
                false_inner,
                ..
            } => match expression_type(cond, frame, functions) {
                Ok(t) => {
                    if t != Type::Bool {
                        let (cond_start, cond_end) = cond.range();

                        errors.push(Error::new(
                            ErrorType::TypeError,
                            format!("condition should be 'bool'; found '{t:?}'"),
                            cond_start,
                            cond_end,
                        ));
                    }

                    let mut true_inner_errors =
                        typecheck_block(true_inner, Some(frame), functions, curr_function);
                    frame.pop_scope();
                    errors.append(&mut true_inner_errors);

                    if let Some(false_inner) = false_inner {
                        let mut false_inner_errors =
                            typecheck_block(false_inner, Some(frame), functions, curr_function);
                        frame.pop_scope();
                        errors.append(&mut false_inner_errors);
                    }
                }
                Err(error) => errors.push(error),
            },
            Statement::Return { start, val, .. } => match val {
                Some(val) => {
                    let (val_start, val_end) = val.range();
                    match expression_type(val, frame, functions) {
                        Ok(supplied_return_type) => {
                            if let Some((_, return_type)) = functions.get(curr_function) {
                                if let Some(return_type) = return_type {
                                    if *return_type != supplied_return_type {
                                        errors.push(Error::new(
                                            ErrorType::TypeError,
                                            format!(
                                                "return type of function '{curr_function}' should be '{return_type:?}'; found '{supplied_return_type:?}'"
                                            ),
                                            val_start,
                                            val_end,
                                        ));
                                    }
                                } else {
                                    errors.push(Error::new(
                                        ErrorType::TypeError,
                                        format!(
                                            "return type of function '{curr_function}' should be void; found '{supplied_return_type:?}'"
                                        ),
                                        val_start,
                                        val_end,
                                    ));
                                }
                            }
                        }

                        Err(error) => errors.push(error),
                    }
                }
                None => {
                    let (_, return_type) = functions.get(curr_function).unwrap();
                    if return_type.is_some() {
                        errors.push(Error::new(
                            ErrorType::TypeError,
                            format!(
                                "return type of function '{curr_function}' should be '{:?}'; found void",
                                return_type.as_ref().unwrap()
                            ),
                            *start,
                            *start + 1,
                        ));
                    }
                }
            },
        }
    }

    errors
}

pub fn typecheck(ast: &AST) -> Vec<Error> {
    let mut errors: Vec<Error> = vec![];

    if let Some(function) = ast.get("main") {
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
                "main should have void return type".into(),
                ann_start - 1,
                ann_end,
            ));
        }
    }

    let functions: GlobalFunctionTypes = ast
        .iter()
        .filter_map(|(name, function)| {
            let mut arg_types = vec![];

            for (_, arg_ann) in &function.args {
                match annotation_type(arg_ann) {
                    Ok(t) => arg_types.push(t),
                    Err(error) => {
                        errors.push(error);
                        return None;
                    }
                }
            }

            let return_type = match &function.return_type {
                Some(return_type) => match annotation_type(return_type) {
                    Ok(t) => Some(t),
                    Err(error) => {
                        errors.push(error);
                        None
                    }
                },
                None => None,
            };

            Some((name.clone(), (arg_types, return_type)))
        })
        .collect();

    for (function_name, function) in ast.iter() {
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

        errors.append(&mut typecheck_block(
            &function.inner,
            Some(&mut frame),
            &functions,
            function_name,
        ));
    }

    errors
}
