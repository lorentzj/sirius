use std::collections::HashMap;
use std::fmt;

use crate::error::{cardinal, Error, ErrorType};
use crate::lexer::Op;
use crate::parser::{Expression, Statement, UnaryOp, AST};
use crate::stack::Frame;

type Globals = HashMap<String, Type>;

#[derive(PartialEq, Clone)]
pub enum Type {
    F64,
    Bool,
    Tuple(Vec<Type>),
    Function(Vec<Type>, Option<Box<Type>>),
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
                    let mut res = "(".to_string();
                    for t in v {
                        res.push_str(&format!("{t:?}"));
                        res.push(',');
                        res.push(' ');
                    }
                    res.pop();
                    res.pop();
                    res.push(')');
                    write!(f, "{res}")
                }
            }
            Type::Function(i, o) => {
                let mut res = "fn(".to_string();
                for t in i {
                    res.push_str(&format!("{t:?}"));
                    res.push(',');
                    res.push(' ');
                }

                res.pop();
                res.pop();
                res.push_str("): ");

                if let Some(o) = o {
                    res.push_str(&format!("{o:?}"));
                }

                write!(f, "{res}")
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
    globals: &Globals,
) -> Result<Type, Error> {
    let (start, end) = expression.range();

    match &expression {
        Expression::Identifier { name, .. } => match frame.get(name) {
            Some(t) => Ok(t.clone()),
            None => match globals.get(name) {
                Some(t) => Ok(t.clone()),
                None => Err(Error::new(
                    ErrorType::UnboundIdentifierError,
                    format!("identifier '{name}' not found in scope"),
                    start,
                    end,
                )),
            },
        },
        Expression::Float { .. } => Ok(Type::F64),
        Expression::Bool { .. } => Ok(Type::Bool),
        Expression::Tuple { inner, .. } => {
            let mut members = vec![];
            for expression in inner {
                members.push(expression_type(expression, frame, globals)?);
            }
            Ok(Type::Tuple(members))
        }

        Expression::OpenTuple { .. } => panic!(),

        Expression::BinaryOp { lhs, rhs, op, .. } => {
            let lhs_type = expression_type(lhs, frame, globals)?;
            let rhs_type = expression_type(rhs, frame, globals)?;

            match &op {
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Exp => {
                    match (lhs_type.clone(), rhs_type.clone()) {
                        (Type::F64 | Type::Bool, Type::F64 | Type::Bool) => Ok(Type::F64),
                        _ => {
                            if let Type::Bool | Type::F64 = lhs_type {
                                Err(Error::new(
                                    ErrorType::TypeError,
                                    format!("cannot apply arithmetic operator to '{rhs_type:?}'"),
                                    start,
                                    end,
                                ))
                            } else {
                                Err(Error::new(
                                    ErrorType::TypeError,
                                    format!("cannot apply arithmetic operator to '{lhs_type:?}'"),
                                    start,
                                    end,
                                ))
                            }
                        }
                    }
                }

                Op::And | Op::Or => match (lhs_type.clone(), rhs_type.clone()) {
                    (Type::Bool, Type::Bool) => Ok(Type::Bool),
                    _ => {
                        if Type::Bool == lhs_type {
                            Err(Error::new(
                                ErrorType::TypeError,
                                format!("cannot apply boolean operator to '{rhs_type:?}'"),
                                start,
                                end,
                            ))
                        } else {
                            Err(Error::new(
                                ErrorType::TypeError,
                                format!("cannot apply boolean operator to '{lhs_type:?}'"),
                                start,
                                end,
                            ))
                        }
                    }
                },

                Op::Greater | Op::Less => match (lhs_type.clone(), rhs_type.clone()) {
                    (Type::F64, Type::F64) => Ok(Type::Bool),
                    _ => {
                        if Type::F64 == lhs_type {
                            Err(Error::new(
                                ErrorType::TypeError,
                                format!("cannot apply comparison operator to '{rhs_type:?}'"),
                                start,
                                end,
                            ))
                        } else {
                            Err(Error::new(
                                ErrorType::TypeError,
                                format!("cannot apply comparison operator to '{lhs_type:?}'"),
                                start,
                                end,
                            ))
                        }
                    }
                },

                Op::Equal => {
                    if let Type::Function(_, _) = lhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            "cannot check equality between functions".into(),
                            start,
                            end,
                        ))
                    } else if let Type::Function(_, _) = rhs_type {
                        Err(Error::new(
                            ErrorType::TypeError,
                            "cannot check equality between functions".into(),
                            start,
                            end,
                        ))
                    } else if lhs_type == rhs_type {
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
                Op::Not => panic!(),
                Op::Dot => Err(Error::new(
                    ErrorType::NotImplementedError,
                    "have not implemented dot operator".into(),
                    start,
                    end,
                )),
            }
        }

        Expression::UnaryOp {
            op: UnaryOp::ArithNeg,
            inner,
            ..
        } => {
            let inner_type = expression_type(inner, frame, globals)?;
            if inner_type == Type::F64 {
                Ok(Type::F64)
            } else {
                Err(Error::new(
                    ErrorType::TypeError,
                    format!("cannot apply arithmetic negation to '{inner_type:?}'"),
                    start,
                    end,
                ))
            }
        }

        Expression::UnaryOp {
            op: UnaryOp::BoolNeg,
            inner,
            ..
        } => {
            let inner_type = expression_type(inner, frame, globals)?;
            if inner_type == Type::Bool {
                Ok(Type::Bool)
            } else {
                Err(Error::new(
                    ErrorType::TypeError,
                    format!("cannot apply boolean negation to '{inner_type:?}'"),
                    start,
                    end,
                ))
            }
        }

        Expression::FnCall { caller, args, .. } => {
            let caller_type = expression_type(caller, frame, globals)?;
            match caller_type {
                Type::Function(arg_types, return_type) => {
                    let function_name = match caller.as_ref() {
                        Expression::Identifier { name, .. } => format!("function '{name}'"),
                        _ => "anonymous function".into(),
                    };

                    if args.len() != arg_types.len() {
                        Err(Error::new(
                            ErrorType::TypeError,
                            format!(
                                "expected {} argument{} to {}; got {}",
                                arg_types.len(),
                                if arg_types.len() == 1 { "" } else { "s" },
                                function_name,
                                args.len()
                            ),
                            start,
                            end,
                        ))
                    } else {
                        for (i, arg) in args.iter().enumerate() {
                            let supplied_arg_type = expression_type(arg, frame, globals)?;
                            if supplied_arg_type != arg_types[i] {
                                let (arg_start, arg_end) = arg.range();
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    format!(
                                        "expected '{:?}' as {} argument to {}; got '{:?}'",
                                        arg_types[i],
                                        cardinal(i + 1),
                                        function_name,
                                        supplied_arg_type
                                    ),
                                    arg_start,
                                    arg_end,
                                ));
                            }
                        }

                        if let Some(return_type) = return_type {
                            Ok(*return_type)
                        } else {
                            Err(Error::new(
                                ErrorType::TypeError,
                                format!("{function_name} has void return type"),
                                start,
                                end,
                            ))
                        }
                    }
                }
                t => Err(Error::new(
                    ErrorType::TypeError,
                    format!("cannot call '{t:?}' as function"),
                    start,
                    end,
                )),
            }
        }
    }
}

fn typecheck_block(
    statements: &[Statement],
    frame: Option<&mut Frame<Type>>,
    globals: &Globals,
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
            Statement::Let { name, ann, val, .. } => match expression_type(val, frame, globals) {
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
            Statement::Print { val, .. } => match expression_type(val, frame, globals) {
                Ok(_) => (),
                Err(error) => errors.push(error),
            },

            Statement::If {
                cond,
                true_inner,
                false_inner,
                ..
            } => match expression_type(cond, frame, globals) {
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
                        typecheck_block(true_inner, Some(frame), globals, curr_function);
                    frame.pop_scope();
                    errors.append(&mut true_inner_errors);

                    if let Some(false_inner) = false_inner {
                        let mut false_inner_errors =
                            typecheck_block(false_inner, Some(frame), globals, curr_function);
                        frame.pop_scope();
                        errors.append(&mut false_inner_errors);
                    }
                }
                Err(error) => errors.push(error),
            },
            Statement::Return { start, val, .. } => match val {
                Some(val) => {
                    let (val_start, val_end) = val.range();
                    match expression_type(val, frame, globals) {
                        Ok(supplied_return_type) => {
                            if let Some(Type::Function(_, return_type)) = globals.get(curr_function)
                            {
                                if let Some(return_type) = return_type {
                                    if *return_type.as_ref() != supplied_return_type {
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
                    let Some(Type::Function(_, return_type)) = globals.get(curr_function) else {
                        continue;
                    };

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

    let globals: Globals = ast
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
                    Ok(t) => Some(Box::new(t)),
                    Err(error) => {
                        errors.push(error);
                        None
                    }
                },
                None => None,
            };

            Some((name.clone(), Type::Function(arg_types, return_type)))
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
            &globals,
            function_name,
        ));
    }

    errors
}
