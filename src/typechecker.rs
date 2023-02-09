use crate::error::{Error, ErrorType};
use crate::parser::{Expression, Statement};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    F64,
    Tuple(Vec<Type>),
}

pub fn print_type(t: &Type) -> String {
    match t {
        Type::F64 => "f64".to_string(),
        Type::Tuple(v) => {
            if v.is_empty() {
                "()".to_string()
            } else {
                let mut out = "(".to_string();
                for val in v {
                    out.push_str(&print_type(val));
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

pub type Context = std::collections::HashMap<String, Type>;

pub fn annotation_type(annotation: &Expression) -> Result<Type, Error> {
    match &annotation {
        Expression::Identifier { start, name, end } => {
            if name.eq("f64") {
                Ok(Type::F64)
            } else {
                Err(Error::new(
                    ErrorType::TypeError,
                    "only \"f64\" is a valid primitive".to_string(),
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

pub fn expression_type(expression: &Expression, context: &Context) -> Result<Type, Error> {
    let (start, end) = expression.range();

    match &expression {
        Expression::Identifier { name, .. } => match context.get(name) {
            Some(t) => Ok(t.clone()),
            None => Err(Error::new(
                ErrorType::UnboundIdentifierError,
                format!("identifier '{}' is not bound", name),
                start,
                end,
            )),
        },
        Expression::Constant { .. } => Ok(Type::F64),
        Expression::Tuple { inner, .. } => {
            let mut members = vec![];
            for expression in inner {
                members.push(expression_type(expression, context)?);
            }
            Ok(Type::Tuple(members))
        }

        Expression::OpenTuple { .. } => Err(Error::new(
            ErrorType::InternalError,
            "open tuple".into(),
            start,
            end,
        )),

        Expression::BinOp { lhs, rhs, .. } => {
            if let (Type::F64, Type::F64) = (
                expression_type(lhs, context)?,
                expression_type(rhs, context)?,
            ) {
                Ok(Type::F64)
            } else {
                Err(Error::new(
                    ErrorType::TypeError,
                    "cannot apply operator to tuple".into(),
                    start,
                    end,
                ))
            }
        }
    }
}

pub fn typecheck(statements: &[Statement]) -> Vec<Error> {
    let mut errors = vec![];
    let mut context = Context::new();

    for statement in statements {
        match statement {
            Statement::Let { name, ann, val } => match expression_type(val, &context) {
                Ok(t) => {
                    if let Some(ann) = ann {
                        let (ann_start, ann_end) = ann.range();

                        match annotation_type(ann) {
                            Ok(ann_t) => {
                                if ann_t != t {
                                    errors.push(Error::new(
                                        ErrorType::TypeError,
                                        format!(
                                            "annotation '{}' does not match expression '{}'",
                                            print_type(&ann_t),
                                            print_type(&t)
                                        ),
                                        ann_start,
                                        ann_end,
                                    ));
                                }
                            }
                            Err(error) => errors.push(error),
                        }
                    }
                    context.insert(name.into(), t);
                }
                Err(error) => errors.push(error),
            },
            Statement::Print(expression) => match expression_type(expression, &context) {
                Ok(_) => (),
                Err(error) => errors.push(error),
            },
        }
    }

    errors
}
