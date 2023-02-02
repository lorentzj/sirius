use crate::error::{Error, ErrorType};
use crate::parser::{Expression, ExpressionData, Statement, TypeExpression, TypeExpressionData};

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

pub fn annotation_type(annotation: &TypeExpression) -> Result<Type, Error> {
    match &annotation.data {
        TypeExpressionData::Identifier(identifier) => {
            if identifier.eq("f64") {
                Ok(Type::F64)
            } else {
                let tokens = (annotation.start..annotation.end).into_iter().collect();
                Err(Error::new(
                    ErrorType::TypeError,
                    "only \"f64\" is a valid primitive".to_string(),
                    tokens,
                ))
            }
        }
        TypeExpressionData::Tuple(v) => {
            let mut members = vec![];
            for expression in v {
                members.push(annotation_type(expression)?);
            }
            Ok(Type::Tuple(members))
        }
    }
}

pub fn expression_type(expression: &Expression, context: &Context) -> Result<Type, Error> {
    let tokens = (expression.start..expression.end).into_iter().collect();

    match &expression.data {
        ExpressionData::Identifier(identifier) => match context.get(identifier) {
            Some(t) => Ok(t.clone()),
            None => Err(Error::new(
                ErrorType::UnboundIdentifierError,
                format!("identifier '{}' is not bound", identifier),
                tokens,
            )),
        },
        ExpressionData::Constant(_) => Ok(Type::F64),
        ExpressionData::Tuple(v) => {
            let mut members = vec![];
            for expression in v {
                members.push(expression_type(expression, context)?);
            }
            Ok(Type::Tuple(members))
        }
        ExpressionData::BinaryOp(lhs, _, rhs) => {
            if let (Type::F64, Type::F64) = (
                expression_type(lhs, context)?,
                expression_type(rhs, context)?,
            ) {
                Ok(Type::F64)
            } else {
                Err(Error::new(
                    ErrorType::TypeError,
                    "Cannot apply operator to tuple".to_string(),
                    tokens,
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
            Statement::Let(name, annotation, expression) => {
                match expression_type(expression, &context) {
                    Ok(t) => {
                        if let Some(annotation) = annotation {
                            let annotation_start = annotation.start;
                            let annotation_end = annotation.end;
                            match annotation_type(annotation) {
                                Ok(ann_t) => {
                                    if ann_t != t {
                                        let tokens = (annotation_start..annotation_end)
                                            .into_iter()
                                            .collect();

                                        errors.push(Error::new(
                                            ErrorType::TypeError,
                                            format!(
                                                "annotation '{}' does not match expression '{}'",
                                                print_type(&ann_t),
                                                print_type(&t)
                                            ),
                                            tokens,
                                        ));
                                    }
                                }
                                Err(error) => errors.push(error),
                            }
                        }
                        context.insert(name.to_string(), t);
                    }
                    Err(error) => errors.push(error),
                }
            }
            Statement::Print(expression) => match expression_type(expression, &context) {
                Ok(_) => (),
                Err(error) => errors.push(error),
            },
        }
    }

    errors
}
