use super::types::Type;
use crate::error::{Error, ErrorType};
use crate::scope::Scope;

use crate::lexer::Op;
use crate::parser::{Expression, Positioned, Statement, E, S};

pub fn annotation_type(annotation: &Expression) -> Result<Type, (usize, usize)> {
    match &annotation.data {
        E::Ident(name, type_args) => {
            if type_args.is_some() {
                Err((annotation.start, annotation.end))
            } else if name.eq("f64") {
                Ok(Type::F64)
            } else if name.eq("i64") {
                Ok(Type::I64(None))
            } else if name.eq("bool") {
                Ok(Type::Bool)
            } else if name.eq("void") {
                Ok(Type::Void)
            } else if name.eq("_") {
                Ok(Type::ForAll(0))
            } else {
                Ok(Type::TypeVar(name.into()))
            }
        }

        E::Tuple(inner) => {
            let mut members = vec![];
            for expression in inner {
                members.push(annotation_type(expression)?);
            }
            Ok(Type::Tuple(members))
        }

        E::BinaryOp(
            box Expression {
                data: E::Tuple(inner),
                ..
            },
            Op::Apply,
            rhs,
        ) => {
            let mut args = vec![];
            for expression in inner {
                args.push(annotation_type(expression)?);
            }

            let return_type = annotation_type(rhs)?;

            Ok(Type::Function(vec![], args, Box::new(return_type)))
        }

        E::BinaryOp(lhs, Op::Apply, rhs) => {
            let args = vec![annotation_type(lhs)?];
            let return_type = annotation_type(rhs)?;

            Ok(Type::Function(vec![], args, Box::new(return_type)))
        }

        E::BinaryOp(lhs, Op::Add, rhs) => {
            let lhs = annotation_type(lhs)?;
            let rhs = annotation_type(rhs)?;

            if let (Type::I64(Some(lhs_ind)), Type::I64(Some(rhs_ind))) = (lhs, rhs) {
                Ok(Type::I64(Some(lhs_ind + rhs_ind)))
            } else {
                Err((annotation.start, annotation.end))
            }
        }

        E::BinaryOp(lhs, Op::Sub, rhs) => {
            let lhs = annotation_type(lhs)?;
            let rhs = annotation_type(rhs)?;

            if let (Type::I64(Some(lhs_ind)), Type::I64(Some(rhs_ind))) = (lhs, rhs) {
                Ok(Type::I64(Some(lhs_ind - rhs_ind)))
            } else {
                Err((annotation.start, annotation.end))
            }
        }

        E::BinaryOp(lhs, Op::Mul, rhs) => {
            let lhs = annotation_type(lhs)?;
            let rhs = annotation_type(rhs)?;

            if let (Type::I64(Some(lhs_ind)), Type::I64(Some(rhs_ind))) = (lhs, rhs) {
                Ok(Type::I64(Some(lhs_ind * rhs_ind)))
            } else {
                Err((annotation.start, annotation.end))
            }
        }

        E::I64(_, Some(ind)) => Ok(Type::I64(Some(ind.clone()))),

        _ => Err((annotation.start, annotation.end)),
    }
}

pub fn populate_annotation(
    t: &Type,
    curr_forall_var: &mut Option<&mut usize>,
    type_vars: &[String],
) -> Result<Type, Error> {
    match t {
        Type::Unknown => Ok(Type::Unknown),
        Type::Void => Ok(Type::Void),
        Type::Bool => Ok(Type::Bool),
        Type::F64 => Ok(Type::F64),
        Type::I64(i) => Ok(Type::I64(i.clone())),
        Type::ForAll(_) => {
            if let Some(curr_forall_var) = curr_forall_var.as_deref_mut() {
                *curr_forall_var += 1;
                Ok(Type::ForAll(*curr_forall_var - 1))
            } else {
                Err(Error::new(
                    ErrorType::Type,
                    "cannot use wildcard here".into(),
                    0,
                    0,
                ))
            }
        }
        Type::TypeVar(name) => {
            if type_vars.contains(name) {
                Ok(Type::TypeVar(name.clone()))
            } else {
                Err(Error::new(
                    ErrorType::Type,
                    format!("type \"{name}\" not found in context"),
                    0,
                    0,
                ))
            }
        }
        Type::Tuple(inner) => {
            let mut populated_inner = vec![];
            for t in inner {
                populated_inner.push(populate_annotation(t, curr_forall_var, type_vars)?);
            }
            Ok(Type::Tuple(populated_inner))
        }
        Type::Function(_, args, return_type) => {
            let mut populated_args = vec![];
            for t in args {
                populated_args.push(populate_annotation(t, curr_forall_var, type_vars)?);
            }

            let populated_return = populate_annotation(return_type, curr_forall_var, type_vars)?;

            Ok(Type::Function(
                vec![],
                populated_args,
                Box::new(populated_return),
            ))
        }
    }
}

fn initialize_expression_types(
    expression: &mut Expression,
    curr_forall_var: &mut usize,
    context: &Scope<Type>,
    type_vars: &[String],
    errors: &mut Vec<Error>,
) {
    match &mut expression.data {
        E::Bool(_) => expression.t = Type::Bool,
        E::F64(_) => expression.t = Type::F64,
        E::I64(_, ind) => expression.t = Type::I64(ind.clone()),
        E::UnaryOp(_, inner) => {
            *curr_forall_var += 1;
            expression.t = Type::ForAll(*curr_forall_var - 1);

            initialize_expression_types(&mut *inner, curr_forall_var, context, type_vars, errors);
        }
        E::BinaryOp(lhs, _, rhs) => {
            *curr_forall_var += 1;
            expression.t = Type::ForAll(*curr_forall_var - 1);

            initialize_expression_types(&mut *lhs, curr_forall_var, context, type_vars, errors);

            initialize_expression_types(&mut *rhs, curr_forall_var, context, type_vars, errors);
        }
        E::Accessor(lhs, rhs) => {
            *curr_forall_var += 1;
            expression.t = Type::ForAll(*curr_forall_var - 1);

            initialize_expression_types(&mut *lhs, curr_forall_var, context, type_vars, errors);

            initialize_expression_types(&mut *rhs, curr_forall_var, context, type_vars, errors);
        }
        E::Tuple(inner) => {
            *curr_forall_var += 1;
            expression.t = Type::ForAll(*curr_forall_var - 1);
            for e in inner {
                initialize_expression_types(e, curr_forall_var, context, type_vars, errors);
            }
        }
        E::FnCall(caller, args) => {
            *curr_forall_var += 1;
            expression.t = Type::ForAll(*curr_forall_var - 1);

            initialize_expression_types(&mut *caller, curr_forall_var, context, type_vars, errors);

            for e in args {
                initialize_expression_types(e, curr_forall_var, context, type_vars, errors);
            }
        }
        E::Ident(name, type_args) => match context.get(name) {
            Some(t) => {
                expression.t = match t {
                    Type::Function(function_type_vars, _, _) => {
                        let mut subs: Vec<_> = (*curr_forall_var
                            ..(*curr_forall_var + function_type_vars.len()))
                            .map(Type::ForAll)
                            .collect();
                        *curr_forall_var += function_type_vars.len();

                        if let Some(type_args) = type_args {
                            if function_type_vars.len() < type_args.len() {
                                errors.push(Error::new(
                                    ErrorType::Type,
                                    format!(
                                        "expected {} maximum type arguments; found {}",
                                        function_type_vars.len(),
                                        type_args.len()
                                    ),
                                    type_args.first().unwrap().start,
                                    type_args.last().unwrap().end,
                                ));
                            } else {
                                let mut populated_type_args = vec![];
                                for type_arg in type_args.iter_mut() {
                                    populated_type_args.push(
                                        match populate_annotation(
                                            &type_arg.inner,
                                            &mut Some(curr_forall_var),
                                            type_vars,
                                        ) {
                                            Ok(t) => t,
                                            Err(mut error) => {
                                                error.start = type_arg.start;
                                                error.end = type_arg.end;
                                                errors.push(error);
                                                Type::Unknown
                                            }
                                        },
                                    );
                                }

                                subs[..populated_type_args.len()]
                                    .clone_from_slice(&populated_type_args[..]);
                            }
                        }

                        t.instantiate_fn(function_type_vars, &subs)
                    }
                    _ => {
                        if type_args.is_some() {
                            errors.push(Error::new(
                                ErrorType::Type,
                                "type arguments only allowed for functions".into(),
                                expression.start,
                                expression.end,
                            ));
                            Type::Unknown
                        } else {
                            t.clone()
                        }
                    }
                };
            }
            None => {
                errors.push(Error::new(
                    ErrorType::UnboundIdentifier,
                    format!("identifier \"{name}\" not found in scope"),
                    expression.start,
                    expression.end,
                ));
                expression.t = Type::Unknown;
            }
        },
        E::OpenTuple { .. } => panic!(),
    }
}

pub fn initialize_statement_types(
    statement: &mut Statement,
    curr_forall_var: &mut usize,
    context: &mut Scope<Type>,
    errors: &mut Vec<Error>,
    type_vars: &[String],
) {
    match &mut statement.data {
        S::Print(val) => {
            initialize_expression_types(val, curr_forall_var, context, type_vars, errors);
        }
        S::Let(name, ann, val) => {
            initialize_expression_types(val, curr_forall_var, context, type_vars, errors);
            *ann = ann.as_ref().map(|ann| {
                match populate_annotation(&ann.inner, &mut Some(curr_forall_var), type_vars) {
                    Ok(t) => Positioned::new(ann.start, t, ann.end),
                    Err(mut error) => {
                        error.start = ann.start;
                        error.end = ann.end;
                        errors.push(error);
                        Positioned::new(ann.start, Type::Unknown, ann.end)
                    }
                }
            });

            context.insert(name.inner.clone(), val.t.clone());
        }
        S::Assign(place, val) => {
            initialize_expression_types(val, curr_forall_var, context, type_vars, errors);

            if context.get(&place.inner).is_none() {
                errors.push(Error::new(
                    ErrorType::UnboundIdentifier,
                    format!("identifier \"{}\" not found in scope", place.inner),
                    place.start,
                    place.end,
                ));
            } else if context.is_global(&place.inner) {
                errors.push(Error::new(
                    ErrorType::Mutation,
                    format!("\"{}\" is global and cannot be mutated", place.inner),
                    place.start,
                    place.end,
                ));
            }
        }
        S::If(cond, true_block, false_block) => {
            initialize_expression_types(cond, curr_forall_var, context, type_vars, errors);

            context.push();
            for stmt in true_block {
                initialize_statement_types(stmt, curr_forall_var, context, errors, type_vars);
            }
            context.pop();
            if let Some(false_block) = false_block {
                context.push();
                for stmt in false_block {
                    initialize_statement_types(stmt, curr_forall_var, context, errors, type_vars);
                }
                context.pop();
            }
        }
        S::For(iterator, from, to, block) => {
            initialize_expression_types(from, curr_forall_var, context, type_vars, errors);
            initialize_expression_types(to, curr_forall_var, context, type_vars, errors);

            context.push();
            context.insert(iterator.inner.clone(), Type::I64(None));
            for stmt in block {
                initialize_statement_types(stmt, curr_forall_var, context, errors, type_vars);
            }
            context.pop();
        }
        S::Return(val) => {
            if let Some(val) = val {
                initialize_expression_types(val, curr_forall_var, context, type_vars, errors);
            }
        }
    }
}
