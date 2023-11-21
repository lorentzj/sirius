use super::types::Type;
use super::ScopeEntry;
use crate::error::{Error, ErrorType};
use crate::scope::Scope;
use crate::solver::poly::Poly;
use crate::solver::rational::Rat;
use std::collections::HashMap;
use std::rc::Rc;

use crate::lexer::Op;
use crate::parser::{Block, Expression, Positioned, Statement, E, S};

pub fn annotation_type(annotation: &Expression) -> Result<Type, (usize, usize)> {
    match &annotation.data {
        E::Ident { name, type_args } => {
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

        E::BinaryOp {
            lhs:
                box Expression {
                    data: E::Tuple(inner),
                    ..
                },
            op: Op::Apply,
            rhs,
        } => {
            let mut args = vec![];
            for expression in inner {
                args.push(annotation_type(expression)?);
            }

            let return_type = annotation_type(rhs)?;

            Ok(Type::Function(vec![], args, Box::new(return_type)))
        }

        E::BinaryOp {
            lhs,
            op: Op::Apply,
            rhs,
        } => {
            let args = vec![annotation_type(lhs)?];
            let return_type = annotation_type(rhs)?;

            Ok(Type::Function(vec![], args, Box::new(return_type)))
        }

        E::BinaryOp {
            lhs,
            op: Op::Add,
            rhs,
        } => {
            let lhs = annotation_type(lhs)?;
            let rhs = annotation_type(rhs)?;

            if let (Type::I64(Some(lhs_ind)), Type::I64(Some(rhs_ind))) = (lhs, rhs) {
                Ok(Type::I64(Some(lhs_ind + rhs_ind)))
            } else {
                Err((annotation.start, annotation.end))
            }
        }

        E::BinaryOp {
            lhs,
            op: Op::Sub,
            rhs,
        } => {
            let lhs = annotation_type(lhs)?;
            let rhs = annotation_type(rhs)?;

            if let (Type::I64(Some(lhs_ind)), Type::I64(Some(rhs_ind))) = (lhs, rhs) {
                Ok(Type::I64(Some(lhs_ind - rhs_ind)))
            } else {
                Err((annotation.start, annotation.end))
            }
        }

        E::BinaryOp {
            lhs,
            op: Op::Mul,
            rhs,
        } => {
            let lhs = annotation_type(lhs)?;
            let rhs = annotation_type(rhs)?;

            if let (Type::I64(Some(lhs_ind)), Type::I64(Some(rhs_ind))) = (lhs, rhs) {
                Ok(Type::I64(Some(lhs_ind * rhs_ind)))
            } else {
                Err((annotation.start, annotation.end))
            }
        }

        E::I64(val) => Ok(Type::I64(Some(Poly::constant(Rat::from(*val))))),

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
    context: &Scope<ScopeEntry>,
    type_vars: &[String],
    errors: &mut Vec<Error>,
    highlight_map: &mut HashMap<usize, Vec<usize>>,
) {
    match &mut expression.data {
        E::Bool(_) => expression.t = Rc::new(Type::Bool),
        E::F64(_) => expression.t = Rc::new(Type::F64),
        E::I64(val) => expression.t = Rc::new(Type::I64(Some(Poly::constant(Rat::from(*val))))),
        E::UnaryOp { inner, .. } => {
            *curr_forall_var += 1;
            expression.t = Rc::new(Type::ForAll(*curr_forall_var - 1));

            initialize_expression_types(
                &mut *inner,
                curr_forall_var,
                context,
                type_vars,
                errors,
                highlight_map,
            );
        }
        E::BinaryOp { lhs, rhs, .. } => {
            *curr_forall_var += 1;
            expression.t = Rc::new(Type::ForAll(*curr_forall_var - 1));

            initialize_expression_types(
                &mut *lhs,
                curr_forall_var,
                context,
                type_vars,
                errors,
                highlight_map,
            );

            initialize_expression_types(
                &mut *rhs,
                curr_forall_var,
                context,
                type_vars,
                errors,
                highlight_map,
            );
        }
        E::Accessor { target, index } => {
            *curr_forall_var += 1;
            expression.t = Rc::new(Type::ForAll(*curr_forall_var - 1));

            initialize_expression_types(
                &mut *target,
                curr_forall_var,
                context,
                type_vars,
                errors,
                highlight_map,
            );

            initialize_expression_types(
                &mut *index,
                curr_forall_var,
                context,
                type_vars,
                errors,
                highlight_map,
            );
        }
        E::Tuple(inner) => {
            *curr_forall_var += 1;
            expression.t = Rc::new(Type::ForAll(*curr_forall_var - 1));
            for e in inner {
                initialize_expression_types(
                    e,
                    curr_forall_var,
                    context,
                    type_vars,
                    errors,
                    highlight_map,
                );
            }
        }
        E::FnCall { func, args } => {
            *curr_forall_var += 1;
            expression.t = Rc::new(Type::ForAll(*curr_forall_var - 1));

            initialize_expression_types(
                &mut *func,
                curr_forall_var,
                context,
                type_vars,
                errors,
                highlight_map,
            );

            for e in args {
                initialize_expression_types(
                    e,
                    curr_forall_var,
                    context,
                    type_vars,
                    errors,
                    highlight_map,
                );
            }
        }
        E::Ident { name, type_args } => match context.get(name) {
            Some(ScopeEntry { t, decl_site, .. }) => {
                if let Some(decl_site) = decl_site {
                    highlight_map.insert(expression.start, vec![expression.start, *decl_site]);
                }

                expression.t = match t.as_ref() {
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

                        Rc::new(t.instantiate_fn(function_type_vars, &subs))
                    }
                    _ => {
                        if type_args.is_some() {
                            errors.push(Error::new(
                                ErrorType::Type,
                                "type arguments only allowed for functions".into(),
                                expression.start,
                                expression.end,
                            ));
                            Rc::new(Type::Unknown)
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
                expression.t = Rc::new(Type::Unknown);
            }
        },
        E::OpenTuple { .. } => unreachable!(),
    }
}

pub fn initialize_statement_types(
    statement: &mut Statement,
    curr_forall_var: &mut usize,
    curr_ind_forall_var: &mut usize,
    context: &mut Scope<ScopeEntry>,
    errors: &mut Vec<Error>,
    type_vars: &[String],
    highlight_map: &mut HashMap<usize, Vec<usize>>,
) {
    match &mut statement.data {
        S::Print(val) => {
            initialize_expression_types(
                val,
                curr_forall_var,
                context,
                type_vars,
                errors,
                highlight_map,
            );
        }
        S::Let {
            name,
            mutable,
            annotation,
            bound_type,
            value,
        } => {
            highlight_map.insert(name.start, vec![name.start]);

            initialize_expression_types(
                value,
                curr_forall_var,
                context,
                type_vars,
                errors,
                highlight_map,
            );

            *curr_forall_var += 1;
            *bound_type = Rc::new(Type::ForAll(*curr_forall_var - 1));

            *annotation = annotation.as_ref().map(|annotation| {
                match populate_annotation(&annotation.inner, &mut Some(curr_forall_var), type_vars)
                {
                    Ok(t) => Positioned::new(annotation.start, Rc::new(t), annotation.end),
                    Err(mut error) => {
                        error.start = annotation.start;
                        error.end = annotation.end;
                        errors.push(error);
                        Positioned::new(annotation.start, Rc::new(Type::Unknown), annotation.end)
                    }
                }
            });

            context.insert(
                name.inner.clone(),
                ScopeEntry {
                    t: bound_type.clone(),
                    mutable: *mutable,
                    decl_site: Some(name.start),
                },
            );
        }
        S::Assign { place, value } => {
            initialize_expression_types(
                value,
                curr_forall_var,
                context,
                type_vars,
                errors,
                highlight_map,
            );
            match context.get(&place.inner) {
                Some(ScopeEntry { mutable, .. }) => {
                    if !mutable {
                        errors.push(Error::new(
                            ErrorType::Mutation,
                            format!("\"{}\" is immutable", place.inner),
                            place.start,
                            place.end,
                        ));
                    }
                }
                None => {
                    errors.push(Error::new(
                        ErrorType::UnboundIdentifier,
                        format!("identifier \"{}\" not found in scope", place.inner),
                        place.start,
                        place.end,
                    ));
                }
            }
        }
        S::If {
            condition,
            true_inner:
                Block {
                    statements: true_block,
                    ..
                },
            false_inner,
            ..
        } => {
            initialize_expression_types(
                condition,
                curr_forall_var,
                context,
                type_vars,
                errors,
                highlight_map,
            );

            context.push();
            for stmt in true_block.iter_mut() {
                initialize_statement_types(
                    stmt,
                    curr_forall_var,
                    curr_ind_forall_var,
                    context,
                    errors,
                    type_vars,
                    highlight_map,
                );
            }
            context.pop();
            if let Some(Block {
                statements: false_block,
                ..
            }) = false_inner
            {
                context.push();
                for stmt in false_block.iter_mut() {
                    initialize_statement_types(
                        stmt,
                        curr_forall_var,
                        curr_ind_forall_var,
                        context,
                        errors,
                        type_vars,
                        highlight_map,
                    );
                }
                context.pop();
            }
        }
        S::For {
            iterator,
            iterator_type,
            from,
            to,
            inner: Block {
                statements: block, ..
            },
            ..
        } => {
            highlight_map.insert(iterator.start, vec![iterator.start]);

            initialize_expression_types(
                from,
                curr_forall_var,
                context,
                type_vars,
                errors,
                highlight_map,
            );
            initialize_expression_types(
                to,
                curr_forall_var,
                context,
                type_vars,
                errors,
                highlight_map,
            );

            *iterator_type = Type::new_free_ind(curr_ind_forall_var, true);

            context.push();
            context.insert(
                iterator.inner.clone(),
                ScopeEntry {
                    t: Rc::new(iterator_type.clone()),
                    mutable: false,
                    decl_site: Some(iterator.start),
                },
            );
            for stmt in block.iter_mut() {
                initialize_statement_types(
                    stmt,
                    curr_forall_var,
                    curr_ind_forall_var,
                    context,
                    errors,
                    type_vars,
                    highlight_map,
                );
            }
            context.pop();
        }
        S::Return(val) => {
            if let Some(val) = val {
                initialize_expression_types(
                    val,
                    curr_forall_var,
                    context,
                    type_vars,
                    errors,
                    highlight_map,
                );
            }
        }
    }
}
