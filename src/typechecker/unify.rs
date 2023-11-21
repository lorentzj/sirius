use std::rc::Rc;

use super::arithmetic::{arith_coerce, is_arith};
use super::eq_classes::EqClasses;
use super::equality::equality_check;
use super::types::Type;
use super::{Constraint, ScopeEntry};
use crate::error::{Error, ErrorType};
use crate::lexer::Op;
use crate::parser::{Block, Expression, Statement, UnaryOp, E, S};
use crate::scope::Scope;
use crate::solver::poly::Poly;
use crate::solver::rational::Rat;

fn unify_expression(
    expression: &Expression,
    eqc: &mut EqClasses,
    context: &Scope<ScopeEntry>,
    constraints: &mut Vec<Constraint>,
) -> Result<(), Error> {
    eqc.add_single(expression.start, &expression.t, expression.end);

    match &expression.data {
        E::Bool(_) => Ok(()),
        E::F64(_) => Ok(()),
        E::I64(_) => Ok(()),
        E::Accessor { .. } => Err(Error::new(
            ErrorType::NotImplemented,
            "array access not implemented yet".into(),
            expression.start,
            expression.end,
        )),
        E::Ident { name, .. } => match context.get(name) {
            Some(ScopeEntry { t: var_t, .. }) => match var_t.as_ref() {
                Type::Function(_, _, _) => Ok(()),
                _ => {
                    eqc.add(expression.start, &expression.t, var_t, expression.end);

                    Ok(())
                }
            },
            None => unreachable!(),
        },
        E::UnaryOp { op, inner } => {
            unify_expression(inner, eqc, context, constraints)?;

            if inner.t.forall_vars().is_empty() {
                match op {
                    UnaryOp::ArithNeg => match inner.t.as_ref() {
                        Type::F64 => eqc.add_owned(
                            expression.start,
                            &expression.t,
                            Type::F64,
                            expression.end,
                        ),
                        Type::I64(None) => eqc.add_owned(
                            expression.start,
                            &expression.t,
                            Type::I64(None),
                            expression.end,
                        ),
                        Type::I64(Some(ind)) => {
                            let new_ind = ind.clone() * Poly::constant(Rat::from(-1));
                            EqClasses::add_owned(
                                eqc,
                                expression.start,
                                &expression.t,
                                Type::I64(Some(new_ind)),
                                expression.end,
                            );
                        }
                        Type::Unknown => return Ok(()),
                        t => {
                            return Err(Error::new(
                                ErrorType::Type,
                                format!("cannot apply arithmetic negation to type \"{t:?}\""),
                                expression.start,
                                expression.end,
                            ))
                        }
                    },
                    UnaryOp::BoolNeg => match inner.t.as_ref() {
                        Type::Bool => eqc.add_owned(
                            expression.start,
                            &expression.t,
                            Type::Bool,
                            expression.end,
                        ),
                        Type::Unknown => return Ok(()),
                        t => {
                            return Err(Error::new(
                                ErrorType::Type,
                                format!("cannot apply boolean negation to type \"{t:?}\""),
                                expression.start,
                                expression.end,
                            ))
                        }
                    },
                    UnaryOp::Tick => {
                        return Err(Error::new(
                            ErrorType::NotImplemented,
                            "tick operator not implemented yet".into(),
                            expression.start,
                            expression.end,
                        ))
                    }
                }
            }
            Ok(())
        }
        E::Tuple(inner) => {
            let inner_types = inner.iter().map(|e| e.t.as_ref().clone()).collect();
            for e in inner {
                unify_expression(e, eqc, context, constraints)?;
            }

            let tuple_t = Type::Tuple(inner_types);

            eqc.add_owned(expression.start, &expression.t, tuple_t, expression.end);

            Ok(())
        }
        E::BinaryOp { lhs, op, rhs } => {
            unify_expression(lhs, eqc, context, constraints)?;
            unify_expression(rhs, eqc, context, constraints)?;

            if lhs.t.forall_vars().is_empty() && rhs.t.forall_vars().is_empty() {
                if is_arith(op) {
                    let arith_result =
                        arith_coerce(expression.start, &lhs.t, op, &rhs.t, expression.end)?;
                    eqc.add_owned(
                        expression.start,
                        &expression.t,
                        arith_result,
                        expression.end,
                    );
                } else {
                    match op {
                        Op::Equal | Op::NotEqual => {
                            match equality_check(lhs.start, &lhs.t, &rhs.t, rhs.end) {
                                Ok(mut cs) => {
                                    if op == &Op::NotEqual {
                                        cs = cs.iter().map(|c| c.negate()).collect();
                                    }
                                    constraints.append(&mut cs);

                                    eqc.add_owned(
                                        expression.start,
                                        &expression.t,
                                        Type::Bool,
                                        expression.end,
                                    );
                                }
                                Err(msg) => {
                                    return Err(Error::new(
                                        ErrorType::Type,
                                        msg,
                                        expression.start,
                                        expression.end,
                                    ))
                                }
                            }
                        }

                        Op::Dot => match lhs.t.as_ref() {
                            Type::Tuple(lhs_inner) => match rhs.data {
                                E::I64(val) => {
                                    if val < 0 || val as usize > lhs_inner.len() {
                                        return Err(Error::new(
                                            ErrorType::Type,
                                            format!(
                                                "\"{val}\" out of bounds of \"{}\"-tuple",
                                                lhs_inner.len()
                                            ),
                                            expression.start,
                                            expression.end,
                                        ));
                                    } else {
                                        eqc.add_owned(
                                            expression.start,
                                            &expression.t,
                                            lhs_inner[val as usize].clone(),
                                            expression.end,
                                        );
                                    }
                                }
                                _ => {
                                    return Err(Error::new(
                                        ErrorType::Type,
                                        "tuple members must be literal integers".into(),
                                        expression.start,
                                        expression.end,
                                    ))
                                }
                            },
                            Type::ForAll(_) => (),
                            _ => {
                                return Err(Error::new(
                                    ErrorType::Type,
                                    format!("cannot access member of \"{:?}\"", lhs.t),
                                    expression.start,
                                    expression.end,
                                ))
                            }
                        },
                        Op::And | Op::Or => {
                            if lhs.t.as_ref() != &Type::Bool {
                                return Err(Error::new(
                                    ErrorType::Type,
                                    format!(
                                        "cannot apply boolean operator to type \"{:?}\"",
                                        lhs.t
                                    ),
                                    expression.start,
                                    expression.end,
                                ));
                            } else if rhs.t.as_ref() != &Type::Bool {
                                return Err(Error::new(
                                    ErrorType::Type,
                                    format!(
                                        "cannot apply boolean operator to type \"{:?}\"",
                                        rhs.t
                                    ),
                                    expression.start,
                                    expression.end,
                                ));
                            } else {
                                eqc.add_owned(
                                    expression.start,
                                    &expression.t,
                                    Type::Bool,
                                    expression.end,
                                );
                            }
                        }
                        Op::Apply => {
                            return Err(Error::new(
                                ErrorType::Type,
                                "cannot use function operator in expression".into(),
                                expression.start,
                                expression.end,
                            ))
                        }
                        _ => unreachable!(),
                    }
                }
            }

            Ok(())
        }
        E::FnCall { func, args } => {
            unify_expression(func, eqc, context, constraints)?;

            for e in args.iter() {
                unify_expression(e, eqc, context, constraints)?;
            }

            let arg_types: Vec<_> = args.iter().map(|e| e.t.clone()).collect();

            match func.t.as_ref() {
                Type::Function(_, i, o) => {
                    if i.len() != arg_types.len() {
                        return Err(Error::new(
                            ErrorType::Type,
                            format!(
                                "expected {} arguments to function; found {}",
                                i.len(),
                                arg_types.len()
                            ),
                            expression.start,
                            expression.end,
                        ));
                    } else {
                        for ((given_arg, expected_arg), arg_expr) in
                            arg_types.into_iter().zip(i).zip(args)
                        {
                            eqc.add_can_demote_or_promote_owned(
                                arg_expr.start,
                                &given_arg,
                                expected_arg.clone(),
                                arg_expr.end,
                            );
                        }
                    }

                    eqc.add_owned(expression.start, &expression.t, *o.clone(), expression.end);
                }
                Type::ForAll(_) => (),
                t => {
                    return Err(Error::new(
                        ErrorType::Type,
                        format!("cannot call type \"{t:?}\" as function"),
                        func.start,
                        func.end,
                    ))
                }
            }

            Ok(())
        }
        E::OpenTuple(_) => unreachable!(),
    }
}

pub struct UnificationResult {
    pub any_changes: bool,
    pub eq_classes: Vec<EqClasses>,
}

pub fn unify(
    block: &mut [Statement],
    mut context: Scope<ScopeEntry>,
    return_type: &Rc<Type>,
    errors: &mut Vec<Error>,
    curr_ind_forall_var: &mut usize,
    constraints: &mut Vec<Constraint>,
) -> UnificationResult {
    let mut result = UnificationResult {
        any_changes: false,
        eq_classes: vec![EqClasses::new()],
    };

    context.push();
    for statement in block {
        match &mut statement.data {
            S::Print(val) => {
                match unify_expression(val, &mut result.eq_classes[0], &context, constraints) {
                    Ok(()) => (),
                    Err(error) => errors.push(error),
                }
            }
            S::Assign { place, value } => {
                match unify_expression(value, &mut result.eq_classes[0], &context, constraints) {
                    Ok(()) => (),
                    Err(error) => errors.push(error),
                }

                match context.get(&place.inner) {
                    Some(ScopeEntry { t, .. }) => {
                        result.eq_classes[0].add(value.start, &value.t, t, value.end)
                    }
                    None => unreachable!(),
                }
            }
            S::Let {
                name,
                mutable,
                annotation,
                bound_type,
                value,
            } => {
                result.eq_classes[0].add_single(value.start, bound_type, value.end);

                match unify_expression(value, &mut result.eq_classes[0], &context, constraints) {
                    Ok(()) => (),
                    Err(error) => errors.push(error),
                }

                if *mutable {
                    result.eq_classes[0].add_must_demote(
                        value.start,
                        bound_type,
                        &value.t,
                        value.end,
                    );
                } else {
                    result.eq_classes[0].add_must_promote(
                        value.start,
                        bound_type,
                        &value.t,
                        value.end,
                    );
                }

                if let Some(annotation) = annotation {
                    result.eq_classes[0].add(
                        annotation.start,
                        bound_type,
                        &annotation.inner,
                        annotation.end,
                    );
                }

                context.insert(
                    name.inner.clone(),
                    ScopeEntry {
                        t: bound_type.clone(),
                        mutable: *mutable,
                        decl_site: Some(name.start),
                    },
                );
            }
            S::Return(val) => {
                if let Some(val) = val {
                    match unify_expression(val, &mut result.eq_classes[0], &context, constraints) {
                        Ok(()) => (),
                        Err(error) => errors.push(error),
                    }

                    result.eq_classes[0].add_can_demote_or_promote(
                        val.start,
                        &val.t,
                        return_type,
                        val.end,
                    );
                } else {
                    result.eq_classes[0].add_owned(
                        statement.start,
                        return_type,
                        Type::Void,
                        statement.end,
                    );
                }
            }
            S::If {
                condition,
                true_inner:
                    Block {
                        statements: true_inner,
                        post_constraints: true_inner_constraints,
                    },
                false_inner,
                ..
            } => {
                match unify_expression(condition, &mut result.eq_classes[0], &context, constraints)
                {
                    Ok(()) => (),
                    Err(error) => errors.push(error),
                }

                result.eq_classes[0].add_owned(
                    condition.start,
                    &condition.t,
                    Type::Bool,
                    condition.end,
                );

                let mut inner_result = unify(
                    true_inner,
                    context.clone(),
                    return_type,
                    errors,
                    curr_ind_forall_var,
                    true_inner_constraints,
                );

                result.any_changes |= inner_result.any_changes;
                result.eq_classes.append(&mut inner_result.eq_classes);

                if let Some(Block {
                    statements: false_inner,
                    post_constraints: false_inner_constraints,
                }) = false_inner
                {
                    let mut inner_result = unify(
                        false_inner,
                        context.clone(),
                        return_type,
                        errors,
                        curr_ind_forall_var,
                        false_inner_constraints,
                    );

                    result.any_changes |= inner_result.any_changes;
                    result.eq_classes.append(&mut inner_result.eq_classes);
                }
            }
            S::For {
                iterator,
                iterator_type,
                from,
                to,
                inner:
                    Block {
                        statements: inner,
                        post_constraints: inner_constraints,
                    },
                ..
            } => {
                match unify_expression(from, &mut result.eq_classes[0], &context, constraints) {
                    Ok(()) => (),
                    Err(error) => errors.push(error),
                }

                match unify_expression(to, &mut result.eq_classes[0], &context, constraints) {
                    Ok(()) => (),
                    Err(error) => errors.push(error),
                }

                result.eq_classes[0].add_can_demote_or_promote_owned(
                    from.start,
                    &from.t,
                    Type::I64(None),
                    from.end,
                );
                result.eq_classes[0].add_can_demote_or_promote_owned(
                    to.start,
                    &to.t,
                    Type::I64(None),
                    to.end,
                );

                context.push();
                context.insert(
                    iterator.inner.clone(),
                    ScopeEntry {
                        t: Rc::new(iterator_type.clone()),
                        mutable: false,
                        decl_site: Some(iterator.start),
                    },
                );

                let mut inner_result = unify(
                    inner,
                    context.clone(),
                    return_type,
                    errors,
                    curr_ind_forall_var,
                    inner_constraints,
                );

                result.any_changes |= inner_result.any_changes;
                result.eq_classes.append(&mut inner_result.eq_classes);

                context.pop();
            }
        }
    }
    context.pop();

    match result.eq_classes[0].generate(curr_ind_forall_var) {
        Ok((block_changes, mut cs)) => {
            constraints.append(&mut cs);
            result.any_changes |= block_changes;
            result
        }
        Err(error) => {
            errors.push(error);
            result
        }
    }
}
