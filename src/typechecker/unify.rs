use super::equality::equality_check;
use super::number_coersion::{arith_coerce, is_arith};
use super::types::Substitutions;
use super::types::{Constraint, Type};
use crate::error::{Error, ErrorType};
use crate::lexer::Op;
use crate::parser::{Expression, Statement, UnaryOp, E, S};
use crate::scope::Scope;

fn unify_expression(
    expression: &mut Expression,
    context: &Scope<Type>,
    curr_forall_var: &mut usize,
    constraints: &mut Vec<Constraint>,
) -> Result<Substitutions, Error> {
    match &mut expression.data {
        E::Bool(_) => Ok(Substitutions::new()),
        E::F64(_) => Ok(Substitutions::new()),
        E::I64(_, _) => Ok(Substitutions::new()),
        E::Accessor(_, _) => Err(Error::new(
            ErrorType::NotImplemented,
            "array access not implemented yet".into(),
            expression.start,
            expression.end,
        )),
        E::Ident(name, _) => match context.get(name) {
            Some(var_t) => match var_t {
                Type::Function(_, _, _) => Ok(Substitutions::new()),
                _ => match expression.t.unify_update_vals(var_t) {
                    Some((subs, mut cs)) => {
                        Constraint::apply_pos(&mut cs, expression.start, expression.end);
                        constraints.extend(cs.into_iter());
                        Ok(subs)
                    }
                    None => Err(Error::new(
                        ErrorType::Type,
                        format!(
                            "cannot unify types \"{:?}\" and \"{var_t:?}\"",
                            expression.t
                        ),
                        expression.start,
                        expression.end,
                    )),
                },
            },
            None => panic!(),
        },
        E::UnaryOp(op, inner) => {
            let mut subs = unify_expression(inner, context, curr_forall_var, constraints)?;

            if inner.t.forall_vars().is_empty() {
                match op {
                    UnaryOp::ArithNeg => match &inner.t {
                        Type::F64 => match Type::F64.unify(&expression.t) {
                            Some((s, mut cs)) => {
                                cs.extend(subs.extend(expression.start, s, expression.end)?);
                                Constraint::apply_pos(&mut cs, expression.start, expression.end);
                                constraints.extend(cs);
                            }
                            None => {
                                return Err(Error::new(
                                    ErrorType::Type,
                                    format!(
                                        "cannot unify types \"{:?}\" and \"{:?}\"",
                                        expression.t,
                                        Type::F64
                                    ),
                                    expression.start,
                                    expression.end,
                                ))
                            }
                        },
                        Type::I64(_) => match Type::I64(None).unify(&expression.t) {
                            Some((s, mut cs)) => {
                                cs.extend(subs.extend(expression.start, s, expression.end)?);
                                Constraint::apply_pos(&mut cs, expression.start, expression.end);
                                constraints.extend(cs);
                            }
                            None => {
                                return Err(Error::new(
                                    ErrorType::Type,
                                    format!(
                                        "cannot unify types \"{:?}\" and \"{:?}\"",
                                        expression.t,
                                        Type::I64(None)
                                    ),
                                    expression.start,
                                    expression.end,
                                ))
                            }
                        },
                        t => {
                            return Err(Error::new(
                                ErrorType::Type,
                                format!("cannot apply arithmetic negation to type \"{t:?}\""),
                                expression.start,
                                expression.end,
                            ))
                        }
                    },
                    UnaryOp::BoolNeg => match &inner.t {
                        Type::Bool => match Type::Bool.unify(&expression.t) {
                            Some((s, mut cs)) => {
                                cs.extend(subs.extend(expression.start, s, expression.end)?);
                                Constraint::apply_pos(&mut cs, expression.start, expression.end);
                                constraints.extend(cs);
                            }
                            None => {
                                return Err(Error::new(
                                    ErrorType::Type,
                                    format!(
                                        "cannot unify types \"{:?}\" and \"{:?}\"",
                                        expression.t,
                                        Type::Bool
                                    ),
                                    expression.start,
                                    expression.end,
                                ))
                            }
                        },
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

            Ok(subs)
        }
        E::Tuple(inner) => {
            let mut subs = Substitutions::new();
            let inner_types = inner.iter().map(|e| e.t.clone()).collect();
            for e in inner {
                let mut cs = subs.extend(
                    expression.start,
                    unify_expression(e, context, curr_forall_var, constraints)?,
                    expression.end,
                )?;
                Constraint::apply_pos(&mut cs, expression.start, expression.end);
                constraints.extend(cs);
            }

            let tuple_t = Type::Tuple(inner_types);

            match expression.t.unify(&tuple_t) {
                Some((s, mut cs)) => {
                    cs.extend(subs.extend(expression.start, s, expression.end)?);
                    Constraint::apply_pos(&mut cs, expression.start, expression.end);
                    constraints.extend(cs);
                }
                None => {
                    return Err(Error::new(
                        ErrorType::Type,
                        format!(
                            "cannot unify types \"{:?}\" and \"{tuple_t:?}\"",
                            expression.t
                        ),
                        expression.start,
                        expression.end,
                    ))
                }
            }

            Ok(subs)
        }
        E::BinaryOp(lhs, op, rhs) => {
            let lhs_subs = unify_expression(lhs, context, curr_forall_var, constraints)?;
            let rhs_subs = unify_expression(rhs, context, curr_forall_var, constraints)?;

            let mut subs = lhs_subs;
            let mut cs = subs.extend(rhs.start, rhs_subs, rhs.end)?;
            Constraint::apply_pos(&mut cs, rhs.start, rhs.end);
            constraints.extend(cs);

            if lhs.t.forall_vars().is_empty() && rhs.t.forall_vars().is_empty() {
                if is_arith(op) {
                    let arith_result =
                        arith_coerce(expression.start, &lhs.t, op, &rhs.t, expression.end)?;

                    match expression.t.unify(&arith_result) {
                        Some((s, mut cs)) => {
                            cs.extend(subs.extend(expression.start, s, expression.end)?);
                            Constraint::apply_pos(&mut cs, expression.start, expression.end);
                            constraints.extend(cs);
                        }
                        None => {
                            return Err(Error::new(
                                ErrorType::Type,
                                format!(
                                    "cannot unify types \"{:?}\" and \"{arith_result:?}\"",
                                    expression.t
                                ),
                                expression.start,
                                expression.end,
                            ))
                        }
                    }
                } else {
                    match op {
                        Op::Equal | Op::NotEqual => match equality_check(&lhs.t, &rhs.t) {
                            Ok(_) => match expression.t.unify(&Type::Bool) {
                                Some((s, mut cs)) => {
                                    cs.extend(subs.extend(expression.start, s, expression.end)?);
                                    Constraint::apply_pos(
                                        &mut cs,
                                        expression.start,
                                        expression.end,
                                    );
                                    constraints.extend(cs);
                                }
                                None => {
                                    return Err(Error::new(
                                        ErrorType::Type,
                                        format!(
                                            "cannot unify types \"{:?}\" and \"{:?}\"",
                                            expression.t,
                                            Type::Bool
                                        ),
                                        expression.start,
                                        expression.end,
                                    ))
                                }
                            },
                            Err(msg) => {
                                return Err(Error::new(
                                    ErrorType::Type,
                                    msg,
                                    expression.start,
                                    expression.end,
                                ))
                            }
                        },

                        Op::Dot => {
                            match &lhs.t {
                                Type::Tuple(lhs_inner) => {
                                    match rhs.data {
                                        E::I64(val, _) => {
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
                                                match lhs_inner[val as usize].unify(&expression.t) {
                                                    Some((s, mut cs)) => {
                                                        cs.extend(subs.extend(expression.start, s, expression.end)?);
                                                        Constraint::apply_pos(&mut cs, expression.start, expression.end);
                                                        constraints.extend(cs);
                                                    }
                                                    None => return Err(Error::new(
                                                        ErrorType::Type,
                                                        format!(
                                                            "cannot unify types \"{:?}\" and \"{:?}\"",
                                                            expression.t,
                                                            Type::Bool
                                                        ),
                                                        expression.start,
                                                        expression.end,
                                                    ))
                                                }
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
                                    }
                                }
                                Type::ForAll(_) => (),
                                _ => {
                                    return Err(Error::new(
                                        ErrorType::Type,
                                        format!("cannot access member of \"{:?}\"", lhs.t),
                                        expression.start,
                                        expression.end,
                                    ))
                                }
                            }
                        }
                        Op::And | Op::Or => {
                            if lhs.t != Type::Bool {
                                return Err(Error::new(
                                    ErrorType::Type,
                                    format!(
                                        "cannot apply boolean operator to type \"{:?}\"",
                                        lhs.t
                                    ),
                                    expression.start,
                                    expression.end,
                                ));
                            } else if rhs.t != Type::Bool {
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
                                match expression.t.unify(&Type::Bool) {
                                    Some((s, mut cs)) => {
                                        cs.extend(subs.extend(
                                            expression.start,
                                            s,
                                            expression.end,
                                        )?);
                                        Constraint::apply_pos(
                                            &mut cs,
                                            expression.start,
                                            expression.end,
                                        );
                                        constraints.extend(cs);
                                    }
                                    None => {
                                        return Err(Error::new(
                                            ErrorType::Type,
                                            format!(
                                                "cannot unify types \"{:?}\" and \"{:?}\"",
                                                expression.t,
                                                Type::Bool
                                            ),
                                            expression.start,
                                            expression.end,
                                        ))
                                    }
                                }
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
                        _ => panic!(),
                    }
                }
            }

            Ok(subs)
        }
        E::FnCall(caller, args) => {
            let mut subs = unify_expression(caller, context, curr_forall_var, constraints)?;
            let arg_types: Vec<_> = args.iter().map(|e| e.t.clone()).collect();
            for e in args {
                let mut cs = subs.extend(
                    expression.start,
                    unify_expression(e, context, curr_forall_var, constraints)?,
                    expression.end,
                )?;
                Constraint::apply_pos(&mut cs, expression.start, expression.end);
                constraints.extend(cs);
            }

            match &caller.t {
                Type::Function(type_vars, i, o) => {
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
                        for (given_arg, expected_arg) in arg_types.iter().zip(i) {
                            match given_arg.unify(expected_arg) {
                                Some((s, mut cs)) => {
                                    cs.extend(subs.extend(expression.start, s, expression.end)?);
                                    Constraint::apply_pos(&mut cs, expression.start, expression.end);
                                    constraints.extend(cs);
                                },
                                None => {
                                    return Err(Error::new(
                                        ErrorType::Type,
                                        format!("cannot unify types \"{given_arg:?}\" and \"{expected_arg:?}\""),
                                        expression.start,
                                        expression.end,
                                    ))
                                }
                            }
                        }
                    }

                    match o.unify(&expression.t) {
                        Some((s, mut cs)) => {
                            cs.extend(subs.extend(expression.start, s, expression.end)?);
                            Constraint::apply_pos(&mut cs, expression.start, expression.end);
                            constraints.extend(cs);
                        }
                        None => {
                            return Err(Error::new(
                                ErrorType::Type,
                                format!("cannot unify types \"{:?}\" and \"{o:?}\"", expression.t),
                                expression.start,
                                expression.end,
                            ))
                        }
                    }

                    *curr_forall_var += type_vars.len();
                }
                Type::ForAll(_) => (),
                t => {
                    return Err(Error::new(
                        ErrorType::Type,
                        format!("cannot call type \"{t:?}\" as function"),
                        expression.start,
                        expression.end,
                    ))
                }
            }

            Ok(subs)
        }
        E::OpenTuple(_) => panic!(),
    }
}

pub fn unify(
    block: &mut [Statement],
    context: &mut Scope<Type>,
    curr_forall_var: &mut usize,
    return_type: &Type,
    errors: &mut Vec<Error>,
    subs: &mut Substitutions,
    constraints: &mut Vec<Constraint>,
) {
    for statement in block {
        match &mut statement.data {
            S::Print(val) => match unify_expression(val, context, curr_forall_var, constraints) {
                Ok(s) => match subs.extend(val.start, s, val.end) {
                    Ok(mut cs) => {
                        Constraint::apply_pos(&mut cs, val.start, val.end);
                        constraints.extend(cs);
                    }
                    Err(error) => errors.push(error),
                },
                Err(error) => errors.push(error),
            },
            S::Assign(place, val) => match context.get_mut(&place.inner) {
                Some(var_t) => {
                    match var_t.unify_assign(&val.t) {
                        Ok((s, mut cs)) => {
                            println!(
                                "Ok Unify-Assign; new value for var {} is {var_t:?}",
                                place.inner
                            );
                            match subs.extend(val.start, s, val.end) {
                                Ok(css) => cs.extend(css),
                                Err(error) => errors.push(error),
                            }
                            Constraint::apply_pos(&mut cs, val.start, val.end);
                            constraints.extend(cs);
                        }
                        Err(was_strict_error) => {
                            if was_strict_error {
                                errors.push(Error::new(
                                    ErrorType::Type,
                                    "assignment violates strict value declaration".into(),
                                    statement.start,
                                    statement.end,
                                ));
                            } else {
                                errors.push(Error::new(
                                    ErrorType::Type,
                                    format!("cannot unify types \"{var_t:?}\" and \"{:?}\"", val.t),
                                    statement.start,
                                    statement.end,
                                ));
                            }
                        }
                    }

                    match unify_expression(val, context, curr_forall_var, constraints) {
                        Ok(s) => match subs.extend(val.start, s, val.end) {
                            Ok(mut cs) => {
                                Constraint::apply_pos(&mut cs, val.start, val.end);
                                constraints.extend(cs);
                            }
                            Err(error) => errors.push(error),
                        },
                        Err(error) => errors.push(error),
                    }
                }
                None => panic!(),
            },
            S::Let(name, ann, val) => {
                match unify_expression(val, context, curr_forall_var, constraints) {
                    Ok(s) => match subs.extend(val.start, s, val.end) {
                        Ok(mut cs) => {
                            Constraint::apply_pos(&mut cs, val.start, val.end);
                            constraints.extend(cs);
                        }
                        Err(error) => errors.push(error),
                    },
                    Err(error) => errors.push(error),
                }

                if let Some(ann) = ann {
                    match val.t.unify(&ann.inner) {
                        Some((s, mut cs)) => match subs.extend(val.start, s, val.end) {
                            Ok(css) => {
                                cs.extend(css);
                                Constraint::apply_pos(&mut cs, val.start, val.end);
                                constraints.extend(cs);

                                if !val.t.apply_ann_exact_values(&ann.inner) {
                                    errors.push(Error::new(
                                        ErrorType::Constraint,
                                        format!(
                                            "cannot apply exact values of annotation \"{:?}\" to type \"{:?}\"",
                                            ann.inner,
                                            val.t,
                                        ),
                                        val.start,
                                        val.end
                                    ));
                                }

                                context.insert(name.inner.clone(), val.t.clone())
                            }
                            Err(error) => {
                                errors.push(error);
                                context.insert(name.inner.clone(), Type::Unknown);
                            }
                        },
                        None => {
                            errors.push(Error::new(
                                ErrorType::Type,
                                format!(
                                    "cannot unify type \"{:?}\" with annotation \"{:?}\"",
                                    val.t, ann.inner
                                ),
                                val.start,
                                val.end,
                            ));
                            context.insert(name.inner.clone(), Type::Unknown);
                        }
                    }
                } else {
                    context.insert(name.inner.clone(), val.t.clone());
                }
            }
            S::Return(val) => match val {
                Some(val) => {
                    if return_type == &Type::Void {
                        errors.push(Error::new(
                            ErrorType::Type,
                            "cannot return value from void function".into(),
                            statement.start,
                            statement.end,
                        ));
                    } else {
                        match unify_expression(val, context, curr_forall_var, constraints) {
                            Ok(s) => match subs.extend(val.start, s, val.end) {
                                Ok(mut cs) => {
                                    Constraint::apply_pos(&mut cs, val.start, val.end);
                                    constraints.extend(cs);
                                }
                                Err(error) => errors.push(error),
                            },
                            Err(error) => errors.push(error),
                        }

                        match val.t.unify(return_type) {
                            Some((s, mut cs)) => {
                                match subs.extend(val.start, s, val.end) {
                                    Ok(css) => cs.extend(css),
                                    Err(error) => errors.push(error)
                                }
                                Constraint::apply_pos(&mut cs, val.start, val.end);
                                constraints.extend(cs);
                            }
                            None => errors.push(Error::new(
                                ErrorType::Type,
                                format!(
                                    "cannot unify type \"{:?}\" with return type \"{return_type:?}\"",
                                    val.t
                                ),
                                val.start,
                                val.end,
                            )),
                        }
                    }
                }
                None => {
                    if return_type != &Type::Void {
                        errors.push(Error::new(
                            ErrorType::Type,
                            "cannot return void from non-void function".into(),
                            statement.start,
                            statement.end,
                        ));
                    }
                }
            },
            S::If(cond, true_inner, false_inner) => {
                match unify_expression(cond, context, curr_forall_var, constraints) {
                    Ok(s) => {
                        match subs.extend(cond.start, s, cond.end) {
                            Ok(mut cs) => {
                                Constraint::apply_pos(&mut cs, cond.start, cond.end);
                                constraints.extend(cs);
                            }
                            Err(error) => errors.push(error),
                        };
                        match cond.t.unify(&Type::Bool) {
                            Some((s, mut cs)) => {
                                match subs.extend(cond.start, s, cond.end) {
                                    Ok(css) => cs.extend(css),
                                    Err(error) => errors.push(error),
                                }
                                Constraint::apply_pos(&mut cs, cond.start, cond.end);
                                constraints.extend(cs);
                            }
                            None => errors.push(Error::new(
                                ErrorType::Type,
                                "condition must be boolean type".into(),
                                cond.start,
                                cond.end,
                            )),
                        }
                    }
                    Err(error) => errors.push(error),
                }

                context.push();
                unify(
                    true_inner,
                    context,
                    curr_forall_var,
                    return_type,
                    errors,
                    subs,
                    constraints,
                );
                context.pop();

                if let Some(false_inner) = false_inner {
                    context.push();
                    unify(
                        false_inner,
                        context,
                        curr_forall_var,
                        return_type,
                        errors,
                        subs,
                        constraints,
                    );
                    context.pop();
                }

                context.apply_transform(Type::demote_dirty);
            }
            S::For(iterator, from, to, inner) => {
                match unify_expression(from, context, curr_forall_var, constraints) {
                    Ok(s) => {
                        match subs.extend(from.start, s, from.end) {
                            Ok(mut cs) => {
                                Constraint::apply_pos(&mut cs, from.start, from.end);
                                constraints.extend(cs);
                            }
                            Err(error) => errors.push(error),
                        };
                        match from.t.unify(&Type::I64(None)) {
                            Some((s, mut cs)) => {
                                match subs.extend(from.start, s, from.end) {
                                    Ok(css) => cs.extend(css),
                                    Err(error) => errors.push(error),
                                }
                                Constraint::apply_pos(&mut cs, from.start, from.end);
                                constraints.extend(cs);
                            }
                            None => errors.push(Error::new(
                                ErrorType::Type,
                                "iteration start must be integer type".into(),
                                from.start,
                                from.end,
                            )),
                        }
                    }
                    Err(error) => errors.push(error),
                }

                match unify_expression(to, context, curr_forall_var, constraints) {
                    Ok(s) => {
                        match subs.extend(to.start, s, to.end) {
                            Ok(mut cs) => {
                                Constraint::apply_pos(&mut cs, to.start, to.end);
                                constraints.extend(cs);
                            }
                            Err(error) => errors.push(error),
                        };
                        match to.t.unify(&Type::I64(None)) {
                            Some((s, mut cs)) => {
                                match subs.extend(to.start, s, to.end) {
                                    Ok(css) => cs.extend(css),
                                    Err(error) => errors.push(error),
                                }
                                Constraint::apply_pos(&mut cs, to.start, to.end);
                                constraints.extend(cs);
                            }
                            None => errors.push(Error::new(
                                ErrorType::Type,
                                "iteration end must be integer type".into(),
                                to.start,
                                to.end,
                            )),
                        }
                    }
                    Err(error) => errors.push(error),
                }

                context.push();
                context.insert(iterator.inner.clone(), Type::I64(None));
                unify(
                    inner,
                    context,
                    curr_forall_var,
                    return_type,
                    errors,
                    subs,
                    constraints,
                );
                context.pop();

                context.apply_transform(Type::demote_dirty);
            }
        }
    }
}

fn substitute_expression(expression: &mut Expression, subs: &Substitutions) {
    for sub in subs.iter() {
        expression.t = expression.t.substitute(sub);
    }

    match &mut expression.data {
        E::Accessor(lhs, rhs) => {
            substitute_expression(lhs, subs);
            substitute_expression(rhs, subs);
        }
        E::BinaryOp(lhs, _, rhs) => {
            substitute_expression(lhs, subs);
            substitute_expression(rhs, subs);
        }
        E::UnaryOp(_, inner) => {
            substitute_expression(inner, subs);
        }
        E::FnCall(caller, args) => {
            substitute_expression(caller, subs);
            for arg in args {
                substitute_expression(arg, subs);
            }
        }
        E::Tuple(inner) => {
            for e in inner {
                substitute_expression(e, subs);
            }
        }

        E::F64(_) | E::I64(_, _) | E::Bool(_) | E::Ident(_, _) => (),

        E::OpenTuple(_) => panic!(),
    }
}

pub fn substitute(block: &mut [Statement], context: &mut Scope<Type>, subs: &Substitutions) {
    for statement in block.iter_mut() {
        match &mut statement.data {
            S::Print(val) => substitute_expression(val, subs),
            S::Assign(_, val) => substitute_expression(val, subs),
            S::Let(name, ann, val) => {
                substitute_expression(val, subs);
                if let Some(ann) = ann {
                    for sub in subs.iter() {
                        ann.inner = ann.inner.substitute(sub);
                    }
                }
                context.insert(name.inner.clone(), val.t.clone());
            }
            S::If(cond, true_inner, false_inner) => {
                substitute_expression(cond, subs);
                substitute(true_inner, context, subs);
                match false_inner {
                    Some(false_inner) => substitute(false_inner, context, subs),
                    None => (),
                }
            }
            S::For(iterator, from, to, inner) => {
                substitute_expression(from, subs);
                substitute_expression(to, subs);
                context.push();
                context.insert(iterator.inner.clone(), Type::I64(None));
                substitute(inner, context, subs);
                context.pop();
            }
            S::Return(Some(val)) => substitute_expression(val, subs),

            S::Return(None) => (),
        }
    }
}
