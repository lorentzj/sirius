use super::equality::equality_check;
use super::number_coersion::{arith_coerce, is_arith};
use super::types::Type;
use super::types::{demote_dirty, Substitutions};
use crate::error::{Error, ErrorType};
use crate::lexer::Op;
use crate::parser::{Expression, Statement, UnaryOp, E, S};
use crate::scope::Scope;

fn unify_expression(
    expression: &Expression,
    context: &Scope<Type>,
    curr_forall_var: &mut usize,
) -> Result<Substitutions, Error> {
    match &expression.data {
        E::Bool(_) => Ok(Substitutions::new()),
        E::F64(_) => Ok(Substitutions::new()),
        E::I64(_, _) => Ok(Substitutions::new()),
        E::Accessor(lhs, rhs) => {
            let lhs_subs = unify_expression(lhs, context, curr_forall_var)?;
            let rhs_subs = unify_expression(rhs, context, curr_forall_var)?;

            match &lhs.t {
                Type::Tuple(lhs_members) => match &rhs.t {
                    Type::I64(Some(ind)) => match ind.constant_val() {
                        Some(val) => {
                            if val as usize >= lhs_members.len() || val < 0 {
                                Err(Error::new(
                                    ErrorType::TypeError,
                                    format!("index out of bounds of {}-tuple", lhs_members.len()),
                                    expression.start,
                                    expression.end,
                                ))
                            } else {
                                let mut subs = lhs_subs;
                                subs.extend(rhs.start, rhs_subs, rhs.end, false)?;
                                match expression.t.unify(&lhs_members[val as usize], false) {
                                    Some(s) => {
                                        subs.extend(lhs.start, s, lhs.end, false)?;
                                        Ok(subs)
                                    }
                                    None => Err(Error::new(
                                        ErrorType::TypeError,
                                        format!(
                                            "cannot unify types \"{:?}\" and \"{:?}\"",
                                            expression.t, &lhs_members[val as usize]
                                        ),
                                        expression.start,
                                        expression.end,
                                    )),
                                }
                            }
                        }
                        None => Err(Error::new(
                            ErrorType::TypeError,
                            "tuple index must be statically known".into(),
                            expression.start,
                            expression.end,
                        )),
                    },
                    Type::I64(None) => Err(Error::new(
                        ErrorType::TypeError,
                        "tuple index must be statically known".into(),
                        expression.start,
                        expression.end,
                    )),
                    Type::ForAll(_) => {
                        let mut subs = lhs_subs;
                        subs.extend(expression.start, rhs_subs, expression.end, false)?;
                        Ok(subs)
                    }
                    t => Err(Error::new(
                        ErrorType::TypeError,
                        format!("cannot index tuple with type \"{t:?}\""),
                        expression.start,
                        expression.end,
                    )),
                },
                Type::ForAll(_) => match &rhs.t {
                    Type::I64(Some(ind)) => match ind.constant_val() {
                        Some(_) => {
                            let mut subs = lhs_subs;
                            subs.extend(expression.start, rhs_subs, expression.end, false)?;
                            Ok(subs)
                        }
                        None => Err(Error::new(
                            ErrorType::TypeError,
                            "tuple index must be statically known".into(),
                            expression.start,
                            expression.end,
                        )),
                    },
                    Type::I64(None) => Err(Error::new(
                        ErrorType::TypeError,
                        "tuple index must be statically known".into(),
                        expression.start,
                        expression.end,
                    )),
                    Type::ForAll(_) => Ok(Substitutions::new()),
                    t => Err(Error::new(
                        ErrorType::TypeError,
                        format!("cannot index tuple with type \"{t:?}\""),
                        expression.start,
                        expression.end,
                    )),
                },
                t => Err(Error::new(
                    ErrorType::TypeError,
                    format!("cannot index into type \"{t:?}\""),
                    expression.start,
                    expression.end,
                )),
            }
        }
        E::Ident(name, _) => match context.get(name) {
            Some(var_t) => match var_t {
                Type::Function(_, _, _) => Ok(Substitutions::new()),
                _ => match expression.t.unify(var_t, false) {
                    Some(subs) => Ok(subs),
                    None => Err(Error::new(
                        ErrorType::TypeError,
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
            let mut subs = unify_expression(inner, context, curr_forall_var)?;

            if inner.t.forall_vars().is_empty() {
                match op {
                    UnaryOp::ArithNeg => match &inner.t {
                        Type::F64 => match Type::F64.unify(&expression.t, false) {
                            Some(s) => subs.extend(expression.start, s, expression.end, false)?,
                            None => {
                                return Err(Error::new(
                                    ErrorType::TypeError,
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
                        Type::I64(_) => match Type::I64(None).unify(&expression.t, false) {
                            Some(s) => subs.extend(expression.start, s, expression.end, false)?,
                            None => {
                                return Err(Error::new(
                                    ErrorType::TypeError,
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
                                ErrorType::TypeError,
                                format!("cannot apply arithmetic negation to type \"{t:?}\""),
                                expression.start,
                                expression.end,
                            ))
                        }
                    },
                    UnaryOp::BoolNeg => match &inner.t {
                        Type::Bool => match Type::Bool.unify(&expression.t, false) {
                            Some(s) => subs.extend(expression.start, s, expression.end, false)?,
                            None => {
                                return Err(Error::new(
                                    ErrorType::TypeError,
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
                                ErrorType::TypeError,
                                format!("cannot apply boolean negation to type \"{t:?}\""),
                                expression.start,
                                expression.end,
                            ))
                        }
                    },
                    UnaryOp::Tick => {
                        return Err(Error::new(
                            ErrorType::NotImplementedError,
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
                subs.extend(
                    expression.start,
                    unify_expression(e, context, curr_forall_var)?,
                    expression.end,
                    false,
                )?;
            }

            let tuple_t = Type::Tuple(inner_types);

            match expression.t.unify(&tuple_t, false) {
                Some(s) => subs.extend(expression.start, s, expression.end, false)?,
                None => {
                    return Err(Error::new(
                        ErrorType::TypeError,
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
            let lhs_subs = unify_expression(lhs, context, curr_forall_var)?;
            let rhs_subs = unify_expression(rhs, context, curr_forall_var)?;

            let mut subs = lhs_subs;
            subs.extend(rhs.start, rhs_subs, rhs.end, false)?;

            if lhs.t.forall_vars().is_empty() && rhs.t.forall_vars().is_empty() {
                if is_arith(op) {
                    let arith_result =
                        arith_coerce(expression.start, &lhs.t, op, &rhs.t, expression.end)?;

                    match expression.t.unify(&arith_result, false) {
                        Some(s) => subs.extend(expression.start, s, expression.end, false)?,
                        None => {
                            return Err(Error::new(
                                ErrorType::TypeError,
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
                            Ok(_) => match expression.t.unify(&Type::Bool, false) {
                                Some(s) => {
                                    subs.extend(expression.start, s, expression.end, false)?
                                }
                                None => {
                                    return Err(Error::new(
                                        ErrorType::TypeError,
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
                                    ErrorType::TypeError,
                                    msg,
                                    expression.start,
                                    expression.end,
                                ))
                            }
                        },

                        Op::Dot => {
                            return Err(Error::new(
                                ErrorType::NotImplementedError,
                                "dot operator not implemented yet".into(),
                                expression.start,
                                expression.end,
                            ))
                        }
                        Op::And | Op::Or => {
                            if lhs.t != Type::Bool {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    format!(
                                        "cannot apply boolean operator to type \"{:?}\"",
                                        lhs.t
                                    ),
                                    expression.start,
                                    expression.end,
                                ));
                            } else if rhs.t != Type::Bool {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    format!(
                                        "cannot apply boolean operator to type \"{:?}\"",
                                        rhs.t
                                    ),
                                    expression.start,
                                    expression.end,
                                ));
                            } else {
                                match expression.t.unify(&Type::Bool, false) {
                                    Some(s) => {
                                        subs.extend(expression.start, s, expression.end, false)?
                                    }
                                    None => {
                                        return Err(Error::new(
                                            ErrorType::TypeError,
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
                                ErrorType::TypeError,
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
            let mut subs = unify_expression(caller, context, curr_forall_var)?;
            let arg_types: Vec<_> = args.iter().map(|e| e.t.clone()).collect();
            for e in args {
                subs.extend(
                    expression.start,
                    unify_expression(e, context, curr_forall_var)?,
                    expression.end,
                    false,
                )?;
            }

            match &caller.t {
                Type::Function(type_vars, i, o) => {
                    if i.len() != arg_types.len() {
                        return Err(Error::new(
                            ErrorType::TypeError,
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
                            match given_arg.unify(expected_arg, true) {
                                Some(s) => {
                                    subs.extend(expression.start, s, expression.end, false)?;
                                },
                                None => {
                                    return Err(Error::new(
                                        ErrorType::TypeError,
                                        format!("cannot unify types \"{given_arg:?}\" and \"{expected_arg:?}\""),
                                        expression.start,
                                        expression.end,
                                    ))
                                }
                            }
                        }
                    }

                    match o.unify(&expression.t, true) {
                        Some(s) => subs.extend(expression.start, s, expression.end, false)?,
                        None => {
                            return Err(Error::new(
                                ErrorType::TypeError,
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
                        ErrorType::TypeError,
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
    block: &[Statement],
    context: &mut Scope<Type>,
    curr_forall_var: &mut usize,
    return_type: &Type,
    errors: &mut Vec<Error>,
    subs: &mut Substitutions,
) {
    for statement in block {
        match &statement.data {
            S::Print(val) => match unify_expression(val, context, curr_forall_var) {
                Ok(s) => match subs.extend(val.start, s, val.end, false) {
                    Ok(()) => (),
                    Err(error) => errors.push(error),
                },
                Err(error) => errors.push(error),
            },
            S::Assign(place, val) => match context.get_mut(&place.inner) {
                Some(var_t) => {
                    match var_t.unify_assign(&val.t) {
                        Some(s) => match subs.extend(val.start, s, val.end, false) {
                            Ok(()) => (),
                            Err(error) => errors.push(error),
                        },
                        None => errors.push(Error::new(
                            ErrorType::TypeError,
                            format!("cannot unify types \"{var_t:?}\" and \"{:?}\"", val.t),
                            statement.start,
                            statement.end,
                        )),
                    }

                    match unify_expression(val, context, curr_forall_var) {
                        Ok(s) => match subs.extend(val.start, s, val.end, false) {
                            Ok(()) => (),
                            Err(error) => errors.push(error),
                        },
                        Err(error) => errors.push(error),
                    }
                }
                None => panic!(),
            },
            S::Let(name, ann, val) => {
                match unify_expression(val, context, curr_forall_var) {
                    Ok(s) => match subs.extend(val.start, s, val.end, false) {
                        Ok(()) => (),
                        Err(error) => errors.push(error),
                    },
                    Err(error) => errors.push(error),
                }

                if let Some(ann) = ann {
                    match val.t.unify_ann(&ann.inner) {
                        Some(s) => match subs.extend(val.start, s, val.end, false) {
                            Ok(()) => {
                                if let Some(mod_t) = val.t.apply_ann(&ann.inner) {
                                    context.insert(name.inner.clone(), mod_t);
                                } else {
                                    context.insert(name.inner.clone(), val.t.clone());
                                }
                            }
                            Err(error) => {
                                errors.push(error);
                                context.insert(name.inner.clone(), Type::Unknown);
                            }
                        },
                        None => {
                            errors.push(Error::new(
                                ErrorType::TypeError,
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
                            ErrorType::TypeError,
                            "cannot return value from void function".into(),
                            statement.start,
                            statement.end,
                        ));
                    } else {
                        match unify_expression(val, context, curr_forall_var) {
                            Ok(s) => match subs.extend(val.start, s, val.end, false) {
                                Ok(()) => (),
                                Err(error) => errors.push(error),
                            },
                            Err(error) => errors.push(error),
                        }

                        match val.t.unify(return_type, true) {
                            Some(s) => match subs.extend(val.start, s, val.end, false) {
                                Ok(()) => (),
                                Err(error) => errors.push(error),
                            },
                            None => errors.push(Error::new(
                                ErrorType::TypeError,
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
                            ErrorType::TypeError,
                            "cannot return void from non-void function".into(),
                            statement.start,
                            statement.end,
                        ));
                    }
                }
            },
            S::If(cond, true_inner, false_inner) => {
                match unify_expression(cond, context, curr_forall_var) {
                    Ok(s) => {
                        match subs.extend(cond.start, s, cond.end, false) {
                            Ok(()) => (),
                            Err(error) => errors.push(error),
                        };
                        match cond.t.unify(&Type::Bool, false) {
                            Some(s) => match subs.extend(cond.start, s, cond.end, false) {
                                Ok(()) => (),
                                Err(error) => errors.push(error),
                            },
                            None => errors.push(Error::new(
                                ErrorType::TypeError,
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
                );
                context.pop();
                context.apply_transform(demote_dirty);

                if let Some(false_inner) = false_inner {
                    context.push();
                    unify(
                        false_inner,
                        context,
                        curr_forall_var,
                        return_type,
                        errors,
                        subs,
                    );
                    context.pop();
                    context.apply_transform(demote_dirty);
                }
            }
            S::For(iterator, from, to, inner) => {
                match unify_expression(from, context, curr_forall_var) {
                    Ok(s) => {
                        match subs.extend(from.start, s, from.end, false) {
                            Ok(()) => (),
                            Err(error) => errors.push(error),
                        };
                        match from.t.unify(&Type::I64(None), true) {
                            Some(s) => match subs.extend(from.start, s, from.end, true) {
                                Ok(()) => (),
                                Err(error) => errors.push(error),
                            },
                            None => errors.push(Error::new(
                                ErrorType::TypeError,
                                "iteration start must be integer type".into(),
                                from.start,
                                from.end,
                            )),
                        }
                    }
                    Err(error) => errors.push(error),
                }

                match unify_expression(to, context, curr_forall_var) {
                    Ok(s) => {
                        match subs.extend(to.start, s, to.end, false) {
                            Ok(()) => (),
                            Err(error) => errors.push(error),
                        };
                        match to.t.unify(&Type::I64(None), true) {
                            Some(s) => match subs.extend(to.start, s, to.end, true) {
                                Ok(()) => (),
                                Err(error) => errors.push(error)
                            },
                            None => errors.push(Error::new(
                                ErrorType::TypeError,
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
                unify(inner, context, curr_forall_var, return_type, errors, subs);
                context.pop();
                context.apply_transform(demote_dirty);
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
