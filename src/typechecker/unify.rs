use std::rc::Rc;

use super::arithmetic::{arith_coerce, is_arith};
use super::eq_classes::EqClasses;
use super::equality::equality_check;
use super::substitute::substitute;
use super::types::Type;
use super::{Constraint, ScopeEntry};
use crate::error::{Error, ErrorType};
use crate::lexer::Op;
use crate::parser::{Expression, Statement, UnaryOp, E, S};
use crate::scope::Scope;
use crate::solver::poly::Poly;

fn unify_expression(
    expression: &Expression,
    eqc: &mut EqClasses,
    context: &Scope<ScopeEntry>,
) -> Result<(), Error> {
    eqc.add_single(expression.start, &expression.t, expression.end);

    match &expression.data {
        E::Bool(_) => Ok(()),
        E::F64(_) => Ok(()),
        E::I64(_, _) => Ok(()),
        E::Accessor(_, _) => Err(Error::new(
            ErrorType::NotImplemented,
            "array access not implemented yet".into(),
            expression.start,
            expression.end,
        )),
        E::Ident(name, _) => match context.get(name) {
            Some(ScopeEntry { t: var_t, .. }) => match var_t.as_ref() {
                Type::Function(_, _, _) => Ok(()),
                _ => {
                    EqClasses::add(eqc, expression.start, &expression.t, var_t, expression.end);

                    Ok(())
                }
            },
            None => unreachable!(),
        },
        E::UnaryOp(op, inner) => {
            unify_expression(inner, eqc, context)?;

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
                            let new_ind = ind.clone() * Poly::constant(-1);
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
                unify_expression(e, eqc, context)?;
            }

            let tuple_t = Type::Tuple(inner_types);

            eqc.add_owned(expression.start, &expression.t, tuple_t, expression.end);

            Ok(())
        }
        E::BinaryOp(lhs, op, rhs) => {
            unify_expression(lhs, eqc, context)?;
            unify_expression(rhs, eqc, context)?;

            if lhs.t.forall_vars().is_empty() && rhs.t.forall_vars().is_empty() {
                if is_arith(op) {
                    let arith_result =
                        arith_coerce(expression.start, &lhs.t, op, &rhs.t, expression.end)?;
                    EqClasses::add_owned(
                        eqc,
                        expression.start,
                        &expression.t,
                        arith_result,
                        expression.end,
                    );
                } else {
                    match op {
                        Op::Equal | Op::NotEqual => match equality_check(&lhs.t, &rhs.t) {
                            Ok(_) => eqc.add_owned(
                                expression.start,
                                &expression.t,
                                Type::Bool,
                                expression.end,
                            ),
                            Err(msg) => {
                                return Err(Error::new(
                                    ErrorType::Type,
                                    msg,
                                    expression.start,
                                    expression.end,
                                ))
                            }
                        },

                        Op::Dot => match lhs.t.as_ref() {
                            Type::Tuple(lhs_inner) => match rhs.data {
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
        E::FnCall(caller, args) => {
            unify_expression(caller, eqc, context)?;

            for e in args.iter() {
                unify_expression(e, eqc, context)?;
            }

            let arg_types: Vec<_> = args.iter().map(|e| e.t.clone()).collect();

            match caller.t.as_ref() {
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
                        caller.start,
                        caller.end,
                    ))
                }
            }

            Ok(())
        }
        E::OpenTuple(_) => unreachable!(),
    }
}

pub fn unify(
    block: &mut [Statement],
    context: &mut Scope<ScopeEntry>,
    return_type: &Rc<Type>,
    errors: &mut Vec<Error>,
    curr_ind_forall_var: &mut usize,
    mut eqc: EqClasses,
    constraints: &mut Vec<Constraint>,
) -> bool {
    context.push();
    for statement in &mut *block {
        match &mut statement.data {
            S::Print(val) => match unify_expression(val, &mut eqc, context) {
                Ok(()) => (),
                Err(error) => errors.push(error),
            },
            S::Assign(place, val) => {
                match unify_expression(val, &mut eqc, context) {
                    Ok(()) => (),
                    Err(error) => errors.push(error),
                }

                match context.get(&place.inner) {
                    Some(ScopeEntry { t, .. }) => eqc.add(val.start, &val.t, t, val.end),
                    None => unreachable!(),
                }
            }
            S::Let(name, mutable, ann, val_adj_type, val) => {
                match unify_expression(val, &mut eqc, context) {
                    Ok(()) => (),
                    Err(error) => errors.push(error),
                }

                eqc.add_single(val.start, val_adj_type, val.end);

                if *mutable {
                    eqc.add_must_demote(val.start, val_adj_type, &val.t, val.end);
                } else {
                    eqc.add_must_promote(val.start, val_adj_type, &val.t, val.end);
                }

                if let Some(ann) = ann {
                    eqc.add(ann.start, val_adj_type, &ann.inner, ann.end);
                }

                context.insert(
                    name.inner.clone(),
                    ScopeEntry {
                        t: val_adj_type.clone(),
                        mutable: *mutable,
                        decl_site: Some(name.start),
                    },
                );
            }
            S::Return(val) => {
                if let Some(val) = val {
                    match unify_expression(val, &mut eqc, context) {
                        Ok(()) => (),
                        Err(error) => errors.push(error),
                    }

                    eqc.add_can_demote_or_promote(val.start, &val.t, return_type, val.end);
                } else {
                    eqc.add_owned(statement.start, return_type, Type::Void, statement.end);
                }
            }
            S::If(cond, _, (true_inner, true_inner_constraints), false_inner) => {
                match unify_expression(cond, &mut eqc, context) {
                    Ok(()) => (),
                    Err(error) => errors.push(error),
                }

                eqc.add_owned(cond.start, &cond.t, Type::Bool, cond.end);

                unify(
                    true_inner,
                    context,
                    return_type,
                    errors,
                    curr_ind_forall_var,
                    eqc.clone(),
                    true_inner_constraints,
                );
                if let Some((false_inner, false_inner_constraints)) = false_inner {
                    unify(
                        false_inner,
                        context,
                        return_type,
                        errors,
                        curr_ind_forall_var,
                        eqc.clone(),
                        false_inner_constraints,
                    );
                }
            }
            S::For(iterator, iter_type, _, from, to, (inner, inner_constraints)) => {
                match unify_expression(from, &mut eqc, context) {
                    Ok(()) => (),
                    Err(error) => errors.push(error),
                }

                match unify_expression(to, &mut eqc, context) {
                    Ok(()) => (),
                    Err(error) => errors.push(error),
                }

                eqc.add_can_demote_or_promote_owned(from.start, &from.t, Type::I64(None), from.end);
                eqc.add_can_demote_or_promote_owned(to.start, &to.t, Type::I64(None), to.end);

                context.push();
                context.insert(
                    iterator.inner.clone(),
                    ScopeEntry {
                        t: Rc::new(iter_type.clone()),
                        mutable: false,
                        decl_site: Some(iterator.start),
                    },
                );

                unify(
                    inner,
                    context,
                    return_type,
                    errors,
                    curr_ind_forall_var,
                    eqc.clone(),
                    inner_constraints,
                );
                context.pop();
            }
        }
    }
    context.pop();

    match eqc.generate(curr_ind_forall_var) {
        Ok((any_changes, cs)) => {
            substitute(block, &eqc);
            *constraints = cs;
            any_changes
        }
        Err(error) => {
            errors.push(error);
            false
        }
    }
}
