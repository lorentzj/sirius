use crate::error::{Error, ErrorType};
use crate::parser::{Expression, Function, Statement, E, S};

fn check_expr_for_foralls(expression: &Expression) -> Vec<Error> {
    let mut errors: Vec<Error> = vec![];

    if !expression.t.forall_vars().is_empty() {
        errors.push(Error::new(
            ErrorType::Type,
            format!(
                "type \"{:?}\" is not concrete; try adding annotations or type arguments",
                expression.t
            ),
            expression.start,
            expression.end,
        ));
    }

    match &expression.data {
        E::Accessor { target, index } => {
            errors.extend(check_expr_for_foralls(target));
            errors.extend(check_expr_for_foralls(index));
        }

        E::BinaryOp { lhs, rhs, .. } => {
            errors.extend(check_expr_for_foralls(lhs));
            errors.extend(check_expr_for_foralls(rhs));
        }

        E::UnaryOp { inner, .. } => {
            errors.extend(check_expr_for_foralls(inner));
        }

        E::FnCall { func, args } => {
            errors.extend(check_expr_for_foralls(func));
            for arg in args {
                errors.extend(check_expr_for_foralls(arg));
            }
        }

        E::Tuple(inner) => {
            for e in inner {
                errors.extend(check_expr_for_foralls(e));
            }
        }

        E::F64(_) | E::Bool(_) | E::I64(_) | E::Ident { .. } => (),

        E::OpenTuple(_) => panic!(),
    }

    errors
}

fn check_stmt_for_foralls(statement: &Statement) -> Vec<Error> {
    let mut errors = vec![];

    match &statement.data {
        S::Print(val) => errors.extend(check_expr_for_foralls(val)),
        S::Assign { value, .. } => errors.extend(check_expr_for_foralls(value)),
        S::Return(Some(val)) => errors.extend(check_expr_for_foralls(val)),
        S::Return(None) => (),
        S::Let {
            bound_type, value, ..
        } => {
            if !bound_type.forall_vars().is_empty() {
                errors.push(Error::new(
                    ErrorType::Type,
                    format!(
                        "type \"{:?}\" is not concrete; try adding annotations or type arguments",
                        bound_type
                    ),
                    value.start,
                    value.end,
                ));
            }
            errors.extend(check_expr_for_foralls(value))
        }
        S::If {
            condition,
            true_inner,
            false_inner,
            ..
        } => {
            errors.extend(check_expr_for_foralls(condition));
            for stmt in &true_inner.statements {
                errors.extend(check_stmt_for_foralls(stmt));
            }
            if let Some(false_inner) = false_inner {
                for stmt in &false_inner.statements {
                    errors.extend(check_stmt_for_foralls(stmt));
                }
            }
        }
        S::For {
            from, to, inner, ..
        } => {
            errors.extend(check_expr_for_foralls(from));
            errors.extend(check_expr_for_foralls(to));
            for stmt in &inner.statements {
                errors.extend(check_stmt_for_foralls(stmt));
            }
        }
    }

    errors
}

pub fn concreteness_check(function: &Function) -> Vec<Error> {
    let mut errors = vec![];

    for statement in &function.body.statements {
        errors.extend(check_stmt_for_foralls(statement))
    }

    errors
}
