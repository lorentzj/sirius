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
        E::Accessor(lhs, rhs) => {
            errors.extend(check_expr_for_foralls(lhs));
            errors.extend(check_expr_for_foralls(rhs));
        }

        E::BinaryOp(lhs, _, rhs) => {
            errors.extend(check_expr_for_foralls(lhs));
            errors.extend(check_expr_for_foralls(rhs));
        }

        E::UnaryOp(_, inner) => {
            errors.extend(check_expr_for_foralls(inner));
        }

        E::FnCall(caller, args) => {
            errors.extend(check_expr_for_foralls(caller));
            for arg in args {
                errors.extend(check_expr_for_foralls(arg));
            }
        }

        E::Tuple(inner) => {
            for e in inner {
                errors.extend(check_expr_for_foralls(e));
            }
        }

        E::F64(_) | E::Bool(_) | E::I64(_, _) | E::Ident(_, _) => (),

        E::OpenTuple(_) => panic!(),
    }

    errors
}

fn check_stmt_for_foralls(statement: &Statement) -> Vec<Error> {
    let mut errors = vec![];

    match &statement.data {
        S::Print(val) => errors.extend(check_expr_for_foralls(val)),
        S::Assign(_, val) => errors.extend(check_expr_for_foralls(val)),
        S::Return(Some(val)) => errors.extend(check_expr_for_foralls(val)),
        S::Return(None) => (),
        S::Let(_, _, val) => errors.extend(check_expr_for_foralls(val)),
        S::If(cond, true_inner, false_inner) => {
            errors.extend(check_expr_for_foralls(cond));
            for stmt in true_inner {
                errors.extend(check_stmt_for_foralls(stmt));
            }
            if let Some(false_inner) = false_inner {
                for stmt in false_inner {
                    errors.extend(check_stmt_for_foralls(stmt));
                }
            }
        }
        S::For(_, from, to, inner) => {
            errors.extend(check_expr_for_foralls(from));
            errors.extend(check_expr_for_foralls(to));
            for stmt in inner {
                errors.extend(check_stmt_for_foralls(stmt));
            }
        }
    }

    errors
}

pub fn concreteness_check(function: &Function) -> Vec<Error> {
    let mut errors = vec![];

    for statement in &function.body {
        errors.extend(check_stmt_for_foralls(statement))
    }

    errors
}
