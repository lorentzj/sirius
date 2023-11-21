use super::eq_classes::EqClasses;
use crate::parser::{Block, Expression, Statement, E, S};

fn substitute_expression(expression: &mut Expression, eq_classes: &EqClasses) {
    if let Some(t) = eq_classes.find_replacement(&expression.t) {
        expression.t = t;
    }

    match &mut expression.data {
        E::Accessor { target, index } => {
            substitute_expression(target, eq_classes);
            substitute_expression(index, eq_classes);
        }
        E::BinaryOp { lhs, rhs, .. } => {
            substitute_expression(lhs, eq_classes);
            substitute_expression(rhs, eq_classes);
        }
        E::UnaryOp { inner, .. } => {
            substitute_expression(inner, eq_classes);
        }
        E::FnCall { func, args } => {
            substitute_expression(func, eq_classes);
            for arg in args {
                substitute_expression(arg, eq_classes);
            }
        }
        E::Tuple(inner) => {
            for e in inner {
                substitute_expression(e, eq_classes);
            }
        }

        E::F64(_) | E::I64(_) | E::Bool(_) | E::Ident { .. } => (),

        E::OpenTuple(_) => unreachable!(),
    }
}

pub fn substitute(block: &mut [Statement], eq_classes: &EqClasses) {
    for statement in block.iter_mut() {
        match &mut statement.data {
            S::Print(val) => substitute_expression(val, eq_classes),
            S::Assign { value, .. } => substitute_expression(value, eq_classes),
            S::Let {
                annotation,
                bound_type,
                value,
                ..
            } => {
                if let Some(t) = eq_classes.find_replacement(bound_type) {
                    *bound_type = t;
                }

                substitute_expression(value, eq_classes);

                if let Some(annotation) = annotation {
                    if let Some(t) = eq_classes.find_replacement(&annotation.inner) {
                        annotation.inner = t;
                    }
                }
            }
            S::If {
                condition,
                true_inner:
                    Block {
                        statements: true_inner,
                        ..
                    },
                false_inner,
                ..
            } => {
                substitute_expression(condition, eq_classes);
                substitute(true_inner, eq_classes);
                match false_inner {
                    Some(Block {
                        statements: false_inner,
                        ..
                    }) => substitute(false_inner, eq_classes),
                    None => (),
                }
            }
            S::For {
                from,
                to,
                inner: Block {
                    statements: inner, ..
                },
                ..
            } => {
                substitute_expression(from, eq_classes);
                substitute_expression(to, eq_classes);
                substitute(inner, eq_classes);
            }
            S::Return(Some(val)) => substitute_expression(val, eq_classes),

            S::Return(None) => (),
        }
    }
}
