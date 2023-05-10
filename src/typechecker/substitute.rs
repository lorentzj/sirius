use super::eq_classes::EqClasses;
use crate::parser::{Expression, Statement, E, S};

fn substitute_expression(expression: &mut Expression, eq_classes: &EqClasses) {
    if let Some(t) = eq_classes.find_replacement(&expression.t) {
        expression.t = t;
    }

    match &mut expression.data {
        E::Accessor(lhs, rhs) => {
            substitute_expression(lhs, eq_classes);
            substitute_expression(rhs, eq_classes);
        }
        E::BinaryOp(lhs, _, rhs) => {
            substitute_expression(lhs, eq_classes);
            substitute_expression(rhs, eq_classes);
        }
        E::UnaryOp(_, inner) => {
            substitute_expression(inner, eq_classes);
        }
        E::FnCall(caller, args) => {
            substitute_expression(caller, eq_classes);
            for arg in args {
                substitute_expression(arg, eq_classes);
            }
        }
        E::Tuple(inner) => {
            for e in inner {
                substitute_expression(e, eq_classes);
            }
        }

        E::F64(_) | E::I64(_, _) | E::Bool(_) | E::Ident(_, _) => (),

        E::OpenTuple(_) => unreachable!(),
    }
}

pub fn substitute(block: &mut [Statement], eq_classes: &EqClasses) {
    for statement in block.iter_mut() {
        match &mut statement.data {
            S::Print(val) => substitute_expression(val, eq_classes),
            S::Assign(_, val) => substitute_expression(val, eq_classes),
            S::Let(_, _, ann, val_adj_type, val) => {
                if let Some(t) = eq_classes.find_replacement(val_adj_type) {
                    *val_adj_type = t;
                }

                substitute_expression(val, eq_classes);

                if let Some(ann) = ann {
                    if let Some(t) = eq_classes.find_replacement(&ann.inner) {
                        ann.inner = t;
                    }
                }
            }
            S::If(cond, _, (true_inner, _), false_inner) => {
                substitute_expression(cond, eq_classes);
                substitute(true_inner, eq_classes);
                match false_inner {
                    Some((false_inner, _)) => substitute(false_inner, eq_classes),
                    None => (),
                }
            }
            S::For(_, _, _, from, to, (inner, _)) => {
                substitute_expression(from, eq_classes);
                substitute_expression(to, eq_classes);
                substitute(inner, eq_classes);
            }
            S::Return(Some(val)) => substitute_expression(val, eq_classes),

            S::Return(None) => (),
        }
    }
}
