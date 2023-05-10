use super::{Constraint, Type};
use crate::lexer::Op;
use crate::parser::{Expression, Statement, E, S};

fn constraints_from_expr(expr: &Expression) -> Vec<Constraint> {
    match &expr.data {
        E::BinaryOp(lhs, Op::Equal, rhs) => {
            if let (Type::I64(Some(lhs_val)), Type::I64(Some(rhs_val))) =
                (lhs.t.as_ref(), rhs.t.as_ref())
            {
                vec![Constraint::new_eq_z(lhs_val.clone() - rhs_val.clone())]
            } else {
                vec![]
            }
        }

        E::BinaryOp(lhs, Op::And, rhs) => {
            vec![constraints_from_expr(lhs), constraints_from_expr(rhs)].concat()
        }

        _ => vec![],
    }
}

pub fn add_preconditions(block: &mut [Statement]) {
    for statement in block {
        match &mut statement.data {
            S::If(cond, preconditions, (true_block, _), false_block) => {
                *preconditions = constraints_from_expr(cond);
                add_preconditions(true_block);
                if let Some((false_block, _)) = false_block {
                    add_preconditions(false_block);
                }
            }

            S::For(_, iter_type, preconditions, from, to, (inner_block, _)) => {
                if let Type::I64(Some(iter_val)) = iter_type {
                    if let Type::I64(Some(from_val)) = from.t.as_ref() {
                        preconditions
                            .push(Constraint::new_gt_eq_z(iter_val.clone() - from_val.clone()))
                    }

                    if let Type::I64(Some(to_val)) = to.t.as_ref() {
                        preconditions.push(Constraint::new_gt_z(to_val.clone() - iter_val.clone()))
                    }
                }

                add_preconditions(inner_block);
            }
            _ => (),
        }
    }
}
