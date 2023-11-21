use super::{Constraint, Type};
use crate::lexer::Op;
use crate::parser::{Block, Expression, Statement, E, S};

fn constraints_from_expr(expr: &Expression) -> Vec<Constraint> {
    match &expr.data {
        E::BinaryOp {
            lhs,
            op: Op::Equal,
            rhs,
        } => {
            if let (Type::I64(Some(lhs_val)), Type::I64(Some(rhs_val))) =
                (lhs.t.as_ref(), rhs.t.as_ref())
            {
                let mut constraint = Constraint::new_eq(lhs_val.clone(), rhs_val.clone());
                constraint.start = expr.start;
                constraint.end = expr.end;
                vec![constraint]
            } else {
                vec![]
            }
        }

        E::BinaryOp {
            lhs,
            op: Op::NotEqual,
            rhs,
        } => {
            if let (Type::I64(Some(lhs_val)), Type::I64(Some(rhs_val))) =
                (lhs.t.as_ref(), rhs.t.as_ref())
            {
                let mut constraint = Constraint::new_neq(lhs_val.clone(), rhs_val.clone());
                constraint.start = expr.start;
                constraint.end = expr.end;
                vec![constraint]
            } else {
                vec![]
            }
        }

        E::BinaryOp {
            lhs,
            op: Op::Greater,
            rhs,
        } => {
            if let (Type::I64(Some(lhs_val)), Type::I64(Some(rhs_val))) =
                (lhs.t.as_ref(), rhs.t.as_ref())
            {
                let mut constraint = Constraint::new_gt(lhs_val.clone(), rhs_val.clone());
                constraint.start = expr.start;
                constraint.end = expr.end;
                vec![constraint]
            } else {
                vec![]
            }
        }

        E::BinaryOp {
            lhs,
            op: Op::Less,
            rhs,
        } => {
            if let (Type::I64(Some(lhs_val)), Type::I64(Some(rhs_val))) =
                (lhs.t.as_ref(), rhs.t.as_ref())
            {
                let mut constraint = Constraint::new_gt(rhs_val.clone(), lhs_val.clone());
                constraint.start = expr.start;
                constraint.end = expr.end;
                vec![constraint]
            } else {
                vec![]
            }
        }

        E::BinaryOp {
            lhs,
            op: Op::GreaterOrEq,
            rhs,
        } => {
            if let (Type::I64(Some(lhs_val)), Type::I64(Some(rhs_val))) =
                (lhs.t.as_ref(), rhs.t.as_ref())
            {
                let mut constraint = Constraint::new_gt_eq(lhs_val.clone(), rhs_val.clone());
                constraint.start = expr.start;
                constraint.end = expr.end;
                vec![constraint]
            } else {
                vec![]
            }
        }

        E::BinaryOp {
            lhs,
            op: Op::LessOrEq,
            rhs,
        } => {
            if let (Type::I64(Some(lhs_val)), Type::I64(Some(rhs_val))) =
                (lhs.t.as_ref(), rhs.t.as_ref())
            {
                let mut constraint = Constraint::new_gt_eq(rhs_val.clone(), lhs_val.clone());
                constraint.start = expr.start;
                constraint.end = expr.end;
                vec![constraint]
            } else {
                vec![]
            }
        }

        E::BinaryOp {
            lhs,
            op: Op::And,
            rhs,
        } => vec![constraints_from_expr(lhs), constraints_from_expr(rhs)].concat(),

        E::BinaryOp {
            lhs,
            op: Op::Or,
            rhs,
        } => {
            let mut lhs_constraints = constraints_from_expr(lhs);
            let rhs_constraints = constraints_from_expr(rhs);

            lhs_constraints.retain(|c| rhs_constraints.contains(c));
            lhs_constraints
        }

        _ => vec![],
    }
}

pub fn add_preconditions(block: &mut [Statement]) {
    for statement in block.iter_mut() {
        match &mut statement.data {
            S::If {
                condition,
                pre_constraints,
                true_inner:
                    Block {
                        statements: true_block,
                        ..
                    },
                false_inner,
            } => {
                *pre_constraints = constraints_from_expr(condition);
                add_preconditions(true_block);
                if let Some(Block {
                    statements: false_block,
                    ..
                }) = false_inner
                {
                    add_preconditions(false_block);
                }
            }

            S::For {
                iterator_type,
                pre_constraints,
                from,
                to,
                inner:
                    Block {
                        statements: inner_block,
                        ..
                    },
                ..
            } => {
                if let Type::I64(Some(iter_val)) = iterator_type {
                    if let Type::I64(Some(from_val)) = from.t.as_ref() {
                        let mut constraint =
                            Constraint::new_gt_eq(iter_val.clone(), from_val.clone());
                        constraint.start = from.start;
                        constraint.end = from.end;
                        pre_constraints.push(constraint);
                    }

                    if let Type::I64(Some(to_val)) = to.t.as_ref() {
                        let mut constraint = Constraint::new_gt(to_val.clone(), iter_val.clone());
                        constraint.start = to.start;
                        constraint.end = to.end;

                        pre_constraints.push(constraint);
                    }
                }

                add_preconditions(inner_block);
            }
            _ => (),
        }
    }
}
