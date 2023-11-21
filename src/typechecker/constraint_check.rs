use crate::error::Error;
use crate::parser::{Block, Function, Statement, S};
use crate::solver::{solve, Constraint};

pub fn constraint_check_block(
    block: &[Statement],
    preconditions: &[Constraint],
    postconditions: &[Constraint],
) -> Vec<Error> {
    let mut errors = vec![];

    errors.extend(solve(preconditions, postconditions));

    if errors.is_empty() {
        for statement in block {
            match &statement.data {
                S::Let { .. } => (),
                S::Print(_) => (),
                S::Return(_) => (),
                S::Assign { .. } => (),
                S::For {
                    pre_constraints,
                    inner:
                        Block {
                            statements: inner_statements,
                            post_constraints: inner_post,
                        },
                    ..
                } => {
                    let mut inner_pre = pre_constraints.clone();
                    inner_pre.append(&mut preconditions.to_vec());
                    errors.extend(constraint_check_block(
                        inner_statements,
                        &inner_pre,
                        inner_post,
                    ));
                }
                S::If {
                    pre_constraints,
                    true_inner:
                        Block {
                            statements: true_inner_statements,
                            post_constraints: true_inner_post,
                        },
                    false_inner,
                    ..
                } => {
                    let mut true_inner_pre = pre_constraints.clone();
                    true_inner_pre.append(&mut preconditions.to_vec());
                    errors.extend(constraint_check_block(
                        true_inner_statements,
                        &true_inner_pre,
                        true_inner_post,
                    ));

                    if let Some(Block {
                        statements: false_inner_statements,
                        post_constraints: false_inner_post,
                    }) = false_inner
                    {
                        let mut false_inner_pre = pre_constraints
                            .iter()
                            .map(Constraint::negate)
                            .collect::<Vec<_>>();
                        false_inner_pre.append(&mut preconditions.to_vec());

                        errors.extend(constraint_check_block(
                            false_inner_statements,
                            &false_inner_pre,
                            false_inner_post,
                        ));
                    }
                }
            }
        }
    }

    errors
}

pub fn constraint_check(func: &Function) -> Vec<Error> {
    constraint_check_block(&func.body.statements, &[], &func.body.post_constraints)
}
