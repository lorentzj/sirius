use crate::error::Error;
use crate::parser::{Function, Statement, S};
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
                S::Let(_, _, _, _, _) => (),
                S::Print(_) => (),
                S::Return(_) => (),
                S::Assign(_, _) => (),
                S::For(_, _, inner_pre, _, _, (inner_statements, inner_post)) => {
                    let mut inner_pre = inner_pre.clone();
                    inner_pre.append(&mut preconditions.to_vec());
                    errors.extend(constraint_check_block(
                        inner_statements,
                        &inner_pre,
                        inner_post,
                    ));
                }
                S::If(_, inner_pre, (true_inner_statements, true_inner_post), false_inner) => {
                    let mut true_inner_pre = inner_pre.clone();
                    true_inner_pre.append(&mut preconditions.to_vec());
                    errors.extend(constraint_check_block(
                        true_inner_statements,
                        &true_inner_pre,
                        true_inner_post,
                    ));

                    if let Some((false_inner_statements, false_inner_post)) = false_inner {
                        let mut false_inner_pre =
                            inner_pre.iter().map(Constraint::negate).collect::<Vec<_>>();
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
    constraint_check_block(&func.body.0, &[], &func.body.1)
}
