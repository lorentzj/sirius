use crate::error::{Error, ErrorType};
use crate::parser::{Statement, AST, S};
use crate::typechecker::Type;

fn check_block_flow_returns(block: &[Statement]) -> bool {
    for statement in block {
        match &statement.data {
            S::If(_, true_inner, Some(false_inner)) => {
                if check_block_flow_returns(true_inner) && check_block_flow_returns(false_inner) {
                    return true;
                }
            }

            S::Return(_) => return true,

            _ => (),
        }
    }

    false
}

pub fn check_flow(ast: &AST) -> Vec<Error> {
    let mut errors = vec![];

    for function in ast.values() {
        if function.return_type.inner != Type::Void
            && function.return_type.inner != Type::Unknown
            && !check_block_flow_returns(&function.body)
        {
            errors.push(Error::new(
                ErrorType::Flow,
                format!("function \"{}\" may not always return value", function.name),
                function.return_type.start,
                function.return_type.end,
            ))
        }
    }

    errors
}
