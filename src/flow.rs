use crate::error::{Error, ErrorType};
use crate::parser::{Statement, AST};

pub fn check_block_flow_returns(block: &[Statement]) -> bool {
    for statement in block {
        match statement {
            Statement::If {
                true_inner,
                false_inner: Some(false_inner),
                ..
            } => {
                if check_block_flow_returns(true_inner) && check_block_flow_returns(false_inner) {
                    return true;
                }
            }

            Statement::Return { .. } => return true,

            _ => (),
        }
    }

    false
}

pub fn check_flow(ast: &AST) -> Vec<Error> {
    let mut errors = vec![];

    for (name, function) in ast {
        if let Some(return_type_expression) = function.return_type.as_ref() {
            if !check_block_flow_returns(&function.inner) {
                let (return_type_start, return_type_end) = return_type_expression.range();
                errors.push(Error::new(
                    ErrorType::FlowError,
                    format!("function '{name}' may not always return value"),
                    return_type_start,
                    return_type_end,
                ))
            }
        }
    }

    errors
}
