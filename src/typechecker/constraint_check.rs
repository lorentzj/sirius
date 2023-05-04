use super::types::C;
use crate::error::{Error, ErrorType};
use crate::parser::Function;

pub fn constraint_check(func: &Function) -> Vec<Error> {
    let mut errors = vec![];

    for constraint in &func.constraints {
        match &constraint.data {
            C::Eq(lhs, rhs) => {
                if let (Some(lhs_val), Some(rhs_val)) = (lhs.constant_val(), rhs.constant_val()) {
                    if lhs_val != rhs_val {
                        errors.push(Error::new(
                            ErrorType::Constraint,
                            format!("cannot satisfy constraint \"{lhs_val} == {rhs_val}\""),
                            constraint.start,
                            constraint.end,
                        ));
                    }
                }
            }
        }
    }

    errors
}
