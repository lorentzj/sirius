use super::types::Type;
use crate::error::{Error, ErrorType};
use crate::parser::Function;

pub fn check_fn_args(function: &Function) -> Vec<Error> {
    let mut errors = vec![];

    let mut type_arg_names = vec![];
    for type_arg in function.type_args.iter() {
        if type_arg_names.contains(&type_arg.inner) {
            errors.push(Error::new(
                ErrorType::Name,
                format!("duplicate type argument name \"{}\"", type_arg.inner),
                type_arg.start,
                type_arg.end,
            ));
        }

        type_arg_names.push(type_arg.inner.clone());
    }

    let mut arg_names = vec![];
    for (arg, _) in function.args.iter() {
        if arg_names.contains(&arg.inner) {
            errors.push(Error::new(
                ErrorType::Name,
                format!("duplicate argument name \"{}\"", arg.inner),
                arg.start,
                arg.end,
            ));
        }

        if type_arg_names.contains(&arg.inner) {
            errors.push(Error::new(
                ErrorType::Name,
                format!("duplicate argument/type argument name \"{}\"", arg.inner),
                arg.start,
                arg.end,
            ));
        }

        arg_names.push(arg.inner.clone());
    }

    if function.name.inner == "main" {
        if !function.type_args.is_empty() {
            errors.push(Error::new(
                ErrorType::Type,
                format!(
                    "function \"main\" must have type \"{:?}\"",
                    Type::Function(vec![], vec![], Box::new(Type::Void))
                ),
                function.type_args.first().unwrap().start,
                function.type_args.last().unwrap().end,
            ));
        }

        if !function.args.is_empty() {
            errors.push(Error::new(
                ErrorType::Type,
                format!(
                    "function \"main\" must have type \"{:?}\"",
                    Type::Function(vec![], vec![], Box::new(Type::Void))
                ),
                function.args.first().unwrap().0.start,
                function.args.last().unwrap().1.end,
            ));
        }

        if function.return_type.inner != Type::Void {
            errors.push(Error::new(
                ErrorType::Type,
                format!(
                    "function \"main\" must have type \"{:?}\"",
                    Type::Function(vec![], vec![], Box::new(Type::Void))
                ),
                function.return_type.start,
                function.return_type.end,
            ));
        }
    }

    errors
}
