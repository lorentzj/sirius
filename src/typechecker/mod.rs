mod types;
use crate::error::{Error, ErrorType};
use crate::parser::ast::{E, Expr};
use crate::solver::Poly;
pub use types::Type;

fn standard_type(name: &str) -> Option<Type> {
    match name {
        "f64" => Some(Type::F64),
        "i64" => Some(Type::I64(None)),
        "bool" => Some(Type::Bool),
        "void" => Some(Type::Void),
        _ => None,
    }
}

pub fn parse_annotation(ann: &Expr, p_vars: &Vec<String>) -> Result<Type, Error> {
    match &ann.data {
        E::Ident(s) => {
            if let Some(t) = standard_type(s) {
                Ok(t)
            } else if p_vars.contains(s) {
                Ok(Type::I64(Some(Poly::var(
                    p_vars.iter().position(|v| v == s).unwrap(),
                    1,
                ))))
            } else {
                Err(Error::new(
                    ErrorType::Type,
                    "Unknown type".to_string(),
                    ann.start,
                    ann.end,
                ))
            }
        }
        E::Tuple(inner) => {
            let mut types = vec![];
            for e in inner {
                types.push(parse_annotation(e, p_vars)?);
            }
            Ok(Type::Tuple(types))
        }

        _ => Err(Error::new(
            ErrorType::Type,
            "Invalid annotation".to_string(),
            ann.start,
            ann.end,
        )),
    }
}
