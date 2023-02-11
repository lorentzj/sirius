use serde::Serialize;

#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum ErrorType {
    ParseError,
    TypeError,
    UnboundIdentifierError,
    RuntimeError,
    NotImplementedError,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct Error {
    pub error_type: ErrorType,
    pub message: String,
    pub start: usize,
    pub end: usize,
}

impl Error {
    pub fn new(error_type: ErrorType, message: String, start: usize, end: usize) -> Error {
        Error {
            error_type,
            message,
            start,
            end,
        }
    }
}

pub fn cardinal(n: usize) -> String {
    if n % 10 == 1 {
        "1st".into()
    } else if n % 10 == 2 {
        "2nd".into()
    } else if n % 10 == 3 {
        "3rd".into()
    } else {
        format!("{n}th",)
    }
}
