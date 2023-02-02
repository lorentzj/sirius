use serde::Serialize;

#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum ErrorType {
    TokenError,
    BracketError,
    ParseError,
    UnboundIdentifierError,
    TypeError,
    NotImplementedError,
    RuntimeError,
    InternalError,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct Error {
    pub error_type: ErrorType,
    pub message: String,
    pub tokens: Vec<usize>,
}

impl Error {
    pub fn new(error_type: ErrorType, message: String, tokens: Vec<usize>) -> Error {
        Error {
            error_type,
            message,
            tokens,
        }
    }
}
