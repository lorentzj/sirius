use serde::Serialize;

#[derive(Debug, PartialEq, Eq, Serialize)]
pub enum ErrorType {
    Syntax,
    Type,
    Name,
    UnboundIdentifier,
    Flow,
    Mutation,
    Constraint,
    Runtime,
    NotImplemented,
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
