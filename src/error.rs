use std::fmt;

use lalrpop_util::ParseError;

use crate::parser::Tok;
use crate::parser::Pos;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorType {
    Syntax,
    Annotation,
    NameResolution,
    Type,
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorType::Syntax => write!(f, "Syntax"),
            ErrorType::Annotation => write!(f, "Annotation"),
            ErrorType::NameResolution => write!(f, "NameResolution"),
            ErrorType::Type => write!(f, "Type"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

    pub fn new_ann(start: usize, message: String, end: usize) -> Error {
        Error {
            error_type: ErrorType::Annotation,
            message,
            start,
            end,
        }
    }

    pub fn new_ann_default_msg(start: usize, end: usize) -> Error {
        Error {
            error_type: ErrorType::Annotation,
            message: "illegal expression in annotation".into(),
            start,
            end,
        }
    }

    pub fn new_name_res(start: usize, message: String, end: usize) -> Error {
        Error {
            error_type: ErrorType::NameResolution,
            message,
            start,
            end,
        }
    }

    pub fn from_lalrpop(err: ParseError<usize, Tok, Error>) -> Error {
        match err {
            ParseError::InvalidToken { location } => Error::new(
                ErrorType::Syntax,
                "invalid token".into(),
                location,
                location,
            ),
            ParseError::UnrecognizedEof { location, .. } => Error::new(
                ErrorType::Syntax,
                "unexpected EOF".into(),
                location - 1,
                location - 1,
            ),
            ParseError::UnrecognizedToken { token, .. } | ParseError::ExtraToken { token } => {
                Error::new(
                    ErrorType::Syntax,
                    match token.1 {
                        Tok::Identifier(n) => format!("unexpected identifier \"{n}\""),
                        Tok::Op(op) => format!("unexpected operator \"{op:?}\""),
                        Tok::Keyword(k) => format!("unexpected keyword \"{k:?}\""),
                        Tok::Float(_) | Tok::Int(_) => "unexpected constant".into(),
                        Tok::Indent => "unexpected indent".into(),
                        Tok::Dedent => "unexpected dedent".into(),
                        Tok::Error(m) => m,
                        Tok::IndentError(m) => m,
                        _ => format!("unexpected token \"{:?}\"", token.1),
                    },
                    token.0,
                    token.0,
                )
            }
            ParseError::User { error } => error,
        }
    }

    pub fn type_from_expr<T>(expr: &Pos<T>, message: &str) -> Error {
        Error::new(
            ErrorType::Type,
            message.to_string(),
            expr.start,
            expr.end - 1,
        )
    }
}
