use std::fmt;

use crate::parser::Pos;
use lalrpop_util::ParseError;

use crate::parser::Tok;
pub type Errors = Vec<Error>;

#[doc(hidden)]
#[macro_export]
macro_rules! __error_at {
    ($pos:expr, $e_type:ident, $msg:expr) => {{ $pos.error($crate::error::ErrorType::$e_type, $msg) }};
}

pub use crate::__error_at as error_at;

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
pub struct Er {
    pub error_type: ErrorType,
    pub message: String,
}

pub type Error = Pos<Er>;

impl Error {
    pub fn new_with(error_type: ErrorType, message: String, start: usize, end: usize) -> Error {
        Error {
            start,
            data: Er {
                error_type,
                message,
            },
            end,
        }
    }

    pub fn from_lalrpop(err: ParseError<usize, Tok, Error>) -> Error {
        match err {
            ParseError::InvalidToken { location } => Error::new_with(
                ErrorType::Syntax,
                "invalid token".into(),
                location,
                location,
            ),
            ParseError::UnrecognizedEof { location, .. } => Error::new_with(
                ErrorType::Syntax,
                "unexpected EOF".into(),
                location - 1,
                location - 1,
            ),
            ParseError::UnrecognizedToken { token, .. } | ParseError::ExtraToken { token } => {
                Error::new_with(
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
}
