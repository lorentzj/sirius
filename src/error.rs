use std::fmt;

use crate::parser::Pos;
use lalrpop_util::ParseError;

use crate::parser::Tok;
pub type Errors = Vec<Error>;

#[doc(hidden)]
#[macro_export]
macro_rules! __error_at {
    ($e_type:ident, $msg:expr, $pos:expr $(,)?) => {{
        Error::new_at(
            $crate::error::Er::new($crate::error::ErrorType::$e_type, $msg),
            $pos,
        )
    }};
    ($e_type:ident, $msg:expr, $from:expr, $to:expr $(,)?) => {{
        Error::new(
            $from,
            $crate::error::Er::new($crate::error::ErrorType::$e_type, $msg),
            $to,
        )
    }};
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

impl Er {
    pub fn new<T: ToString>(error_type: ErrorType, message: T) -> Self {
        Self {
            error_type,
            message: message.to_string(),
        }
    }
}

pub type Error = Pos<Er>;

impl Error {
    pub fn from_lalrpop(err: ParseError<usize, Tok, Error>) -> Error {
        match err {
            ParseError::InvalidToken { location } => {
                error_at!(Syntax, "invalid token", location, location,)
            }
            ParseError::UnrecognizedEof { location, .. } => {
                error_at!(Syntax, "unexpected EOF", location - 1, location - 1,)
            }
            ParseError::UnrecognizedToken { token, .. } | ParseError::ExtraToken { token } => {
                error_at!(
                    Syntax,
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
