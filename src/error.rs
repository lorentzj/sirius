use std::fmt;

use crate::parser::Pos;
use lalrpop_util::ParseError;

use crate::parser::Tok;
pub type Errors = Vec<Error>;

#[doc(hidden)]
#[macro_export]
macro_rules! __error_at {
    ($e_type:ident, $pos:expr, $($msg:tt)+) => {{
        $crate::error::Error::new_at(
            $crate::error::Er::new($crate::error::ErrorType::$e_type, format!($($msg)*)),
            $pos,
        )
    }};
}

pub use crate::__error_at as error_at;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorType {
    Syntax,
    NameResolution,
    Flow,
    Mutation,
    Type,
    NotImplmented
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorType::Syntax => write!(f, "Syntax"),
            ErrorType::NameResolution => write!(f, "NameResolution"),
            ErrorType::Flow => write!(f, "Flow"),
            ErrorType::Mutation => write!(f, "Mutation"),
            ErrorType::Type => write!(f, "Type"),
            ErrorType::NotImplmented => write!(f, "NotImplmented"),
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
            ParseError::InvalidToken { location } => Error::new(
                location,
                Er {
                    error_type: ErrorType::Syntax,
                    message: "invalid token".into(),
                },
                location + 1,
            ),
            ParseError::UnrecognizedEof { location, .. } => Error::new(
                location - 1,
                Er {
                    error_type: ErrorType::Syntax,
                    message: "unexpected EOF".into(),
                },
                location,
            ),
            ParseError::UnrecognizedToken { token, .. } | ParseError::ExtraToken { token } => {
                Error::new(
                    token.0,
                    Er {
                        error_type: ErrorType::Syntax,
                        message: match token.1 {
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
                    },
                    token.0 + 1,
                )
            }
            ParseError::User { error } => error,
        }
    }
}
