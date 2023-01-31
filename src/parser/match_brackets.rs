pub use crate::error::{Error, ErrorType};
pub use crate::lexer::{Op, Token, TokenType};
use serde::Serialize;

pub type TokenID = usize;

#[derive(Debug, PartialEq, Eq, Serialize, Clone)]
pub enum BracketType {
    None,
    Paren,
    SqBracket,
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum BracketTree {
    Token(TokenID),
    Bracket(BracketType, TokenID, Vec<BracketTree>, TokenID),
}

pub fn match_brackets(
    tokens: &[Token],
    start: TokenID,
    end: TokenID,
    expecting: BracketType,
) -> Result<(Vec<BracketTree>, TokenID), Error> {
    if start >= tokens.len() {
        if expecting == BracketType::SqBracket {
            return Err(Error {
                error_type: ErrorType::BracketError,
                message: "expected close square bracket".to_string(),
                tokens: vec![start - 1],
            });
        } else if expecting == BracketType::Paren {
            return Err(Error {
                error_type: ErrorType::BracketError,
                message: "expected close parenthesis".to_string(),
                tokens: vec![start - 1],
            });
        }
    }

    let mut ret: Vec<BracketTree> = Vec::new();
    let mut i = start;

    while i < end {
        match tokens[i].token_type {
            TokenType::OpenParen => {
                let (bracket_tokens, end) = match_brackets(tokens, i + 1, end, BracketType::Paren)?;
                ret.push(BracketTree::Bracket(
                    BracketType::Paren,
                    i,
                    bracket_tokens,
                    end,
                ));
                i = end + 1;
            }
            TokenType::CloseParen => {
                if expecting == BracketType::Paren {
                    return Ok((ret, i));
                } else {
                    return Err(Error::new(
                        ErrorType::BracketError,
                        "unexpected parenthesis".to_string(),
                        vec![i],
                    ));
                }
            }
            TokenType::OpenSqBracket => {
                let (bracket_tokens, end) =
                    match_brackets(tokens, i + 1, end, BracketType::SqBracket)?;
                ret.push(BracketTree::Bracket(
                    BracketType::SqBracket,
                    i,
                    bracket_tokens,
                    end,
                ));
                i = end + 1;
            }
            TokenType::CloseSqBracket => {
                if expecting == BracketType::SqBracket {
                    return Ok((ret, i));
                } else {
                    return Err(Error::new(
                        ErrorType::BracketError,
                        "unexpected square bracket".to_string(),
                        vec![i],
                    ));
                }
            }

            _ => {
                ret.push(BracketTree::Token(i));
                i += 1;
            }
        }
    }

    match expecting {
        BracketType::None => Ok((ret, i)),
        BracketType::SqBracket => {
            if end == tokens.len() {
                i -= 1;
            }
            Err(Error {
                error_type: ErrorType::BracketError,
                message: "expected close square bracket".to_string(),
                tokens: vec![start - 1, i],
            })
        }
        BracketType::Paren => {
            if end == tokens.len() {
                i -= 1;
            }
            Err(Error {
                error_type: ErrorType::BracketError,
                message: "expected close parenthesis".to_string(),
                tokens: vec![start - 1, i],
            })
        }
    }
}
