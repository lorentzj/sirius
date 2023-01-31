use super::match_brackets::*;
use crate::error::Error;
//use crate::lexer::{Op, Token, TokenType};
use serde::Serialize;

type TokenID = usize;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum TypeExpression {
    Identifier(String),
    Tuple(Vec<TypeExpression>),
}

pub fn parse_type_expression(
    _tokens: &[Token],
    _bracket_tree: Vec<BracketTree>,
    _start: TokenID,
    _end: TokenID,
) -> Result<TypeExpression, Error> {
    Ok(TypeExpression::Tuple(vec![]))
}
