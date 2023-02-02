use super::match_brackets::*;
use crate::error::Error;
//use crate::lexer::{Op, Token, TokenType};
use serde::Serialize;

type BracketPairs = Vec<(TokenID, TokenID)>;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum TypeExpressionData {
    Identifier(String),
    Tuple(Vec<TypeExpression>),
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct TypeExpression {
    pub data: TypeExpressionData,
    pub start: TokenID,
    pub end: TokenID,
    pub bracket_pairs: BracketPairs,
}

impl TypeExpression {
    fn new(data: TypeExpressionData, start: TokenID, end: TokenID) -> Self {
        TypeExpression {
            data,
            start,
            end,
            bracket_pairs: vec![],
        }
    }
}

pub fn parse_type_expression(
    tokens: &[Token],
    bracket_tree: Vec<BracketTree>,
    start: TokenID,
    end: TokenID,
) -> Result<TypeExpression, Error> {
    if bracket_tree.is_empty() {
        Ok(TypeExpression::new(
            TypeExpressionData::Tuple(vec![]),
            start,
            end,
        ))
    } else {
        let mut bracket_pairs: BracketPairs = vec![];
        let mut lhs: Option<TypeExpression> = None;
        let mut can_append = false;
        let mut comma = false;

        for branch in bracket_tree {
            match branch {
                BracketTree::Token(token_id) => match &tokens[token_id].token_type {
                    TokenType::Op(next_op) => {
                        if lhs.is_none() {
                            return Err(Error::new(
                                ErrorType::ParseError,
                                "unexpected operator".to_string(),
                                vec![token_id],
                            ));
                        } else if let Op::Comma = next_op {
                            comma = true;
                        } else {
                            return Err(Error::new(
                                ErrorType::ParseError,
                                "unexpected operator".to_string(),
                                vec![token_id],
                            ));
                        }
                    }
                    TokenType::Identifier(identifier) => match lhs {
                        Some(mut old_lhs) => {
                            let old_lhs_start = old_lhs.start;

                            let current_identifier = TypeExpression::new(
                                TypeExpressionData::Identifier(identifier.to_string()),
                                token_id,
                                token_id + 1,
                            );

                            if comma {
                                match old_lhs.data {
                                    TypeExpressionData::Tuple(ref mut old_v) => {
                                        if can_append {
                                            let mut v = vec![];
                                            v.append(old_v);
                                            v.push(current_identifier);
                                            lhs = Some(TypeExpression::new(
                                                TypeExpressionData::Tuple(v),
                                                old_lhs_start,
                                                token_id + 1,
                                            ));
                                        } else {
                                            lhs = Some(TypeExpression::new(
                                                TypeExpressionData::Tuple(vec![
                                                    old_lhs,
                                                    current_identifier,
                                                ]),
                                                old_lhs_start,
                                                token_id + 1,
                                            ));
                                            can_append = true;
                                        }
                                    }
                                    TypeExpressionData::Identifier(_) => {
                                        lhs = Some(TypeExpression::new(
                                            TypeExpressionData::Tuple(vec![
                                                old_lhs,
                                                current_identifier,
                                            ]),
                                            old_lhs_start,
                                            token_id + 1,
                                        ));
                                        can_append = true;
                                    }
                                }
                            } else {
                                return Err(Error::new(
                                    ErrorType::ParseError,
                                    "unexpected identifier".to_string(),
                                    vec![token_id],
                                ));
                            }
                            comma = false;
                        }
                        None => {
                            lhs = Some(TypeExpression::new(
                                TypeExpressionData::Identifier(identifier.to_string()),
                                token_id,
                                token_id + 1,
                            ));
                        }
                    },
                    _ => {
                        return Err(Error::new(
                            ErrorType::ParseError,
                            "unexpected token in type annotation".to_string(),
                            vec![token_id],
                        ));
                    }
                },

                BracketTree::Bracket(bracket_type, open_id, subtrees, close_id) => {
                    match bracket_type {
                        BracketType::Paren => {
                            let mut inner_expression =
                                parse_type_expression(tokens, subtrees, open_id, close_id + 1)?;
                            bracket_pairs.append(&mut inner_expression.bracket_pairs);

                            inner_expression.start -= 1;
                            inner_expression.end += 1;

                            match lhs {
                                Some(mut old_lhs) => {
                                    let old_lhs_start = old_lhs.start;
                                    if comma {
                                        match old_lhs.data {
                                            TypeExpressionData::Tuple(ref mut old_v) => {
                                                if can_append {
                                                    let mut v = vec![];
                                                    v.append(old_v);
                                                    v.push(inner_expression);
                                                    lhs = Some(TypeExpression::new(
                                                        TypeExpressionData::Tuple(v),
                                                        old_lhs_start,
                                                        close_id + 1,
                                                    ));
                                                } else {
                                                    lhs = Some(TypeExpression::new(
                                                        TypeExpressionData::Tuple(vec![
                                                            old_lhs,
                                                            inner_expression,
                                                        ]),
                                                        old_lhs_start,
                                                        close_id + 1,
                                                    ));
                                                }
                                                can_append = true;
                                            }
                                            TypeExpressionData::Identifier(_) => {
                                                lhs = Some(TypeExpression::new(
                                                    TypeExpressionData::Tuple(vec![
                                                        old_lhs,
                                                        inner_expression,
                                                    ]),
                                                    old_lhs_start,
                                                    close_id + 1,
                                                ));
                                                can_append = true;
                                            }
                                        }
                                    } else {
                                        return Err(Error::new(
                                            ErrorType::ParseError,
                                            "unexpected tuple".to_string(),
                                            vec![open_id],
                                        ));
                                    }
                                }

                                None => {
                                    lhs = Some(inner_expression);
                                    can_append = false;
                                }
                            }
                        }
                        _ => {
                            return Err(Error::new(
                                ErrorType::ParseError,
                                "unexpected brackets in type annotation".to_string(),
                                vec![open_id, close_id],
                            ));
                        }
                    }
                    bracket_pairs.push((open_id, close_id));
                    comma = false;
                }
            }
        }

        if comma {
            return Err(Error::new(
                ErrorType::ParseError,
                "unexpected operator".to_string(),
                vec![end - 1],
            ));
        }

        match lhs {
            Some(mut expression) => {
                expression.bracket_pairs.append(&mut bracket_pairs);
                Ok(expression)
            }
            None => Err(Error::new(
                ErrorType::ParseError,
                "expected expression".to_string(),
                (start..end).collect(),
            )),
        }
    }
}
