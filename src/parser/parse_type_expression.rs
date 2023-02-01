use super::match_brackets::*;
use crate::error::Error;
//use crate::lexer::{Op, Token, TokenType};
use serde::Serialize;

type BracketPairs = Vec<(TokenID, TokenID)>;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum TypeExpression {
    Identifier(String),
    Tuple(Vec<TypeExpression>),
}

pub fn parse_type_expression(
    tokens: &[Token],
    bracket_tree: Vec<BracketTree>,
    start: TokenID,
    end: TokenID,
) -> Result<(TypeExpression, BracketPairs), Error> {
    if bracket_tree.is_empty() {
        Ok((TypeExpression::Tuple(vec![]), vec![]))
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
                        Some(TypeExpression::Tuple(ref mut old_v)) => {
                            if comma {
                                if can_append {
                                    let mut v = vec![];
                                    v.append(old_v);
                                    v.push(TypeExpression::Identifier(identifier.to_string()));
                                    lhs = Some(TypeExpression::Tuple(v));
                                } else {
                                    let mut v = vec![];
                                    v.append(old_v);

                                    lhs = Some(TypeExpression::Tuple(vec![
                                        TypeExpression::Tuple(v),
                                        TypeExpression::Identifier(identifier.to_string()),
                                    ]));
                                    can_append = true;
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
                        Some(TypeExpression::Identifier(ref old_identifier)) => {
                            if comma {
                                lhs = Some(TypeExpression::Tuple(vec![
                                    TypeExpression::Identifier(old_identifier.to_string()),
                                    TypeExpression::Identifier(identifier.to_string()),
                                ]));
                                can_append = true;
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
                            lhs = Some(TypeExpression::Identifier(identifier.to_string()));
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
                            let (inner_expression, mut inner_brackets) =
                                parse_type_expression(tokens, subtrees, open_id, close_id + 1)?;

                            bracket_pairs.append(&mut inner_brackets);

                            match lhs {
                                Some(TypeExpression::Tuple(ref mut old_v)) => {
                                    if comma {
                                        if can_append {
                                            let mut v = vec![];
                                            v.append(old_v);
                                            v.push(inner_expression);
                                            lhs = Some(TypeExpression::Tuple(v));
                                        } else {
                                            let mut v = vec![];
                                            v.append(old_v);

                                            lhs = Some(TypeExpression::Tuple(vec![
                                                TypeExpression::Tuple(v),
                                                inner_expression,
                                            ]));
                                        }
                                        can_append = true;
                                    } else {
                                        return Err(Error::new(
                                            ErrorType::ParseError,
                                            "unexpected tuple".to_string(),
                                            vec![open_id],
                                        ));
                                    }
                                }
                                Some(TypeExpression::Identifier(ref old_identifier)) => {
                                    if comma {
                                        lhs = Some(TypeExpression::Tuple(vec![
                                            TypeExpression::Identifier(old_identifier.to_string()),
                                            inner_expression,
                                        ]));
                                        can_append = true;
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
            Some(expression) => Ok((expression, bracket_pairs)),
            None => Err(Error::new(
                ErrorType::ParseError,
                "expected expression".to_string(),
                (start..end).collect(),
            )),
        }
    }
}
