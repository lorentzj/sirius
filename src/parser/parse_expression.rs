use super::match_brackets::*;
use crate::error::Error;
use crate::lexer::{Op, Token, TokenType};
use serde::Serialize;

type TokenID = usize;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum ExpressionData {
    Constant(f64),
    Identifier(String),
    BinaryOp(Box<Expression>, Op, Box<Expression>),
    Tuple(Vec<Expression>),
    FnCall(Box<Expression>, Vec<Expression>),
    Accessor(Box<Expression>, Vec<Expression>),
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Expression {
    pub data: ExpressionData,
    pub start: TokenID,
    pub end: TokenID,
    pub bracket_pairs: Vec<(TokenID, TokenID)>,
    pub allow_rotation: bool,
}

impl Expression {
    fn new(data: ExpressionData, start: TokenID, end: TokenID, allow_rotation: bool) -> Self {
        Expression {
            data,
            start,
            end,
            bracket_pairs: vec![],
            allow_rotation,
        }
    }
}

fn precedence(op: &Op) -> i32 {
    match op {
        Op::Comma => 0,
        Op::Plus => 1,
        Op::Minus => 1,
        Op::Mul => 2,
        Op::Div => 2,
        Op::Exp => 3,
        Op::Dot => 4,
    }
}

pub fn rotate_binary_op(lhs: Expression, op: Op, rhs: Expression) -> Expression {
    let lhs_start = lhs.start;
    let rhs_end = rhs.end;
    match &lhs.data {
        ExpressionData::BinaryOp(inner_lhs, inner_op, inner_rhs) => {
            if precedence(&op) > precedence(inner_op) && lhs.allow_rotation {
                Expression::new(
                    ExpressionData::BinaryOp(
                        Box::new(*inner_lhs.to_owned()),
                        *inner_op,
                        Box::new(rotate_binary_op(*inner_rhs.to_owned(), op, rhs)),
                    ),
                    lhs_start,
                    rhs_end,
                    true,
                )
            } else {
                match op {
                    Op::Comma => {
                        if lhs.allow_rotation && let ExpressionData::Tuple(mut v) = lhs.data {
                            v.push(rhs);
                            Expression::new(
                                ExpressionData::Tuple(v),
                                lhs_start,
                                rhs_end,
                                true
                            )
                        } else {
                            Expression::new(
                                ExpressionData::Tuple(vec![lhs, rhs]),
                                lhs_start,
                                rhs_end,
                                true
                            )
                        }
                    },
                    _ => Expression::new(
                        ExpressionData::BinaryOp(Box::new(lhs), op, Box::new(rhs)),
                        lhs_start,
                        rhs_end,
                        true,
                    )
                }
            }
        }

        ExpressionData::Tuple(v) => {
            if precedence(&op) > precedence(&Op::Comma) {
                let mut v = v.to_owned();
                let last = v.pop().unwrap();
                v.push(rotate_binary_op(last, op, rhs));

                Expression::new(ExpressionData::Tuple(v), lhs_start, rhs_end, true)
            } else {
                match op {
                    Op::Comma => {
                        let mut v = v.to_owned();
                        v.push(rhs);
                        Expression::new(ExpressionData::Tuple(v), lhs_start, rhs_end, true)
                    }
                    _ => Expression::new(
                        ExpressionData::BinaryOp(Box::new(lhs), op, Box::new(rhs)),
                        lhs_start,
                        rhs_end,
                        true,
                    ),
                }
            }
        }

        _ => match op {
            Op::Comma => Expression::new(
                ExpressionData::Tuple(vec![lhs, rhs]),
                lhs_start,
                rhs_end,
                true,
            ),
            _ => Expression::new(
                ExpressionData::BinaryOp(Box::new(lhs), op, Box::new(rhs)),
                lhs_start,
                rhs_end,
                true,
            ),
        },
    }
}

pub fn parse_expression(
    tokens: &[Token],
    bracket_tree: Vec<BracketTree>,
    start: TokenID,
    end: TokenID,
) -> Result<Expression, Error> {
    if bracket_tree.is_empty() {
        return Ok(Expression::new(
            ExpressionData::Tuple(vec![]),
            start,
            end,
            false,
        ));
    }

    let mut bracket_pairs: Vec<(TokenID, TokenID)> = vec![];
    let mut op: Option<Op> = None;
    let mut lhs: Option<Expression> = None;

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
                    }
                    if op.is_none() {
                        op = Some(*next_op)
                    } else {
                        return Err(Error::new(
                            ErrorType::ParseError,
                            "unexpected operator".to_string(),
                            vec![token_id],
                        ));
                    }
                }

                TokenType::Constant(constant) => {
                    let rhs = Expression::new(
                        ExpressionData::Constant(*constant),
                        token_id,
                        token_id + 1,
                        false,
                    );

                    if let Some(op) = op {
                        if let Some(old_lhs) = lhs {
                            lhs = Some(rotate_binary_op(old_lhs, op, rhs));
                        } else {
                            lhs = Some(rhs);
                        }
                    } else if lhs.is_none() {
                        lhs = Some(rhs);
                    } else {
                        return Err(Error::new(
                            ErrorType::ParseError,
                            "expected operator".to_string(),
                            vec![token_id],
                        ));
                    }
                    op = None;
                }

                TokenType::Identifier(identifier) => {
                    let rhs = Expression::new(
                        ExpressionData::Identifier(identifier.to_string()),
                        token_id,
                        token_id + 1,
                        false,
                    );

                    if let Some(op) = op {
                        if let Some(old_lhs) = lhs {
                            lhs = Some(rotate_binary_op(old_lhs, op, rhs));
                        } else {
                            lhs = Some(rhs);
                        }
                    } else if lhs.is_none() {
                        lhs = Some(rhs);
                    } else {
                        return Err(Error::new(
                            ErrorType::ParseError,
                            "expected operator".to_string(),
                            vec![token_id],
                        ));
                    }
                    op = None;
                }
                TokenType::Keyword(_) => {
                    return Err(Error::new(
                        ErrorType::ParseError,
                        "unexpected keyword".to_string(),
                        vec![token_id],
                    ));
                }

                _ => {
                    return Err(Error::new(
                        ErrorType::ParseError,
                        "unexpected token".to_string(),
                        vec![token_id],
                    ));
                }
            },

            BracketTree::Bracket(bracket_type, open_id, subtrees, close_id) => {
                let mut inner_expression =
                    parse_expression(tokens, subtrees, open_id, close_id + 1)?;
                inner_expression.allow_rotation = false;

                bracket_pairs.extend(inner_expression.bracket_pairs.iter());

                if bracket_type == BracketType::Paren {
                    if let Some(op) = op {
                        if let Some(old_lhs) = lhs {
                            lhs = Some(rotate_binary_op(old_lhs, op, inner_expression));
                        } else {
                            lhs = Some(inner_expression);
                        }
                    } else if lhs.is_none() {
                        lhs = Some(inner_expression);
                    } else {
                        return Err(Error::new(
                            ErrorType::ParseError,
                            "expected operator".to_string(),
                            vec![open_id],
                        ));
                    }
                }

                bracket_pairs.push((open_id, close_id));
                op = None;
            }
        }
    }

    match lhs {
        Some(mut expression) => {
            if op.is_some() {
                return Err(Error::new(
                    ErrorType::ParseError,
                    "missing operand".to_string(),
                    vec![end - 1],
                ));
            }

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
