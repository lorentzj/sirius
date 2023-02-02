mod match_brackets;
mod parse_expression;
mod parse_type_expression;

use crate::error::{Error, ErrorType};
use crate::lexer::{lex, Keyword, Token, TokenType};
pub use crate::parser::match_brackets::TokenID;
use crate::parser::match_brackets::{match_brackets, BracketType};
pub use crate::parser::parse_expression::{parse_expression, Expression, ExpressionData};
pub use crate::parser::parse_type_expression::{
    parse_type_expression, TypeExpression, TypeExpressionData,
};

use serde::Serialize;

type TypeAnnotationUnparsed = Option<(TokenID, TokenID)>;

#[derive(Debug, PartialEq, Serialize)]
pub enum Statement {
    Print(Expression),
    Let(String, Option<TypeExpression>, Expression),
}

#[derive(Debug, PartialEq, Serialize)]
pub struct ParserOutput {
    pub tokens: Vec<Token>,
    pub statements: Vec<Statement>,
    pub bracket_pairs: Vec<(TokenID, TokenID)>,
    pub type_tokens: Vec<TokenID>,
    pub errors: Vec<Error>,
}

fn consume_print(
    tokens: &Vec<Token>,
    start: TokenID,
) -> Result<(Expression, TokenID), (Error, TokenID)> {
    if start >= tokens.len() {
        return Err((
            Error::new(
                ErrorType::ParseError,
                "expected expression".to_string(),
                vec![tokens.len() - 1],
            ),
            tokens.len(),
        ));
    }

    let mut i = start;
    while i < tokens.len() {
        match tokens[i].token_type {
            TokenType::Semicolon => break,
            _ => i += 1,
        }
    }

    if start == i {
        return Err((
            Error::new(
                ErrorType::ParseError,
                "expected expression".to_string(),
                vec![tokens.len() - 1],
            ),
            tokens.len(),
        ));
    }

    if i == tokens.len() {
        return Err((
            Error::new(
                ErrorType::ParseError,
                "expected semicolon after expression".to_string(),
                vec![tokens.len() - 1],
            ),
            tokens.len(),
        ));
    }

    match match_brackets(tokens, start, i, BracketType::None) {
        Ok((bracket_tree, _)) => match parse_expression(tokens, bracket_tree, start, i) {
            Ok(expression) => Ok((expression, i + 1)),
            Err(error) => Err((error, i + 1)),
        },
        Err(error) => Err((error, i + 1)),
    }
}

// let [identifier] (: [type annotation])? = [expression];
enum LetStatementParserState {
    Start,
    Identifier(String),
    Colon(String),
    TypeAnnotation(String, TypeAnnotationUnparsed),
    Assign(String, TypeAnnotationUnparsed),
    Expression(String, TypeAnnotationUnparsed, usize),
    Done(String, TypeAnnotationUnparsed, Expression),
}

fn consume_let_token(
    tokens: &[Token],
    i: TokenID,
    state: LetStatementParserState,
) -> Result<LetStatementParserState, (Error, TokenID)> {
    match state {
        LetStatementParserState::Start => {
            if let TokenType::Identifier(identifier) = &tokens[i].token_type {
                Ok(LetStatementParserState::Identifier(identifier.to_owned()))
            } else {
                Err((
                    Error::new(
                        ErrorType::ParseError,
                        "expected identifier".to_string(),
                        vec![i],
                    ),
                    i + 1,
                ))
            }
        }
        LetStatementParserState::Identifier(identifier) => {
            if let TokenType::Colon = &tokens[i].token_type {
                Ok(LetStatementParserState::Colon(identifier))
            } else if let TokenType::Assign = &tokens[i].token_type {
                Ok(LetStatementParserState::Assign(identifier, None))
            } else {
                Err((
                    Error::new(
                        ErrorType::ParseError,
                        "expected \"=\" or \":\"".to_string(),
                        vec![i],
                    ),
                    i + 1,
                ))
            }
        }
        LetStatementParserState::Colon(identifier) => match &tokens[i].token_type {
            TokenType::Assign => Err((
                Error::new(
                    ErrorType::ParseError,
                    "expected type annotation".to_string(),
                    vec![i],
                ),
                i + 1,
            )),
            TokenType::Semicolon => Err((
                Error::new(
                    ErrorType::ParseError,
                    "expected type annotation".to_string(),
                    vec![i],
                ),
                i + 1,
            )),
            _ => Ok(LetStatementParserState::TypeAnnotation(
                identifier,
                Some((i, i + 1)),
            )),
        },
        LetStatementParserState::TypeAnnotation(identifier, type_annotation) => {
            match &tokens[i].token_type {
                TokenType::Assign => {
                    Ok(LetStatementParserState::Assign(identifier, type_annotation))
                }
                TokenType::Semicolon => Err((
                    Error::new(ErrorType::ParseError, "expected \"=\"".to_string(), vec![i]),
                    i + 1,
                )),
                _ => {
                    let mut new_annotation = type_annotation.unwrap();
                    new_annotation = (new_annotation.0, new_annotation.1 + 1);
                    Ok(LetStatementParserState::TypeAnnotation(
                        identifier,
                        Some(new_annotation),
                    ))
                }
            }
        }
        LetStatementParserState::Assign(identifier, type_annotation) => {
            match &tokens[i].token_type {
                TokenType::Semicolon => Err((
                    Error::new(
                        ErrorType::ParseError,
                        "expected expression".to_string(),
                        vec![i],
                    ),
                    i + 1,
                )),
                _ => Ok(LetStatementParserState::Expression(
                    identifier,
                    type_annotation,
                    i,
                )),
            }
        }
        LetStatementParserState::Expression(identifier, type_annotation, start_expression) => {
            match &tokens[i].token_type {
                TokenType::Semicolon => {
                    let expression: Result<Expression, Error> =
                        match match_brackets(tokens, start_expression, i, BracketType::None) {
                            Ok((bracket_tree, _)) => {
                                match parse_expression(tokens, bracket_tree, start_expression, i) {
                                    Ok(expression) => Ok(expression),
                                    Err(error) => Err(error),
                                }
                            }
                            Err(error) => Err(error),
                        };

                    match expression {
                        Ok(expression) => Ok(LetStatementParserState::Done(
                            identifier,
                            type_annotation,
                            expression,
                        )),
                        Err(error) => Err((error, i)),
                    }
                }

                _ => Ok(LetStatementParserState::Expression(
                    identifier,
                    type_annotation,
                    start_expression,
                )),
            }
        }
        _ => Ok(state),
    }
}

fn consume_let(
    tokens: &Vec<Token>,
    start: TokenID,
) -> Result<(String, TypeAnnotationUnparsed, Expression, TokenID), (Error, TokenID)> {
    let mut state = LetStatementParserState::Start;
    let mut i = start;
    while i < tokens.len() {
        match consume_let_token(tokens, i, state) {
            Ok(LetStatementParserState::Done(identifier, type_annotation, expression)) => {
                state = LetStatementParserState::Done(identifier, type_annotation, expression);
                break;
            }
            Ok(next) => {
                state = next;
            }
            Err((error, end_i)) => return Err((error, end_i)),
        }
        i += 1;
    }

    if let LetStatementParserState::Done(identifier, type_annotation, expression) = state {
        Ok((identifier, type_annotation, expression, i + 1))
    } else if i == tokens.len() {
        Err((
            Error::new(
                ErrorType::ParseError,
                "unexpected EOF".to_string(),
                vec![i - 1],
            ),
            i,
        ))
    } else {
        Err((
            Error::new(
                ErrorType::InternalError,
                "unexpected end of statement".to_string(),
                vec![i - 1],
            ),
            i,
        ))
    }
}

pub fn parse(code: &str) -> ParserOutput {
    let lexed = lex(code);
    let mut bracket_pairs: Vec<(TokenID, TokenID)> = vec![];
    let mut type_tokens: Vec<TokenID> = vec![];

    if !lexed.errors.is_empty() {
        ParserOutput {
            tokens: lexed.tokens,
            statements: vec![],
            bracket_pairs,
            type_tokens,
            errors: lexed.errors,
        }
    } else {
        let mut errors: Vec<Error> = vec![];
        let mut statements: Vec<Statement> = vec![];
        let mut i = 0;
        let mut extend_error = false;

        while i < lexed.tokens.len() {
            if let TokenType::Keyword(keyword) = lexed.tokens[i].token_type {
                match keyword {
                    Keyword::Print => {
                        extend_error = false;
                        match consume_print(&lexed.tokens, i + 1) {
                            Ok((mut expression, new_i)) => {
                                bracket_pairs.append(&mut expression.bracket_pairs);
                                statements.push(Statement::Print(expression));
                                i = new_i;
                            }
                            Err((error, new_i)) => {
                                errors.push(error);
                                i = new_i;
                            }
                        }
                    }

                    Keyword::Let => {
                        extend_error = false;
                        match consume_let(&lexed.tokens, i + 1) {
                            Ok((identifier, expression_type_bounds, mut expression, new_i)) => {
                                bracket_pairs.append(&mut expression.bracket_pairs);
                                match expression_type_bounds {
                                    Some((type_expression_start, type_expression_end)) => {
                                        type_tokens
                                            .extend(type_expression_start..type_expression_end);

                                        match match_brackets(
                                            &lexed.tokens,
                                            type_expression_start,
                                            type_expression_end,
                                            BracketType::None,
                                        ) {
                                            Ok((bracket_tree, _)) => {
                                                match parse_type_expression(
                                                    &lexed.tokens,
                                                    bracket_tree,
                                                    type_expression_start,
                                                    type_expression_end,
                                                ) {
                                                    Ok(mut type_expression) => {
                                                        bracket_pairs.append(
                                                            &mut type_expression.bracket_pairs,
                                                        );
                                                        statements.push(Statement::Let(
                                                            identifier,
                                                            Some(type_expression),
                                                            expression,
                                                        ));
                                                        i = new_i;
                                                    }
                                                    Err(error) => {
                                                        errors.push(error);
                                                        i = new_i;
                                                    }
                                                }
                                            }
                                            Err(error) => {
                                                errors.push(error);
                                                i = new_i;
                                            }
                                        };
                                    }
                                    None => {
                                        statements
                                            .push(Statement::Let(identifier, None, expression));
                                        i = new_i;
                                    }
                                }
                            }
                            Err((error, new_i)) => {
                                errors.push(error);
                                i = new_i;
                            }
                        }
                    }
                }
            } else {
                if extend_error {
                    let last_error = errors.len() - 1;
                    errors[last_error].tokens.push(i);
                } else {
                    errors.push(Error::new(
                        ErrorType::ParseError,
                        "expected statement".to_string(),
                        vec![i],
                    ));
                    extend_error = true;
                }
                i += 1;
            }
        }

        ParserOutput {
            tokens: lexed.tokens,
            statements,
            bracket_pairs,
            type_tokens,
            errors,
        }
    }
}
