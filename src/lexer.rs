use crate::error::{Error, ErrorType};

use serde::Serialize;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize)]
pub enum Op {
    Comma,
    Exp,
    Mul,
    Div,
    Plus,
    Minus,
    Dot,
}

#[derive(Debug, PartialEq, Eq, Serialize, Clone, Copy)]
pub enum Keyword {
    Print,
    Let,
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum TokenType {
    OpenParen,
    CloseParen,
    OpenSqBracket,
    CloseSqBracket,
    Op(Op),
    Constant(f64),
    Identifier(String),
    Keyword(Keyword),
    Assign,
    Semicolon,
    Colon,
    Error,
}

fn parse_substring(s: &str) -> Result<TokenType, String> {
    return if s == "let" {
        Ok(TokenType::Keyword(Keyword::Let))
    } else if s == "print" {
        Ok(TokenType::Keyword(Keyword::Print))
    } else {
        match s.chars().next() {
            Some('0'..='9') => {
                let parsed_number = String::from_iter(s.chars()).parse::<f64>();
                match parsed_number {
                    Ok(n) => {
                        if n.is_infinite() {
                            Err("number overflowed f64".to_string())
                        } else {
                            Ok(TokenType::Constant(n))
                        }
                    }
                    Err(msg) => Err(msg.to_string()),
                }
            }
            _ => match s {
                "let" => Ok(TokenType::Keyword(Keyword::Let)),
                "print" => Ok(TokenType::Keyword(Keyword::Print)),
                _ => Ok(TokenType::Identifier(s.to_string())),
            },
        }
    };
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl Token {
    fn new(token_type: TokenType, line: usize, start: usize, end: usize) -> Token {
        Token {
            token_type,
            line,
            start,
            end,
        }
    }
}

#[derive(Debug, PartialEq, Serialize)]
pub struct LexerOutput {
    pub tokens: Vec<Token>,
    pub errors: Vec<Error>,
}

pub fn lex(text: &str) -> LexerOutput {
    let mut tokens: Vec<Token> = vec![];
    let mut errors: Vec<Error> = vec![];
    let mut curr_substring = String::new();
    let mut line = 0;
    let mut col = 0;

    for char in text.chars() {
        match char {
            '0'..='9' | 'A'..='Z' | 'a'..='z' | '\\' | '\u{0370}'..='\u{03FF}' | '_' => {
                curr_substring.push(char);
            }

            _ => {
                if !curr_substring.is_empty() {
                    if char == '.' {
                        let all_digits = curr_substring.bytes().all(|c| c.is_ascii_digit());
                        if all_digits {
                            curr_substring.push(char);
                            col += 1;
                            continue;
                        }
                    }

                    match parse_substring(&curr_substring) {
                        Ok(token_type) => tokens.push(Token::new(
                            token_type,
                            line,
                            col - curr_substring.len(),
                            col,
                        )),

                        Err(msg) => {
                            tokens.push(Token::new(
                                TokenType::Error,
                                line,
                                col - curr_substring.len(),
                                col,
                            ));

                            errors.push(Error::new(
                                ErrorType::TokenError,
                                msg,
                                vec![tokens.len() - 1],
                            ))
                        }
                    }
                    curr_substring = "".to_string();
                }

                match char {
                    ' ' | '\t' | '\n' => (),
                    ';' => tokens.push(Token::new(TokenType::Semicolon, line, col, col + 1)),
                    ':' => tokens.push(Token::new(TokenType::Colon, line, col, col + 1)),
                    '(' => tokens.push(Token::new(TokenType::OpenParen, line, col, col + 1)),
                    ')' => tokens.push(Token::new(TokenType::CloseParen, line, col, col + 1)),
                    '[' => tokens.push(Token::new(TokenType::OpenSqBracket, line, col, col + 1)),
                    ']' => tokens.push(Token::new(TokenType::CloseSqBracket, line, col, col + 1)),
                    '.' => tokens.push(Token::new(TokenType::Op(Op::Dot), line, col, col + 1)),
                    '^' => tokens.push(Token::new(TokenType::Op(Op::Exp), line, col, col + 1)),
                    '*' => tokens.push(Token::new(TokenType::Op(Op::Mul), line, col, col + 1)),
                    '/' => tokens.push(Token::new(TokenType::Op(Op::Div), line, col, col + 1)),
                    '+' => tokens.push(Token::new(TokenType::Op(Op::Plus), line, col, col + 1)),
                    '-' => tokens.push(Token::new(TokenType::Op(Op::Minus), line, col, col + 1)),
                    ',' => tokens.push(Token::new(TokenType::Op(Op::Comma), line, col, col + 1)),
                    '=' => tokens.push(Token::new(TokenType::Assign, line, col, col + 1)),

                    _ => {
                        if !tokens.is_empty()
                            && tokens.last().unwrap().token_type == TokenType::Error
                        {
                            let n_tokens = tokens.len();
                            tokens[n_tokens].end += 1;
                        } else {
                            tokens.push(Token::new(TokenType::Error, line, col, col + 1));
                            errors.push(Error::new(
                                ErrorType::TokenError,
                                "unknown token".to_string(),
                                vec![tokens.len() - 1],
                            ));
                        }
                    }
                }
            }
        }

        match char {
            '\n' => {
                line += 1;
                col = 0;
            }
            _ => col += 1,
        }
    }

    if !curr_substring.is_empty() {
        match parse_substring(&curr_substring) {
            Ok(token_type) => tokens.push(Token::new(
                token_type,
                line,
                col - curr_substring.len(),
                col,
            )),

            Err(msg) => {
                tokens.push(Token::new(
                    TokenType::Error,
                    line,
                    col - curr_substring.len(),
                    col,
                ));

                errors.push(Error::new(
                    ErrorType::TokenError,
                    msg,
                    vec![tokens.len() - 1],
                ))
            }
        }
    }

    LexerOutput { tokens, errors }
}
