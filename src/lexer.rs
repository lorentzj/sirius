use serde::Serialize;
use std::fmt;
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Clone, PartialEq, Eq, Serialize)]
pub enum Op {
    Dot,
    Exp,
    Mul,
    Div,
    Add,
    Sub,
    And,
    Or,
    Not,
    Greater,
    Less,
    Equal,
    NotEqual,
    Comma,
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Dot => write!(f, "."),
            Op::Exp => write!(f, "^"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::And => write!(f, "&"),
            Op::Or => write!(f, "|"),
            Op::Not => write!(f, "!"),
            Op::Greater => write!(f, ">"),
            Op::Less => write!(f, "<"),
            Op::Equal => write!(f, "=="),
            Op::NotEqual => write!(f, "=="),
            Op::Comma => write!(f, ","),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Serialize)]
pub enum Keyword {
    Print,
    Let,
    If,
    Else,
    True,
    False,
    Fn,
    Return,
}

impl fmt::Debug for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Keyword::Let => write!(f, "let"),
            Keyword::Print => write!(f, "print"),
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::True => write!(f, "true"),
            Keyword::False => write!(f, "false"),
            Keyword::Fn => write!(f, "fn"),
            Keyword::Return => write!(f, "return"),
        }
    }
}

#[derive(Clone, PartialEq, Serialize)]
pub enum Tok {
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Op(Op),
    Float(f64),
    Identifier(String),
    Keyword(Keyword),
    Assign,
    Semicolon,
    Colon,
    Error(String),
}

impl fmt::Debug for Tok {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Tok::OpenParen => write!(f, "("),
            Tok::CloseParen => write!(f, ")"),
            Tok::OpenCurly => write!(f, "{{"),
            Tok::CloseCurly => write!(f, "}}"),
            Tok::Op(op) => write!(f, "{op:?}"),
            Tok::Float(v) => write!(f, "{v}"),
            Tok::Identifier(n) => write!(f, "{n}"),
            Tok::Keyword(k) => write!(f, "{k:?}"),
            Tok::Assign => write!(f, "="),
            Tok::Semicolon => write!(f, ";"),
            Tok::Colon => write!(f, ":"),
            Tok::Error(m) => write!(f, "error({m})"),
        }
    }
}

fn parse_substring(s: &str) -> Result<Tok, String> {
    return if s == "let" {
        Ok(Tok::Keyword(Keyword::Let))
    } else if s == "print" {
        Ok(Tok::Keyword(Keyword::Print))
    } else if s == "if" {
        Ok(Tok::Keyword(Keyword::If))
    } else if s == "else" {
        Ok(Tok::Keyword(Keyword::Else))
    } else if s == "true" {
        Ok(Tok::Keyword(Keyword::True))
    } else if s == "false" {
        Ok(Tok::Keyword(Keyword::False))
    } else if s == "fn" {
        Ok(Tok::Keyword(Keyword::Fn))
    } else if s == "return" {
        Ok(Tok::Keyword(Keyword::Return))
    } else {
        match s.chars().next() {
            Some('0'..='9') => {
                let parsed_number = String::from_iter(s.chars()).parse::<f64>();
                match parsed_number {
                    Ok(n) => {
                        if n.is_infinite() {
                            Err("number overflowed f64".into())
                        } else {
                            Ok(Tok::Float(n))
                        }
                    }
                    Err(msg) => Err(msg.to_string()),
                }
            }
            _ => match s {
                "let" => Ok(Tok::Keyword(Keyword::Let)),
                "print" => Ok(Tok::Keyword(Keyword::Print)),
                _ => Ok(Tok::Identifier(s.into())),
            },
        }
    };
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Token {
    pub data: Tok,
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl Token {
    fn new(data: Tok, line: usize, start: usize, end: usize) -> Token {
        Token {
            data,
            line,
            start,
            end,
        }
    }
}

pub struct Lexer<'input> {
    code: &'input str,
    char_inds: Peekable<CharIndices<'input>>,
    curr_substring_start: Option<usize>,
    curr_substring_all_digits: bool,
    last_equal: bool,
    line: usize,
    col: usize,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            code: input,
            char_inds: input.char_indices().peekable(),
            curr_substring_start: None,
            curr_substring_all_digits: true,
            last_equal: false,
            line: 0,
            col: 0,
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(&(i, char)) = self.char_inds.peek() {
            match char {
                '0'..='9' | 'A'..='Z' | 'a'..='z' | '\u{0370}'..='\u{03FF}' | '_' => {
                    self.char_inds.next();

                    if self.curr_substring_start.is_none() {
                        self.curr_substring_start = Some(i);
                        self.curr_substring_all_digits &= char.is_ascii_digit();
                    }

                    self.col += 1;

                    if self.last_equal {
                        self.last_equal = false;
                        return Some(Token::new(Tok::Assign, self.line, self.col - 1, self.col));
                    }
                }

                '=' => {
                    if let Some(curr_substring_start) = self.curr_substring_start {
                        if char == '.' && self.curr_substring_all_digits {
                            self.char_inds.next();
                            self.col += 1;
                            continue;
                        }

                        self.curr_substring_all_digits = true;
                        self.curr_substring_start = None;

                        match parse_substring(&self.code[curr_substring_start..i]) {
                            Ok(token_type) => {
                                return Some(Token::new(
                                    token_type,
                                    self.line,
                                    self.col + curr_substring_start - i,
                                    self.col,
                                ))
                            }

                            Err(msg) => {
                                return Some(Token::new(
                                    Tok::Error(msg),
                                    self.line,
                                    self.col + curr_substring_start - i,
                                    self.col,
                                ));
                            }
                        }
                    }

                    self.char_inds.next();

                    if self.last_equal {
                        self.last_equal = false;
                        self.col += 2;
                        return Some(Token::new(
                            Tok::Op(Op::Equal),
                            self.line,
                            self.col - 2,
                            self.col,
                        ));
                    } else {
                        self.last_equal = true;
                    }
                }

                _ => {
                    if let Some(curr_substring_start) = self.curr_substring_start {
                        if char == '.' && self.curr_substring_all_digits {
                            self.char_inds.next();
                            self.col += 1;
                            continue;
                        }

                        self.curr_substring_all_digits = true;
                        self.curr_substring_start = None;

                        match parse_substring(&self.code[curr_substring_start..i]) {
                            Ok(token_type) => {
                                return Some(Token::new(
                                    token_type,
                                    self.line,
                                    self.col + curr_substring_start - i,
                                    self.col,
                                ))
                            }

                            Err(msg) => {
                                return Some(Token::new(
                                    Tok::Error(msg),
                                    self.line,
                                    self.col + curr_substring_start - i,
                                    self.col,
                                ));
                            }
                        }
                    }

                    if self.last_equal {
                        self.last_equal = false;
                        self.col += 1;
                        return Some(Token::new(Tok::Assign, self.line, self.col - 1, self.col));
                    }

                    self.char_inds.next();

                    match char {
                        '\n' => {
                            self.line += 1;
                            self.col = 0;
                        }
                        _ => self.col += 1,
                    }

                    match char {
                        ' ' | '\t' | '\n' => (),
                        ';' => {
                            return Some(Token::new(
                                Tok::Semicolon,
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        ':' => {
                            return Some(Token::new(Tok::Colon, self.line, self.col - 1, self.col))
                        }
                        '(' => {
                            return Some(Token::new(
                                Tok::OpenParen,
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        ')' => {
                            return Some(Token::new(
                                Tok::CloseParen,
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        '{' => {
                            return Some(Token::new(
                                Tok::OpenCurly,
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        '}' => {
                            return Some(Token::new(
                                Tok::CloseCurly,
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        '.' => {
                            return Some(Token::new(
                                Tok::Op(Op::Dot),
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        '^' => {
                            return Some(Token::new(
                                Tok::Op(Op::Exp),
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        '*' => {
                            return Some(Token::new(
                                Tok::Op(Op::Mul),
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        '/' => {
                            return Some(Token::new(
                                Tok::Op(Op::Div),
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        '+' => {
                            return Some(Token::new(
                                Tok::Op(Op::Add),
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        '-' => {
                            return Some(Token::new(
                                Tok::Op(Op::Sub),
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        '&' => {
                            return Some(Token::new(
                                Tok::Op(Op::And),
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        '|' => {
                            return Some(Token::new(
                                Tok::Op(Op::Or),
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        '!' => {
                            return Some(Token::new(
                                Tok::Op(Op::Not),
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        '>' => {
                            return Some(Token::new(
                                Tok::Op(Op::Greater),
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        '<' => {
                            return Some(Token::new(
                                Tok::Op(Op::Less),
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }
                        ',' => {
                            return Some(Token::new(
                                Tok::Op(Op::Comma),
                                self.line,
                                self.col - 1,
                                self.col,
                            ))
                        }

                        _ => {
                            return Some(Token::new(
                                Tok::Error(format!("unknown token '{char}'")),
                                self.line,
                                self.col - 1,
                                self.col,
                            ));
                        }
                    }
                }
            }
        }

        if let Some(curr_substring_start) = self.curr_substring_start {
            self.curr_substring_start = None;

            match parse_substring(&self.code[curr_substring_start..]) {
                Ok(token_type) => Some(Token::new(
                    token_type,
                    self.line,
                    self.col + curr_substring_start - self.code.len(),
                    self.col,
                )),

                Err(msg) => Some(Token::new(
                    Tok::Error(msg),
                    self.line,
                    self.col + curr_substring_start - self.code.len(),
                    self.col,
                )),
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Op, Tok, Token};

    #[test]
    fn empty() {
        let tokens: Vec<Token> = Lexer::new("").collect();
        assert!(tokens.is_empty());
    }

    #[test]
    fn one_line() {
        let test_str = "(abcdef + g * 12)^2/hij";
        let tokens: Vec<Token> = Lexer::new(test_str).collect();

        let expected_tokens = vec![
            Token::new(Tok::OpenParen, 0, 0, 1),
            Token::new(Tok::Identifier("abcdef".into()), 0, 1, 7),
            Token::new(Tok::Op(Op::Add), 0, 8, 9),
            Token::new(Tok::Identifier("g".into()), 0, 10, 11),
            Token::new(Tok::Op(Op::Mul), 0, 12, 13),
            Token::new(Tok::Float(12.0), 0, 14, 16),
            Token::new(Tok::CloseParen, 0, 16, 17),
            Token::new(Tok::Op(Op::Exp), 0, 17, 18),
            Token::new(Tok::Float(2.0), 0, 18, 19),
            Token::new(Tok::Op(Op::Div), 0, 19, 20),
            Token::new(Tok::Identifier("hij".into()), 0, 20, 23),
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn multi_line() {
        let test_str = "this is\n*a test*\n with lots of newlines\n";
        let tokens: Vec<Token> = Lexer::new(test_str).collect();

        let expected_tokens = vec![
            Token::new(Tok::Identifier("this".into()), 0, 0, 4),
            Token::new(Tok::Identifier("is".into()), 0, 5, 7),
            Token::new(Tok::Op(Op::Mul), 1, 0, 1),
            Token::new(Tok::Identifier("a".into()), 1, 1, 2),
            Token::new(Tok::Identifier("test".into()), 1, 3, 7),
            Token::new(Tok::Op(Op::Mul), 1, 7, 8),
            Token::new(Tok::Identifier("with".into()), 2, 1, 5),
            Token::new(Tok::Identifier("lots".into()), 2, 6, 10),
            Token::new(Tok::Identifier("of".into()), 2, 11, 13),
            Token::new(Tok::Identifier("newlines".into()), 2, 14, 22),
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn bad_literals() {
        let test_str = "1e9999 1xyz";
        let tokens: Vec<Token> = Lexer::new(test_str).collect();

        let expected_tokens = vec![
            Token::new(Tok::Error("number overflowed f64".into()), 0, 0, 6),
            Token::new(Tok::Error("invalid float literal".into()), 0, 7, 11),
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn dot_op_and_decimal() {
        let test_str = "a.b + 12.3 - 12..1 * 100.1.2";
        let tokens: Vec<Token> = Lexer::new(test_str).collect();

        let expected_tokens = vec![
            Token::new(Tok::Identifier("a".into()), 0, 0, 1),
            Token::new(Tok::Op(Op::Dot), 0, 1, 2),
            Token::new(Tok::Identifier("b".into()), 0, 2, 3),
            Token::new(Tok::Op(Op::Add), 0, 4, 5),
            Token::new(Tok::Float(12.3), 0, 6, 10),
            Token::new(Tok::Op(Op::Sub), 0, 11, 12),
            Token::new(Tok::Error("invalid float literal".into()), 0, 13, 18),
            Token::new(Tok::Op(Op::Mul), 0, 19, 20),
            Token::new(Tok::Error("invalid float literal".into()), 0, 21, 28),
        ];

        assert_eq!(tokens, expected_tokens);
    }
}
