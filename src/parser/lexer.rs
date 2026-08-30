use std::fmt;

#[derive(Clone, PartialEq, Eq)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Op {
    Dot,
    Arith(ArithOp),
    And,
    Or,
    Not,
    Greater,
    GreaterOrEq,
    Less,
    LessOrEq,
    Eq,
    NotEq,
    Tick,
    Apply,
    Option,
    Comma,
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Dot => write!(f, "."),
            Op::Arith(ArithOp::Exp) => write!(f, "^"),
            Op::Arith(ArithOp::Mul) => write!(f, "*"),
            Op::Arith(ArithOp::Div) => write!(f, "/"),
            Op::Arith(ArithOp::Add) => write!(f, "+"),
            Op::Arith(ArithOp::Sub) => write!(f, "-"),
            Op::And => write!(f, "and"),
            Op::Or => write!(f, "or"),
            Op::Not => write!(f, "!"),
            Op::GreaterOrEq => write!(f, ">="),
            Op::Greater => write!(f, ">"),
            Op::Less => write!(f, "<"),
            Op::LessOrEq => write!(f, "<="),
            Op::Eq => write!(f, "=="),
            Op::NotEq => write!(f, "!="),
            Op::Tick => write!(f, "'"),
            Op::Apply => write!(f, "->"),
            Op::Option => write!(f, "?"),
            Op::Comma => write!(f, ","),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum AssnOp {
    Eq,
    Mul,
    Div,
    Add,
    Sub,
}

impl fmt::Debug for AssnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssnOp::Eq => write!(f, "="),
            AssnOp::Mul => write!(f, "*="),
            AssnOp::Div => write!(f, "/="),
            AssnOp::Add => write!(f, "+="),
            AssnOp::Sub => write!(f, "-="),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Keyword {
    Print,
    Let,
    If,
    Else,
    True,
    False,
    And,
    Or,
    Fn,
    Return,
    For,
    From,
    To,
    SuchThat,
    In,
    Yield,
    Mut,
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
            Keyword::And => write!(f, "and"),
            Keyword::Or => write!(f, "or"),
            Keyword::Fn => write!(f, "fn"),
            Keyword::Return => write!(f, "return"),
            Keyword::For => write!(f, "for"),
            Keyword::From => write!(f, "from"),
            Keyword::To => write!(f, "to"),
            Keyword::SuchThat => write!(f, "st"),
            Keyword::In => write!(f, "\\in"),
            Keyword::Yield => write!(f, "yield"),
            Keyword::Mut => write!(f, "mut"),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Tok {
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenSqBracket,
    CloseSqBracket,
    Indent,
    Dedent,
    NewLine,
    Op(Op),
    AssignOp(AssnOp),
    Float(f64),
    Int(i64),
    Identifier(String),
    Keyword(Keyword),
    Semicolon,
    Colon,
    Comment,
    Error(String),
    IndentError(String),
}

impl fmt::Debug for Tok {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Tok::Indent => write!(f, "[indent]"),
            Tok::Dedent => write!(f, "[dedent]"),
            Tok::NewLine => write!(f, "[newline]"),
            Tok::OpenParen => write!(f, "("),
            Tok::CloseParen => write!(f, ")"),
            Tok::OpenCurly => write!(f, "{{"),
            Tok::CloseCurly => write!(f, "}}"),
            Tok::OpenSqBracket => write!(f, "["),
            Tok::CloseSqBracket => write!(f, "]"),
            Tok::Op(op) => write!(f, "{op:?}"),
            Tok::AssignOp(op) => write!(f, "{op:?}"),
            Tok::Float(v) => write!(f, "{v}"),
            Tok::Int(v) => write!(f, "{v}"),
            Tok::Identifier(n) => write!(f, "{n}"),
            Tok::Keyword(k) => write!(f, "{k:?}"),
            Tok::Semicolon => write!(f, ";"),
            Tok::Colon => write!(f, ":"),
            Tok::Comment => write!(f, "comment"),
            Tok::Error(m) => write!(f, "error({m})"),
            Tok::IndentError(m) => write!(f, "error({m})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub data: Tok,
    pub line: usize,
    pub start: usize,
    pub is_type_ann: bool,
    pub end: usize,
}

impl Token {
    fn new(data: Tok, line: usize, start: usize, end: usize) -> Token {
        Token {
            data,
            line,
            start,
            is_type_ann: false,
            end,
        }
    }

    pub fn is_comment(&self) -> bool {
        self.data == Tok::Comment
    }

    pub fn get_error(&self) -> Option<String> {
        match &self.data {
            Tok::Error(e) => Some(e.clone()),
            Tok::IndentError(e) => Some(e.clone()),
            _ => None,
        }
    }
}

fn parse_keyword(s: &str) -> Option<Tok> {
    match s {
        "let" => Some(Tok::Keyword(Keyword::Let)),
        "print" => Some(Tok::Keyword(Keyword::Print)),
        "if" => Some(Tok::Keyword(Keyword::If)),
        "else" => Some(Tok::Keyword(Keyword::Else)),
        "true" => Some(Tok::Keyword(Keyword::True)),
        "false" => Some(Tok::Keyword(Keyword::False)),
        "and" => Some(Tok::Keyword(Keyword::And)),
        "or" => Some(Tok::Keyword(Keyword::Or)),
        "fn" => Some(Tok::Keyword(Keyword::Fn)),
        "return" => Some(Tok::Keyword(Keyword::Return)),
        "for" => Some(Tok::Keyword(Keyword::For)),
        "from" => Some(Tok::Keyword(Keyword::From)),
        "to" => Some(Tok::Keyword(Keyword::To)),
        "st" => Some(Tok::Keyword(Keyword::SuchThat)),
        "\\in" => Some(Tok::Keyword(Keyword::In)),
        "yield" => Some(Tok::Keyword(Keyword::Yield)),
        "mut" => Some(Tok::Keyword(Keyword::Mut)),
        _ => None,
    }
}

fn lex_substring_alpha_num_dec(s: &str, line: usize, start: usize, end: usize) -> Vec<Token> {
    match parse_keyword(s) {
        Some(tok) => vec![Token::new(tok, line, start, end)],
        None => {
            if s == "." {
                vec![Token::new(Tok::Op(Op::Dot), line, start, end)]
            } else {
                match s.chars().next() {
                    Some('0'..='9') => {
                        if s.contains('.') {
                            let parsed_number = String::from_iter(s.chars()).parse::<f64>();
                            match parsed_number {
                                Ok(n) => {
                                    if n.is_infinite() {
                                        vec![Token::new(
                                            Tok::Error("number overflowed f64".into()),
                                            line,
                                            start,
                                            end,
                                        )]
                                    } else {
                                        vec![Token::new(Tok::Float(n), line, start, end)]
                                    }
                                }
                                Err(msg) => {
                                    vec![Token::new(Tok::Error(msg.to_string()), line, start, end)]
                                }
                            }
                        } else {
                            let parsed_number = String::from_iter(s.chars()).parse::<i64>();
                            match parsed_number {
                                Ok(n) => vec![Token::new(Tok::Int(n), line, start, end)],
                                Err(msg) => {
                                    vec![Token::new(Tok::Error(msg.to_string()), line, start, end)]
                                }
                            }
                        }
                    }
                    None => vec![],
                    _ => {
                        if s.contains('.') {
                            let mut new_start = start;
                            let mut res = vec![];
                            for sub in s.split('.') {
                                res.extend(lex_substring_alpha_num_dec(
                                    sub,
                                    line,
                                    new_start,
                                    new_start + sub.len(),
                                ));
                                new_start += sub.len() + 1;
                                res.push(Token::new(
                                    Tok::Op(Op::Dot),
                                    line,
                                    new_start - 1,
                                    new_start,
                                ));
                            }
                            res.pop();
                            res
                        } else {
                            vec![Token::new(Tok::Identifier(s.to_string()), line, start, end)]
                        }
                    }
                }
            }
        }
    }
}

pub fn tokenize(code: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = vec![];
    let mut bracket_level = (0, 0, 0);
    let mut indent_spaces: Option<usize> = Some(0);
    let mut commenting = false;
    let mut prev_indent_level = 0;
    let mut line = 0;
    let mut col = 0;

    let mut curr_substring_start: Option<usize> = None;

    for (i, char) in code.char_indices() {
        if commenting {
            if char == '\n' {
                commenting = false;
            } else {
                if let Some(Token {
                    data: Tok::Comment,
                    end,
                    ..
                }) = tokens.last_mut()
                {
                    *end += 1;
                }
                col += 1;
                continue;
            }
        }

        match char {
            '0'..='9' | 'A'..='Z' | 'a'..='z' | '\u{0370}'..='\u{03FF}' | '_' | '.' | '\\' => {
                if let Some(spaces) = indent_spaces {
                    if spaces % 4 == 0 {
                        let curr_indent_level = spaces / 4;
                        #[allow(clippy::comparison_chain)]
                        if curr_indent_level > prev_indent_level {
                            for c in prev_indent_level..curr_indent_level {
                                tokens.push(Token::new(Tok::Indent, line, c * 4, (c + 1) * 4));
                            }
                        } else if prev_indent_level > curr_indent_level {
                            for _c in curr_indent_level..prev_indent_level {
                                tokens.push(Token::new(
                                    Tok::Dedent,
                                    line,
                                    0,
                                    curr_indent_level * 4,
                                ));
                            }
                        }
                        prev_indent_level = curr_indent_level;
                    } else {
                        tokens.push(Token::new(
                            Tok::IndentError(format!(
                                "indent level must be multiple of 4; found {spaces}"
                            )),
                            line,
                            col - spaces,
                            col,
                        ));
                    }
                }
                indent_spaces = None;
                if curr_substring_start.is_none() {
                    curr_substring_start = Some(i);
                }

                col += 1;
            }
            _ => {
                if let Some(curr_ss) = curr_substring_start {
                    tokens.extend(lex_substring_alpha_num_dec(
                        &code[curr_ss..i],
                        line,
                        col - (i - curr_ss),
                        col,
                    ));

                    curr_substring_start = None;
                }

                if char == '\n' {
                    line += 1;
                    col = 0;
                } else {
                    if char != ' ' && char != '\u{00A0}' {
                        if let Some(spaces) = indent_spaces {
                            if spaces % 4 == 0 {
                                let curr_indent_level = spaces / 4;
                                #[allow(clippy::comparison_chain)]
                                if curr_indent_level > prev_indent_level {
                                    for c in prev_indent_level..curr_indent_level {
                                        tokens.push(Token::new(
                                            Tok::Indent,
                                            line,
                                            c * 4,
                                            (c + 1) * 4,
                                        ));
                                    }
                                } else if prev_indent_level > curr_indent_level {
                                    for _c in curr_indent_level..prev_indent_level {
                                        tokens.push(Token::new(
                                            Tok::Dedent,
                                            line,
                                            0,
                                            curr_indent_level * 4,
                                        ));
                                    }
                                }
                                prev_indent_level = curr_indent_level;
                            } else {
                                tokens.push(Token::new(
                                    Tok::IndentError(format!(
                                        "indent level must be multiple of 4; found {spaces}"
                                    )),
                                    line,
                                    col - spaces,
                                    col,
                                ));
                            }
                        }
                        indent_spaces = None;
                    }
                    col += 1;
                }

                let mut should_pop = false;
                let mut clear_line_whitespace = false;

                let new_token = match char {
                    ' ' | '\u{00A0}' => match indent_spaces {
                        Some(i) => {
                            indent_spaces = Some(i + 1);
                            continue;
                        }
                        None => continue,
                    },
                    '\n' => {
                        if bracket_level == (0, 0, 0) {
                            indent_spaces = Some(0);
                            Token::new(Tok::NewLine, line, col, col)
                        } else {
                            continue;
                        }
                    }
                    ';' => Token::new(Tok::Semicolon, line, col - 1, col),
                    ':' => Token::new(Tok::Colon, line, col - 1, col),
                    '(' => {
                        bracket_level.0 += 1;
                        Token::new(Tok::OpenParen, line, col - 1, col)
                    }
                    ')' => {
                        bracket_level.0 -= 1;
                        Token::new(Tok::CloseParen, line, col - 1, col)
                    }
                    '{' => {
                        bracket_level.1 += 1;
                        Token::new(Tok::OpenCurly, line, col - 1, col)
                    }
                    '}' => {
                        bracket_level.1 -= 1;
                        Token::new(Tok::CloseCurly, line, col - 1, col)
                    }
                    '[' => {
                        bracket_level.2 += 1;
                        Token::new(Tok::OpenSqBracket, line, col - 1, col)
                    }
                    ']' => {
                        bracket_level.2 -= 1;
                        Token::new(Tok::CloseSqBracket, line, col - 1, col)
                    }
                    '.' => Token::new(Tok::Op(Op::Dot), line, col - 1, col),
                    '^' => Token::new(Tok::Op(Op::Arith(ArithOp::Exp)), line, col - 1, col),
                    '*' => Token::new(Tok::Op(Op::Arith(ArithOp::Mul)), line, col - 1, col),
                    '/' => Token::new(Tok::Op(Op::Arith(ArithOp::Div)), line, col - 1, col),
                    '#' => {
                        clear_line_whitespace = true;
                        commenting = true;
                        Token::new(Tok::Comment, line, col - 1, col)
                    }
                    '+' => Token::new(Tok::Op(Op::Arith(ArithOp::Add)), line, col - 1, col),
                    '-' => Token::new(Tok::Op(Op::Arith(ArithOp::Sub)), line, col - 1, col),
                    '!' => Token::new(Tok::Op(Op::Not), line, col - 1, col),
                    '\'' => Token::new(Tok::Op(Op::Tick), line, col - 1, col),
                    '>' => {
                        if let Some(Token {
                            data: Tok::Op(Op::Arith(ArithOp::Sub)),
                            start,
                            ..
                        }) = tokens.last()
                        {
                            should_pop = true;
                            Token::new(Tok::Op(Op::Apply), line, *start, col)
                        } else {
                            Token::new(Tok::Op(Op::Greater), line, col - 1, col)
                        }
                    }
                    '<' => Token::new(Tok::Op(Op::Less), line, col - 1, col),
                    '?' => Token::new(Tok::Op(Op::Option), line, col - 1, col),
                    '=' => {
                        if let Some(Token {
                            data: Tok::AssignOp(AssnOp::Eq),
                            start,
                            ..
                        }) = tokens.last()
                        {
                            should_pop = true;
                            Token::new(Tok::Op(Op::Eq), line, *start, col)
                        } else if let Some(Token {
                            data: Tok::Op(last_op),
                            start,
                            ..
                        }) = tokens.last()
                        {
                            match last_op {
                                Op::Greater => {
                                    should_pop = true;
                                    Token::new(Tok::Op(Op::GreaterOrEq), line, *start, col)
                                }
                                Op::Less => {
                                    should_pop = true;
                                    Token::new(Tok::Op(Op::LessOrEq), line, *start, col)
                                }
                                Op::Not => {
                                    should_pop = true;
                                    Token::new(Tok::Op(Op::NotEq), line, *start, col)
                                }
                                Op::Arith(ArithOp::Add) => {
                                    should_pop = true;
                                    Token::new(Tok::AssignOp(AssnOp::Add), line, *start, col)
                                }
                                Op::Arith(ArithOp::Sub) => {
                                    should_pop = true;
                                    Token::new(Tok::AssignOp(AssnOp::Sub), line, *start, col)
                                }
                                Op::Arith(ArithOp::Mul) => {
                                    should_pop = true;
                                    Token::new(Tok::AssignOp(AssnOp::Mul), line, *start, col)
                                }
                                Op::Arith(ArithOp::Div) => {
                                    should_pop = true;
                                    Token::new(Tok::AssignOp(AssnOp::Div), line, *start, col)
                                }
                                _ => Token::new(Tok::AssignOp(AssnOp::Eq), line, col - 1, col),
                            }
                        } else {
                            Token::new(Tok::AssignOp(AssnOp::Eq), line, col - 1, col)
                        }
                    }
                    ',' => Token::new(Tok::Op(Op::Comma), line, col - 1, col),
                    _ => Token::new(
                        Tok::Error(format!("unknown token \"{char}\"")),
                        line,
                        col - 1,
                        col,
                    ),
                };

                if should_pop {
                    tokens.pop();
                }

                if clear_line_whitespace {
                    while let Some(last) = tokens.last() {
                        if last.line == line {
                            match last.data {
                                Tok::Indent => prev_indent_level -= 1,
                                Tok::Dedent => prev_indent_level += 1,
                                Tok::IndentError(_) => (),
                                _ => break,
                            }
                            tokens.pop();
                        } else {
                            break;
                        }
                    }
                }

                tokens.push(new_token);
            }
        }
    }

    if let Some(curr_ss) = curr_substring_start {
        tokens.extend(lex_substring_alpha_num_dec(
            &code[curr_ss..code.len()],
            line,
            col - (code.len() - curr_ss),
            col,
        ));
    }

    if prev_indent_level > 0 {
        tokens.push(Token::new(Tok::NewLine, line, col, col));

        for _i in 0..prev_indent_level {
            tokens.push(Token::new(Tok::Dedent, line, col, col))
        }
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::{ArithOp, Op, Tok, Token, tokenize};

    #[test]
    fn empty() {
        let tokens = tokenize("");
        assert!(tokens.is_empty());
    }

    #[test]
    fn one_line() {
        let test_str = "(abcdef + g * 12)^2.0/hij";
        let tokens = tokenize(test_str);

        let expected_tokens = vec![
            Token::new(Tok::OpenParen, 0, 0, 1),
            Token::new(Tok::Identifier("abcdef".into()), 0, 1, 7),
            Token::new(Tok::Op(Op::Arith(ArithOp::Add)), 0, 8, 9),
            Token::new(Tok::Identifier("g".into()), 0, 10, 11),
            Token::new(Tok::Op(Op::Arith(ArithOp::Mul)), 0, 12, 13),
            Token::new(Tok::Int(12), 0, 14, 16),
            Token::new(Tok::CloseParen, 0, 16, 17),
            Token::new(Tok::Op(Op::Arith(ArithOp::Exp)), 0, 17, 18),
            Token::new(Tok::Float(2.0), 0, 18, 21),
            Token::new(Tok::Op(Op::Arith(ArithOp::Div)), 0, 21, 22),
            Token::new(Tok::Identifier("hij".into()), 0, 22, 25),
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn multi_line() {
        let test_str = "this is\n*a test*\nwith lots of newlines\n";
        let tokens = tokenize(test_str);

        let expected_tokens = vec![
            Token::new(Tok::Identifier("this".into()), 0, 0, 4),
            Token::new(Tok::Identifier("is".into()), 0, 5, 7),
            Token::new(Tok::NewLine, 1, 0, 0),
            Token::new(Tok::Op(Op::Arith(ArithOp::Mul)), 1, 0, 1),
            Token::new(Tok::Identifier("a".into()), 1, 1, 2),
            Token::new(Tok::Identifier("test".into()), 1, 3, 7),
            Token::new(Tok::Op(Op::Arith(ArithOp::Mul)), 1, 7, 8),
            Token::new(Tok::NewLine, 2, 0, 0),
            Token::new(Tok::Identifier("with".into()), 2, 0, 4),
            Token::new(Tok::Identifier("lots".into()), 2, 5, 9),
            Token::new(Tok::Identifier("of".into()), 2, 10, 12),
            Token::new(Tok::Identifier("newlines".into()), 2, 13, 21),
            Token::new(Tok::NewLine, 3, 0, 0),
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn bad_literals() {
        let test_str = "1.e9999 1.xyz";
        let tokens = tokenize(test_str);

        let expected_tokens = vec![
            Token::new(Tok::Error("number overflowed f64".into()), 0, 0, 7),
            Token::new(Tok::Error("invalid float literal".into()), 0, 8, 13),
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn dot_op_and_decimal() {
        let test_str = "a.b + 12.3 - 12..1 * 100.1.2";
        let tokens = tokenize(test_str);

        let expected_tokens = vec![
            Token::new(Tok::Identifier("a".into()), 0, 0, 1),
            Token::new(Tok::Op(Op::Dot), 0, 1, 2),
            Token::new(Tok::Identifier("b".into()), 0, 2, 3),
            Token::new(Tok::Op(Op::Arith(ArithOp::Add)), 0, 4, 5),
            Token::new(Tok::Float(12.3), 0, 6, 10),
            Token::new(Tok::Op(Op::Arith(ArithOp::Sub)), 0, 11, 12),
            Token::new(Tok::Error("invalid float literal".into()), 0, 13, 18),
            Token::new(Tok::Op(Op::Arith(ArithOp::Mul)), 0, 19, 20),
            Token::new(Tok::Error("invalid float literal".into()), 0, 21, 28),
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn indent_dedent() {
        let test_str = "
a b c
    a
        b c d
        e
    g
        h i j
f
d e f
";
        let tokens = tokenize(test_str);

        let expected_tokens = vec![
            Token::new(Tok::NewLine, 1, 0, 0),
            Token::new(Tok::Identifier("a".into()), 1, 0, 1),
            Token::new(Tok::Identifier("b".into()), 1, 2, 3),
            Token::new(Tok::Identifier("c".into()), 1, 4, 5),
            Token::new(Tok::NewLine, 2, 0, 0),
            Token::new(Tok::Indent, 2, 0, 4),
            Token::new(Tok::Identifier("a".into()), 2, 4, 5),
            Token::new(Tok::NewLine, 3, 0, 0),
            Token::new(Tok::Indent, 3, 4, 8),
            Token::new(Tok::Identifier("b".into()), 3, 8, 9),
            Token::new(Tok::Identifier("c".into()), 3, 10, 11),
            Token::new(Tok::Identifier("d".into()), 3, 12, 13),
            Token::new(Tok::NewLine, 4, 0, 0),
            Token::new(Tok::Identifier("e".into()), 4, 8, 9),
            Token::new(Tok::NewLine, 5, 0, 0),
            Token::new(Tok::Dedent, 5, 0, 4),
            Token::new(Tok::Identifier("g".into()), 5, 4, 5),
            Token::new(Tok::NewLine, 6, 0, 0),
            Token::new(Tok::Indent, 6, 4, 8),
            Token::new(Tok::Identifier("h".into()), 6, 8, 9),
            Token::new(Tok::Identifier("i".into()), 6, 10, 11),
            Token::new(Tok::Identifier("j".into()), 6, 12, 13),
            Token::new(Tok::NewLine, 7, 0, 0),
            Token::new(Tok::Dedent, 7, 0, 0),
            Token::new(Tok::Dedent, 7, 0, 0),
            Token::new(Tok::Identifier("f".into()), 7, 0, 1),
            Token::new(Tok::NewLine, 8, 0, 0),
            Token::new(Tok::Identifier("d".into()), 8, 0, 1),
            Token::new(Tok::Identifier("e".into()), 8, 2, 3),
            Token::new(Tok::Identifier("f".into()), 8, 4, 5),
            Token::new(Tok::NewLine, 9, 0, 0),
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn indents_brackets() {
        let test_str = "
    a b c(d)
        e f {
            g h i
        }
    j
    ";
        let tokens = tokenize(test_str);

        let expected_tokens = vec![
            Token::new(Tok::NewLine, 1, 0, 0),
            Token::new(Tok::Indent, 1, 0, 4),
            Token::new(Tok::Identifier("a".into()), 1, 4, 5),
            Token::new(Tok::Identifier("b".into()), 1, 6, 7),
            Token::new(Tok::Identifier("c".into()), 1, 8, 9),
            Token::new(Tok::OpenParen, 1, 9, 10),
            Token::new(Tok::Identifier("d".into()), 1, 10, 11),
            Token::new(Tok::CloseParen, 1, 11, 12),
            Token::new(Tok::NewLine, 2, 0, 0),
            Token::new(Tok::Indent, 2, 4, 8),
            Token::new(Tok::Identifier("e".into()), 2, 8, 9),
            Token::new(Tok::Identifier("f".into()), 2, 10, 11),
            Token::new(Tok::OpenCurly, 2, 12, 13),
            Token::new(Tok::Identifier("g".into()), 3, 12, 13),
            Token::new(Tok::Identifier("h".into()), 3, 14, 15),
            Token::new(Tok::Identifier("i".into()), 3, 16, 17),
            Token::new(Tok::CloseCurly, 4, 8, 9),
            Token::new(Tok::NewLine, 5, 0, 0),
            Token::new(Tok::Dedent, 5, 0, 4),
            Token::new(Tok::Identifier("j".into()), 5, 4, 5),
            Token::new(Tok::NewLine, 6, 0, 0),
            Token::new(Tok::NewLine, 6, 4, 4),
            Token::new(Tok::Dedent, 6, 4, 4),
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn repeated_dot_op() {
        let test_str = "aa.bb.1.2 . 3".to_string();
        let tokens = tokenize(&test_str);

        let expected_tokens = vec![
            Token::new(Tok::Identifier("aa".into()), 0, 0, 2),
            Token::new(Tok::Op(Op::Dot), 0, 2, 3),
            Token::new(Tok::Identifier("bb".into()), 0, 3, 5),
            Token::new(Tok::Op(Op::Dot), 0, 5, 6),
            Token::new(Tok::Int(1), 0, 6, 7),
            Token::new(Tok::Op(Op::Dot), 0, 7, 8),
            Token::new(Tok::Int(2), 0, 8, 9),
            Token::new(Tok::Op(Op::Dot), 0, 10, 11),
            Token::new(Tok::Int(3), 0, 12, 13),
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn indent() {
        let test_str = "    a".to_string();
        let tokens = tokenize(&test_str);

        let expected_tokens = vec![
            Token::new(Tok::Indent, 0, 0, 4),
            Token::new(Tok::Identifier("a".into()), 0, 4, 5),
            Token::new(Tok::NewLine, 0, 5, 5),
            Token::new(Tok::Dedent, 0, 5, 5),
        ];

        assert_eq!(tokens, expected_tokens);
    }
}
