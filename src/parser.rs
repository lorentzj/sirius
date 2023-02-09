use lalrpop_util::ParseError;
use serde::Serialize;

use crate::error::{Error, ErrorType};
use crate::lexer::{Lexer, Op, Tok, Token};

lalrpop_mod!(#[allow(clippy::all)] pub grammar);

#[derive(Serialize, Debug)]
pub enum Expression {
    Constant {
        start: usize,
        val: f64,
        end: usize,
    },
    Identifier {
        start: usize,
        name: String,
        end: usize,
    },
    BinOp {
        start: usize,
        lhs: Box<Expression>,
        op: Op,
        rhs: Box<Expression>,
        end: usize,
    },
    OpenTuple {
        start: usize,
        inner: Vec<Expression>,
        end: usize,
    },
    Tuple {
        start: usize,
        inner: Vec<Expression>,
        end: usize,
    },
}

impl Expression {
    pub fn range(&self) -> (usize, usize) {
        match self {
            Expression::Constant { start, end, .. } => (*start, *end),
            Expression::Identifier { start, end, .. } => (*start, *end),
            Expression::BinOp { start, end, .. } => (*start, *end),
            Expression::OpenTuple { start, end, .. } => (*start, *end),
            Expression::Tuple { start, end, .. } => (*start, *end),
        }
    }

    #[cfg(test)]
    fn short_fmt(&self) -> String {
        match self {
            Expression::Constant { val, .. } => format!("{}", val),
            Expression::Identifier { name, .. } => format!("{}", name),
            Expression::BinOp { lhs, op, rhs, .. } => {
                format!("({}{:?}{})", lhs.short_fmt(), op, rhs.short_fmt())
            }
            Expression::Tuple { inner, .. } => {
                let mut result = String::new();
                result.push_str("(");
                for (i, expr) in inner.iter().enumerate() {
                    result.push_str(&expr.short_fmt());
                    if i < inner.len() - 1 {
                        result.push_str(",");
                    }
                }
                result.push_str(")");
                result
            }
            Expression::OpenTuple { .. } => "{!! Open Tuple !!}".into(),
        }
    }
}

#[derive(Serialize, Debug)]
pub enum Statement {
    Let {
        name: String,
        ann: Option<Box<Expression>>,
        val: Box<Expression>,
    },
    Print(Box<Expression>),
}

impl Statement {
    #[cfg(test)]
    fn short_fmt(&self) -> String {
        match self {
            Statement::Let { name, ann, val } => match ann {
                Some(ann) => format!("let {}: {} = {};", name, ann.short_fmt(), val.short_fmt()),
                None => format!("let {} = {};", name, val.short_fmt()),
            },
            Statement::Print(v) => {
                format!("print {};", v.short_fmt())
            }
        }
    }
}

#[derive(Serialize, Debug)]
pub struct ParserOutput {
    pub tokens: Vec<Token>,
    pub errors: Vec<Error>,
    pub type_tokens: Vec<usize>,
    pub ast: Vec<Statement>,
}

pub fn parse(code: &str) -> ParserOutput {
    let tokens: Vec<_> = Lexer::new(code).collect();
    let mut errors: Vec<Error> = vec![];
    let mut type_tokens: Vec<usize> = vec![];

    match grammar::ASTParser::new().parse(
        tokens
            .iter()
            .enumerate()
            .map(|(i, token)| Ok((i, token.data.clone(), i + 1))),
    ) {
        Ok(ast) => {
            for statement in &ast {
                if let Statement::Let { ann: Some(ann), .. } = statement {
                    let (ann_start, ann_end) = ann.range();
                    let tokens: Vec<usize> = (ann_start..ann_end).into_iter().collect();
                    type_tokens.extend(&tokens);
                }
            }

            return ParserOutput {
                tokens,
                errors,
                type_tokens,
                ast,
            };
        }

        Err(ParseError::ExtraToken { token }) => errors.push(Error::new(
            ErrorType::ParseError,
            match token.1 {
                Tok::Identifier(n) => format!("unexpected identifier '{}'", n),
                Tok::Op(op) => format!("unexpected operator '{:?}'", op),
                Tok::Keyword(k) => format!("unexpected keyword '{:?}'", k),
                Tok::Constant(_) => "unexpected constant".into(),
                Tok::Error(m) => m,
                _ => format!("unexpected token '{:?}'", token.1),
            },
            token.0,
            token.2,
        )),
        Err(ParseError::InvalidToken { location }) => errors.push(Error::new(
            ErrorType::ParseError,
            "invalid token".into(),
            location,
            location + 1,
        )),
        Err(ParseError::UnrecognizedEOF { location, .. }) => errors.push(Error::new(
            ErrorType::ParseError,
            "unexpected EOF".into(),
            location - 1,
            location,
        )),
        Err(ParseError::UnrecognizedToken { token, .. }) => errors.push(Error::new(
            ErrorType::ParseError,
            match token.1 {
                Tok::Identifier(n) => format!("unexpected identifier '{}'", n),
                Tok::Op(op) => format!("unexpected operator '{:?}'", op),
                Tok::Keyword(k) => format!("unexpected keyword '{:?}'", k),
                Tok::Constant(_) => "unexpected constant".into(),
                Tok::Error(m) => m,
                _ => format!("unexpected token '{:?}'", token.1),
            },
            token.0,
            token.2,
        )),
        Err(ParseError::User { error }) => errors.push(error),
    }

    ParserOutput {
        tokens,
        errors,
        type_tokens: vec![],
        ast: vec![],
    }
}

#[cfg(test)]
mod tests {
    use super::parse;

    #[test]
    fn lead_op() {
        let test_str = "let x = +";
        let message = parse(test_str).errors[0].message.clone();
        assert_eq!(message, "unexpected operator '+'");
    }

    #[test]
    fn op_precedence() {
        let test_str = "print (a^2/3 + 4/0.1*b*c^2 - 3/2)^(d, 0.5^e - 3);";
        let tree = parse(test_str);
        assert_eq!(
            tree.ast[0].short_fmt(),
            "print (((((a^2)/3)+(((4/0.1)*b)*(c^2)))-(3/2))^(d,((0.5^e)-3)));"
        );
    }

    #[test]
    fn tuples() {
        let test_str = "let x = (a + 2, (b, c, (d, e), e), f^2, (g, h)), (i, j), k;";
        let tree = parse(test_str);
        assert_eq!(
            tree.ast[0].short_fmt(),
            "let x = (((a+2),(b,c,(d,e),e),(f^2),(g,h)),(i,j),k);"
        )
    }

    #[test]
    fn annotation() {
        let test_str = "let y: f64, 64 = x, 2;";
        let tree = parse(test_str);
        assert_eq!(tree.ast[0].short_fmt(), "let y: (f64,64) = (x,2);")
    }
}
