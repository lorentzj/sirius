use std::collections::HashMap;

use lalrpop_util::ParseError;
use serde::Serialize;

use crate::error::{Error, ErrorType};
use crate::lexer::{tokenize, Op, Tok, Token};

lalrpop_mod!(#[allow(clippy::all)] pub grammar);

#[derive(Serialize, Clone, Debug)]
pub enum UnaryOp {
    ArithNeg,
    BoolNeg,
    Tick,
}

#[derive(Serialize, Clone, Debug)]
pub enum Expression {
    F64 {
        start: usize,
        val: f64,
        end: usize,
    },
    I64 {
        start: usize,
        val: i64,
        end: usize,
    },
    Bool {
        start: usize,
        val: bool,
        end: usize,
    },
    Identifier {
        start: usize,
        name: String,
        end: usize,
    },
    BinaryOp {
        start: usize,
        lhs: Box<Expression>,
        op: Op,
        rhs: Box<Expression>,
        end: usize,
    },
    UnaryOp {
        start: usize,
        op: UnaryOp,
        inner: Box<Expression>,
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
    Accessor {
        start: usize,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        end: usize,
    },
    FnCall {
        start: usize,
        caller: Box<Expression>,
        args: Vec<Expression>,
        end: usize,
    },
}

impl Expression {
    pub fn range(&self) -> (usize, usize) {
        match self {
            Expression::F64 { start, end, .. } => (*start, *end),
            Expression::I64 { start, end, .. } => (*start, *end),
            Expression::Bool { start, end, .. } => (*start, *end),
            Expression::Identifier { start, end, .. } => (*start, *end),
            Expression::BinaryOp { start, end, .. } => (*start, *end),
            Expression::UnaryOp { start, end, .. } => (*start, *end),
            Expression::OpenTuple { start, end, .. } => (*start, *end),
            Expression::Tuple { start, end, .. } => (*start, *end),
            Expression::Accessor { start, end, .. } => (*start, *end),
            Expression::FnCall { start, end, .. } => (*start, *end),
        }
    }

    #[cfg(test)]
    fn short_fmt(&self) -> String {
        match self {
            Expression::F64 { val, .. } => format!("{}", val),
            Expression::I64 { val, .. } => format!("{}", val),
            Expression::Bool { val, .. } => {
                if *val {
                    "true".into()
                } else {
                    "false".into()
                }
            }
            Expression::Identifier { name, .. } => format!("{}", name),
            Expression::BinaryOp { lhs, op, rhs, .. } => {
                format!("({}{:?}{})", lhs.short_fmt(), op, rhs.short_fmt())
            }
            Expression::UnaryOp {
                inner,
                op: UnaryOp::ArithNeg,
                ..
            } => format!("(-{})", inner.short_fmt()),
            Expression::UnaryOp {
                inner,
                op: UnaryOp::BoolNeg,
                ..
            } => format!("(!{})", inner.short_fmt()),
            Expression::UnaryOp {
                inner,
                op: UnaryOp::Tick,
                ..
            } => format!("({}')", inner.short_fmt()),
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
            Expression::FnCall { caller, args, .. } => {
                let mut result = format!("({})", caller.short_fmt());
                result.push_str("(");
                for (i, expr) in args.iter().enumerate() {
                    result.push_str(&expr.short_fmt());
                    if i < args.len() - 1 {
                        result.push_str(",");
                    }
                }
                result.push_str(")");
                result
            }

            Expression::Accessor { lhs, rhs, .. } => {
                format!("({}[{}])", lhs.short_fmt(), rhs.short_fmt())
            }

            Expression::OpenTuple { .. } => panic!(),
        }
    }
}

#[derive(Serialize, Clone, Debug)]
pub enum Statement {
    Let {
        start: usize,
        name: String,
        ann: Option<Box<Expression>>,
        val: Box<Expression>,
        end: usize,
    },
    Print {
        start: usize,
        val: Box<Expression>,
        end: usize,
    },
    Return {
        start: usize,
        val: Option<Box<Expression>>,
        end: usize,
    },
    If {
        start: usize,
        cond: Box<Expression>,
        true_inner: Vec<Statement>,
        false_inner: Option<Vec<Statement>>,
        end: usize,
    },
    For {
        start: usize,
        iterator: String,
        from: Box<Expression>,
        to: Box<Expression>,
        inner: Vec<Statement>,
        end: usize,
    },
}

impl Statement {
    fn _range(&self) -> (usize, usize) {
        match self {
            Statement::If { start, end, .. } => (*start, *end),
            Statement::For { start, end, .. } => (*start, *end),
            Statement::Let { start, end, .. } => (*start, *end),
            Statement::Print { start, end, .. } => (*start, *end),
            Statement::Return { start, end, .. } => (*start, *end),
        }
    }

    #[cfg(test)]
    fn short_fmt(&self) -> String {
        match self {
            Statement::Let { name, ann, val, .. } => match ann {
                Some(ann) => format!("let {name}: {} = {};", ann.short_fmt(), val.short_fmt()),
                None => format!("let {name} = {};", val.short_fmt()),
            },
            Statement::Print { val, .. } => {
                format!("print {};", val.short_fmt())
            }
            Statement::If {
                cond,
                true_inner,
                false_inner,
                ..
            } => {
                let mut res = String::new();
                res.push_str(&format!("if {} {{\n", cond.short_fmt()));

                for stmt in true_inner {
                    res.push_str(&stmt.short_fmt());
                    res.push('\n');
                }

                res.push('}');
                if let Some(false_inner) = false_inner {
                    res.push_str(" else {{\n".into());
                    for stmt in false_inner {
                        res.push_str(&stmt.short_fmt());
                        res.push('\n');
                    }
                    res.push('}');
                }
                res
            }
            Statement::For {
                iterator,
                from,
                to,
                inner,
                ..
            } => {
                let mut res = String::new();
                res.push_str(&format!(
                    "for {} from {} to {} {{\n",
                    iterator,
                    from.short_fmt(),
                    to.short_fmt()
                ));

                for stmt in inner {
                    res.push_str(&stmt.short_fmt());
                    res.push('\n');
                }

                res.push('}');
                res
            }
            Statement::Return { val, .. } => {
                format!(
                    "return {};",
                    val.as_ref().map(|v| v.short_fmt()).unwrap_or("".into())
                )
            }
        }
    }
}

#[derive(Serialize, Debug)]
pub struct Function {
    pub name: String,
    pub type_args: Vec<String>,
    pub args: Vec<(String, Expression)>,
    pub return_type: Option<Expression>,
    pub inner: Vec<Statement>,
}

pub type AST = HashMap<String, Function>;

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ParserOutput {
    pub tokens: Vec<Token>,
    pub errors: Vec<Error>,
    pub type_tokens: Vec<usize>,
    pub highlight_map: HashMap<usize, Vec<usize>>,
    pub annotations: HashMap<usize, String>,
    pub ast: AST,
}

pub fn parse(code: &str) -> ParserOutput {
    let tokens: Vec<_> = tokenize(code);
    let mut errors: Vec<Error> = vec![];
    let mut type_tokens: Vec<usize> = vec![];
    let mut highlight_map: HashMap<usize, Vec<usize>> = HashMap::default();

    match grammar::ASTParser::new().parse(
        &mut highlight_map,
        tokens
            .iter()
            .enumerate()
            .map(|(i, token)| Ok((i, token.data.clone(), i + 1))),
    ) {
        Ok(ast) => {
            for (_, function) in ast.iter() {
                for (_, ann) in &function.args {
                    let (ann_start, ann_end) = ann.range();
                    let tokens: Vec<usize> = (ann_start..ann_end).collect();
                    type_tokens.extend(&tokens);
                }

                if let Some(ann) = &function.return_type {
                    let (ann_start, ann_end) = ann.range();
                    let tokens: Vec<usize> = (ann_start..ann_end).collect();
                    type_tokens.extend(&tokens);
                }

                for statement in &function.inner {
                    if let Statement::Let { ann: Some(ann), .. } = statement {
                        let (ann_start, ann_end) = ann.range();
                        let tokens: Vec<usize> = (ann_start..ann_end).collect();
                        type_tokens.extend(&tokens);
                    }
                }
            }

            return ParserOutput {
                tokens,
                errors,
                type_tokens,
                highlight_map,
                ast,
                annotations: HashMap::default(),
            };
        }

        Err(ParseError::ExtraToken { token }) => errors.push(Error::new(
            ErrorType::ParseError,
            match token.1 {
                Tok::Identifier(n) => format!("unexpected identifier '{n}'"),
                Tok::Op(op) => format!("unexpected operator '{op:?}'"),
                Tok::Keyword(k) => format!("unexpected keyword '{k:?}'"),
                Tok::Float(_) => "unexpected constant".into(),
                Tok::Int(_) => "unexpected constant".into(),
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
                Tok::Identifier(n) => format!("unexpected identifier '{n}'"),
                Tok::Op(op) => format!("unexpected operator '{op:?}'"),
                Tok::Keyword(k) => format!("unexpected keyword '{k:?}'"),
                Tok::Float(_) => "unexpected constant".into(),
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
        highlight_map: HashMap::default(),
        ast: HashMap::default(),
        annotations: HashMap::default(),
    }
}

#[cfg(test)]
mod tests {
    use super::parse;

    #[test]
    fn lead_op() {
        let test_str = "fn main() { let x = + }";
        let message = parse(test_str).errors[0].message.clone();
        assert_eq!(message, "unexpected operator '+'");
    }

    #[test]
    fn op_precedence() {
        let test_str = "fn main() { print (a^2/3 + 4/0.1*b*c^2 - 3 && 4/2)^(d, 0.5^e - 3); }";
        let tree = parse(test_str);
        assert_eq!(
            tree.ast["main"].inner[0].short_fmt(),
            "print ((((((a^2)/3)+(((4/0.1)*b)*(c^2)))-3)&&(4/2))^(d,((0.5^e)-3)));"
        );
    }

    #[test]
    fn tuples() {
        let test_str = "fn main() { let x = (a + 2, (b, c, (d, e), e), f^2, (g, h)), (i, j), k; }";
        let tree = parse(test_str);
        assert_eq!(
            tree.ast["main"].inner[0].short_fmt(),
            "let x = (((a+2),(b,c,(d,e),e),(f^2),(g,h)),(i,j),k);"
        )
    }

    #[test]
    fn annotation() {
        let test_str = "fn main() { let y: (f64, 64) = x, 2; }";
        let tree = parse(test_str);
        assert_eq!(
            tree.ast["main"].inner[0].short_fmt(),
            "let y: (f64,64) = (x,2);"
        )
    }
}
