use std::collections::HashMap;

use lalrpop_util::ParseError;
use serde::Serialize;

use crate::typechecker::{annotation_type, Constraint, Ind, Type};

use crate::error::{Error, ErrorType};
use crate::lexer::{tokenize, Op, Tok, Token};

lalrpop_mod!(#[allow(clippy::all)] pub grammar);

#[derive(Serialize, Clone, Debug)]
pub struct Positioned<T> {
    pub inner: T,
    pub start: usize,
    pub end: usize,
}

impl<T> Positioned<T> {
    pub fn new(start: usize, inner: T, end: usize) -> Self {
        Positioned { start, inner, end }
    }

    pub fn map<A>(&self, f: fn(&T) -> A) -> Positioned<A> {
        Positioned {
            start: self.start,
            inner: f(&self.inner),
            end: self.end,
        }
    }
}

impl std::hash::Hash for Positioned<String> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

fn expr_to_pos_type(e: &Expression) -> Result<Positioned<Type>, Error> {
    match annotation_type(e) {
        Ok(t) => Ok(Positioned::new(e.start, t, e.end)),
        Err((start, end)) => Err(Error::new(
            ErrorType::Syntax,
            "illegal expression in annotation".to_string(),
            start,
            end,
        )),
    }
}

#[derive(Serialize, Clone, Debug)]
pub enum UnaryOp {
    ArithNeg,
    BoolNeg,
    Tick,
}

#[derive(Serialize, Clone, Debug)]
pub enum E {
    F64(f64),
    I64(i64, Option<Ind>),
    Bool(bool),
    Ident(String, Option<Vec<Positioned<Type>>>),
    BinaryOp(Box<Expression>, Op, Box<Expression>),
    UnaryOp(UnaryOp, Box<Expression>),
    OpenTuple(Vec<Expression>),
    Tuple(Vec<Expression>),
    Accessor(Box<Expression>, Box<Expression>),
    FnCall(Box<Expression>, Vec<Expression>),
}

#[derive(Serialize, Clone, Debug)]
pub struct Expression {
    pub start: usize,
    pub data: E,
    pub t: Type,
    pub end: usize,
}

impl Expression {
    pub fn fresh(start: usize, data: E, end: usize) -> Box<Self> {
        Box::new(Expression {
            start,
            data,
            t: Type::ForAll(0),
            end,
        })
    }
}

#[derive(Serialize, Clone, Debug)]
pub enum S {
    Let(Positioned<String>, Option<Positioned<Type>>, Expression),
    Assign(Positioned<String>, Expression),
    Print(Expression),
    Return(Option<Expression>),
    If(Expression, Vec<Statement>, Option<Vec<Statement>>),
    For(Positioned<String>, Expression, Expression, Vec<Statement>),
}

#[derive(Serialize, Clone, Debug)]
pub struct Statement {
    pub start: usize,
    pub data: S,
    pub end: usize,
}

impl Statement {
    pub fn new_let(
        name: Positioned<String>,
        annotation: Option<Positioned<Type>>,
        val: Expression,
    ) -> Self {
        let start = name.start - 1;
        let end = val.end;
        Statement {
            start,
            data: S::Let(name, annotation, val),
            end,
        }
    }

    pub fn new_assign(name: Positioned<String>, val: Expression) -> Self {
        let start = name.start;
        let end = val.end;
        Statement {
            start,
            data: S::Assign(name, val),
            end,
        }
    }

    pub fn new_print(val: Expression) -> Self {
        let start = val.start - 1;
        let end = val.end;
        Statement {
            start,
            data: S::Print(val),
            end,
        }
    }

    pub fn new_return(start: usize, val: Option<Expression>) -> Self {
        let end = val.as_ref().map(|val| val.end).unwrap_or(start + 1);

        Statement {
            start,
            data: S::Return(val),
            end,
        }
    }

    pub fn new_if(
        start: usize,
        cond: Expression,
        true_block: Vec<Statement>,
        false_block: Option<Vec<Statement>>,
        end: usize,
    ) -> Self {
        Statement {
            start,
            data: S::If(cond, true_block, false_block),
            end,
        }
    }

    pub fn new_for(
        start: usize,
        iterator: Positioned<String>,
        from: Expression,
        to: Expression,
        inner: Vec<Statement>,
        end: usize,
    ) -> Self {
        Statement {
            start,
            data: S::For(iterator, from, to, inner),
            end,
        }
    }
}

#[derive(Serialize, Debug)]
pub struct Function {
    pub name: String,
    pub type_args: Vec<Positioned<String>>,
    pub args: Vec<(Positioned<String>, Positioned<Type>)>,
    pub return_type: Positioned<Type>,
    pub body: Vec<Statement>,
    pub constraints: Vec<Constraint>,
}

impl Function {
    pub fn type_arg_names(&self) -> Vec<String> {
        self.type_args.iter().map(|n| n.inner.clone()).collect()
    }

    pub fn arg_names(&self) -> Vec<String> {
        self.args.iter().map(|n| n.0.inner.clone()).collect()
    }

    pub fn arg_types(&self) -> Vec<Type> {
        self.args.iter().map(|n| n.1.inner.clone()).collect()
    }
}

pub type AST = HashMap<String, Function>;

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct CompilerState {
    pub tokens: Vec<Token>,
    pub errors: Vec<Error>,
    pub type_tokens: Vec<usize>,
    pub highlight_map: HashMap<usize, Vec<usize>>,
    pub ast: AST,
}

pub fn parse(code: &str) -> CompilerState {
    let tokens: Vec<_> = tokenize(code);
    let mut errors: Vec<Error> = vec![];
    let mut type_tokens: Vec<usize> = vec![];
    let mut highlight_map: HashMap<usize, Vec<usize>> = HashMap::default();

    match grammar::ASTParser::new().parse(
        &mut highlight_map,
        &mut type_tokens,
        &mut errors,
        tokens
            .iter()
            .enumerate()
            .filter(|(_, token)| {
                !matches!(
                    token,
                    Token {
                        data: Tok::Comment,
                        ..
                    }
                )
            })
            .map(|(i, token)| Ok((i, token.data.clone(), i + 1))),
    ) {
        Ok(ast) => CompilerState {
            tokens,
            errors,
            type_tokens,
            highlight_map,
            ast,
        },
        Err(err) => {
            let error = match err {
                ParseError::InvalidToken { location } => Error::new(
                    ErrorType::Syntax,
                    "invalid token".into(),
                    location,
                    location + 1,
                ),
                ParseError::UnrecognizedEOF { location, .. } => Error::new(
                    ErrorType::Syntax,
                    "unexpected EOF".into(),
                    location - 1,
                    location,
                ),
                ParseError::UnrecognizedToken { token, .. } | ParseError::ExtraToken { token } => {
                    Error::new(
                        ErrorType::Syntax,
                        match token.1 {
                            Tok::Identifier(n) => format!("unexpected identifier \"{n}\""),
                            Tok::Op(op) => format!("unexpected operator \"{op:?}\""),
                            Tok::Keyword(k) => format!("unexpected keyword \"{k:?}\""),
                            Tok::Float(_) => "unexpected constant".into(),
                            Tok::Int(_) => "unexpected constant".into(),
                            Tok::Indent => "unexpected indent".into(),
                            Tok::Dedent => "unexpected dedent".into(),
                            Tok::Error(m) => m,
                            _ => format!("unexpected token \"{:?}\"", token.1),
                        },
                        token.0,
                        token.2,
                    )
                }
                ParseError::User { error } => error,
            };

            errors.push(error);
            CompilerState {
                tokens,
                errors,
                type_tokens: vec![],
                highlight_map: HashMap::default(),
                ast: HashMap::default(),
            }
        }
    }
}
