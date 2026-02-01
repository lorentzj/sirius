use std::{collections::HashMap, rc::Rc};

pub(crate) mod ast;
pub(crate) mod lexer;

mod positioned;

use crate::error::{Error, ErrorType};
pub use ast::{AccessDim, Block, E, Expr, Function, Stmt, Tree, TypeArg, UnaryOp};
pub use lexer::{Tok, Token};
pub use positioned::Pos;

lalrpop_util::lalrpop_mod!(#[allow(clippy::all)] pub grammar, "/parser/grammar.rs");

pub fn add_highlight_pair(map: &mut HashMap<usize, Vec<usize>>, a: usize, b: usize) {
    map.insert(a, vec![a, b]);
    map.insert(b, vec![a, b]);
}

pub struct ParserOutput {
    pub code: String,
    pub tokens: Vec<Token>,
    pub tree: Option<Rc<Tree>>,
    pub highlight_map: HashMap<usize, Vec<usize>>,
    pub errors: Vec<Error>,
}

pub fn parse(code: String) -> ParserOutput {
    let mut tokens = lexer::tokenize(&code);

    let errors: Vec<_> = tokens
        .iter()
        .enumerate()
        .filter_map(|(i, t)| match &t.data {
            Tok::Error(msg) => Some(Error::new(ErrorType::Syntax, msg.clone(), i, i)),
            Tok::IndentError(msg) => Some(Error::new(ErrorType::Syntax, msg.clone(), i, i)),
            _ => None,
        })
        .collect();

    let tokens_no_comments_iter = tokens
        .iter()
        .enumerate()
        .filter(|(_, t)| !t.is_comment())
        .map(|(i, token)| Ok((i, token.data.clone(), i + 1)));

    let mut highlight_map = HashMap::default();
    let mut type_tokens = vec![];

    if errors.is_empty() {
        let parser_output = grammar::TreeParser::new().parse(
            &mut highlight_map,
            &mut type_tokens,
            tokens_no_comments_iter,
        );

        for i in type_tokens {
            tokens[i].is_type_ann = true;
        }

        match parser_output {
            Ok(tree) => ParserOutput {
                code,
                tokens,
                tree: Some(Rc::new(tree)),
                highlight_map,
                errors,
            },

            Err(err) => ParserOutput {
                code,
                tokens,
                tree: None,
                highlight_map,
                errors: vec![Error::from_lalrpop(err)],
            },
        }
    } else {
        ParserOutput {
            code,
            tokens,
            tree: None,
            highlight_map,
            errors,
        }
    }
}
