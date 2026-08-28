pub mod ast;
pub mod lexer;
pub mod positioned;

use crate::error::{Error, ErrorType, Errors};
pub use ast::{AccessDim, Block, Expr, Function, Stmt, Tree, UnaryOp};
pub use lexer::{Tok, Token};
pub use positioned::Pos;

lalrpop_util::lalrpop_mod!(#[allow(clippy::all)] pub grammar, "/parser/grammar.rs");

pub struct ParserOutput {
    pub tokens: Vec<Token>,
    pub tree: Option<Tree>,
    pub errors: Errors,
}

pub fn parse(code: &str) -> ParserOutput {
    let mut tokens = lexer::tokenize(code);

    let errors: Vec<_> = tokens
        .iter()
        .enumerate()
        .filter_map(|(i, t)| {
            t.get_error()
                .map(|msg| Error::new_with(ErrorType::Syntax, msg.clone(), i, i))
        })
        .collect();

    let tokens_no_comments_iter = tokens
        .iter()
        .enumerate()
        .filter(|(_, t)| !t.is_comment())
        .map(|(i, token)| Ok((i, token.data.clone(), i + 1)));

    let mut type_tokens = vec![];

    if errors.is_empty() {
        let parser_output =
            grammar::TreeParser::new().parse(&mut type_tokens, tokens_no_comments_iter);

        for i in type_tokens {
            tokens[i].is_type_ann = true;
        }

        match parser_output {
            Ok(tree) => ParserOutput {
                tokens,
                tree: Some(tree),
                errors,
            },

            Err(err) => ParserOutput {
                tokens,
                tree: None,
                errors: vec![Error::from_lalrpop(err)],
            },
        }
    } else {
        ParserOutput {
            tokens,
            tree: None,
            errors,
        }
    }
}

#[cfg(test)]
pub fn parse_expr(code: &str) -> Option<Expr> {
    let tokens = lexer::tokenize(code);

    for token in &tokens {
        if token.get_error().is_some() {
            return None;
        }
    }

    let tokens_no_comments_iter = tokens
        .iter()
        .enumerate()
        .filter(|(_, t)| !t.is_comment())
        .map(|(i, token)| Ok((i, token.data.clone(), i + 1)));

    grammar::ExprParser::new()
        .parse(&mut vec![], tokens_no_comments_iter)
        .ok()
}
