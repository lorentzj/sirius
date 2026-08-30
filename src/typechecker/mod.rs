mod check_stmt;
mod from_annotation;
mod scope;
mod types;
mod visit;

use crate::parser::ParserOutput;
use crate::parser::{
    Pos,
    ast::{Block, S},
};

use crate::typechecker::scope::{BlockScope, BlockScopeEntry};
use crate::typechecker::types::T;
use crate::{
    error::{Errors, error_at},
    parser::Function,
};
use scope::Scopes;

use from_annotation::{annotation, fun_type};

pub use types::Type;

pub fn check_source(source: &mut ParserOutput) {
    if let Some(tree) = &mut source.tree
        && source.errors.is_empty()
    {
        let mut scopes = Scopes::new();
        scopes.push();

        let mut fail_check_signatures = false;

        for fun in &tree.0 {
            match fun_type(fun) {
                Ok(t) => {
                    scopes.insert(
                        fun.name.data.clone(),
                        Type::new_at(t.data, &fun.name),
                        false,
                    );
                }
                Err(e) => {
                    scopes.insert(fun.name.data.clone(), Type::error_at(&fun.name), false);
                    source.errors.extend(e);
                    fail_check_signatures = true;
                }
            }
        }

        if fail_check_signatures {
            return;
        }

        for fun in tree.0.iter_mut() {
            let p_args = Pos::inner_collect(&fun.type_args);
            source
                .errors
                .extend(FunctionTypeChecker::run(&mut scopes, p_args, fun));
        }
    }
}

struct FunctionTypeChecker<'a> {
    errors: Errors,
    ctx: &'a mut Scopes,
    p_vars: Vec<String>,
}

impl<'a> FunctionTypeChecker<'a> {
    pub fn run(globals: &'a mut Scopes, p_vars: Vec<String>, ast: &'a mut Function) -> Errors {
        let mut checker = Self {
            errors: vec![],
            ctx: globals,
            p_vars,
        };

        checker.ctx.n_typevars(ast.type_args.len() as u32);
        checker.ctx.push();

        checker.traverse_block(&ast.body);

        checker.ctx.pop();

        checker.errors
    }

    fn traverse_block(&mut self, block: &Block) {
        if !self.errors.is_empty() {
            return;
        }

        for stmt in block.stmts.iter() {
            if matches!(
                self.ctx.peek(),
                Some(BlockScope {
                    always_returns: true,
                    ..
                })
            ) {
                self.errors
                    .push(error_at!(Flow, stmt, "statement after return"));
                break;
            }

            match &stmt.data {
                S::Print(expr) => self.visit_print(expr),
                S::Return(expr) => self.visit_return(expr),
                S::Yield(expr) => self.visit_yield(expr),
                S::YieldFrom(expr) => self.visit_yield_from(expr),
                S::Let {
                    mutable,
                    name,
                    ann,
                    value,
                } => self.visit_let(*mutable, name.as_ref(), ann.as_ref(), value),
                S::Assign { place, op, value } => self.visit_assign(place, op, value),
                S::If {
                    cond,
                    true_body,
                    false_body,
                } => self.visit_if(cond, true_body, false_body),
                S::For {
                    iter,
                    lower,
                    upper,
                    body,
                } => self.visit_for(iter.as_ref(), lower, upper, body),
            }
        }
    }
}
