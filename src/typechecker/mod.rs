mod from_annotation;
mod types;

use crate::parser::ParserOutput;
use crate::parser::{
    Pos,
    ast::{Block, Expr, S},
    lexer::AssnOp,
};
use crate::scope::Scope;
use crate::typechecker::types::T;
use crate::{error::Errors, parser::Function};

use from_annotation::{annotation, fun_type};

pub use types::Type;

pub fn check_source(source: &mut ParserOutput) {
    if let Some(tree) = &mut source.tree
        && source.errors.is_empty()
    {
        let mut global_types = Scope::default();
        global_types.push();
        let mut fail_check_signatures = false;

        for fun in &tree.0 {
            match fun_type(fun) {
                Ok(t) => {
                    global_types.insert(fun.name.data.clone(), Pos::new_at(t.data, &fun.name));
                }
                Err(e) => {
                    global_types.insert(fun.name.data.clone(), Pos::new_at(T::Error, &fun.name));
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
                .extend(FunctionTypeChecker::run(&mut global_types, p_args, fun));
        }
    }
}

struct FunctionTypeChecker<'a> {
    errors: Errors,
    scope: &'a mut Scope<Type>,
    p_vars: Vec<String>,
}

impl<'a> FunctionTypeChecker<'a> {
    pub fn run(globals: &'a mut Scope<Type>, p_vars: Vec<String>, ast: &'a mut Function) -> Errors {
        let mut checker = Self {
            errors: vec![],
            scope: globals,
            p_vars,
        };

        checker.scope.push();
        checker.traverse_block(&ast.body);
        checker.scope.pop();

        checker.errors
    }

    fn visit_expr(&mut self, _expr: &Expr) {}

    fn visit_print(&mut self, expr: &Expr) {
        self.visit_expr(expr);
    }

    fn visit_return(&mut self, expr: &Expr) {
        self.visit_expr(expr);
    }

    fn visit_yield(&mut self, expr: &Expr) {
        self.visit_expr(expr);
    }

    fn visit_yield_from(&mut self, expr: &Expr) {
        self.visit_expr(expr);
    }

    fn visit_let(&mut self, _mutable: bool, _name: Pos<&str>, ann: Option<&Expr>, value: &Expr) {
        let _ann = if let Some(ann) = ann {
            match annotation(ann, &self.p_vars) {
                Ok(ann) => Some(ann),
                Err(e) => {
                    self.errors.push(e);
                    None
                }
            }
        } else {
            None
        };

        self.visit_expr(value);
    }

    fn visit_assign(&mut self, _place: &Expr, _op: &AssnOp, value: &Expr) {
        self.visit_expr(value);
    }

    fn visit_if(&mut self, cond: &Expr) {
        self.visit_expr(cond);
    }

    fn visit_for(&mut self, _iter: Pos<&str>, lower: &Expr, upper: &Expr) {
        self.visit_expr(lower);
        self.visit_expr(upper);
    }

    fn traverse_block(&mut self, block: &Block) {
        for stmt in block.stmts.iter() {
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
                } => {
                    self.scope.push();
                    self.visit_if(cond);
                    self.scope.push();
                    self.traverse_block(true_body);
                    self.scope.pop();
                    if let Some(false_body) = false_body {
                        self.scope.push();
                        self.traverse_block(false_body);
                        self.scope.pop();
                    }
                    self.scope.pop();
                }
                S::For {
                    iter,
                    lower,
                    upper,
                    body,
                } => {
                    self.scope.push();
                    self.visit_for(iter.as_ref(), lower, upper);
                    self.traverse_block(body);
                    self.scope.pop();
                }
            }
        }
    }
}
