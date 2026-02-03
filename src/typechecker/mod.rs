mod types;

use crate::{error::Error, parser::Function};
use crate::parser::ParserOutput;
pub use types::Type;
mod from_annotation;

use crate::{parser::{Pos, ast::{S, Block, Expr}, lexer::{AssnOp, Op}}};
use crate::scope::Scope;

use from_annotation::annotation;

pub fn check_source(source: &mut ParserOutput) -> Vec<Error> {
    if let Some(tree) = &mut source.tree && source.errors.is_empty() {
        let mut errors = vec![];

        let mut global_types = Scope::default();
        let mut fail_check_signatures = false;

        for fun in tree.0.iter() {
            let p_args = Pos::collect(&fun.type_args);
            let mut args = vec![];
            for (_, ann) in fun.args.iter() {
                match annotation(ann, &p_args) {
                    Ok(t) => {
                        args.push(t);
                    },
                    Err(e) => {
                        errors.push(e);
                        fail_check_signatures = true;
                    }
                }
            }

            let mut ret_t = Type::Void;

            if let Some(ret) = &fun.ret {
                match annotation(ret, &p_args) {
                    Ok(t) => {
                        ret_t = t;
                    },
                    Err(e) => {
                        errors.push(e);
                        fail_check_signatures = true;
                    }
                }
            }

            global_types.insert(fun.name.data.clone(), Pos::new(
                fun.name.start, 
                Type::new_fn(p_args, args, ret_t),
                fun.name.end
            ));
        }

        if fail_check_signatures {
            return errors;
        }

        for fun in tree.0.iter_mut() {
            let p_args = Pos::collect(&fun.type_args);
            errors.extend(FunctionTypeChecker::run(&mut global_types, p_args, fun));
        }

        errors
    } else {
        vec![]
    }
}

enum Constraint {
    Eq(Pos<Type>, Pos<Type>),
    DemoteEq(Pos<Type>, Pos<Type>),
    BinOp(Pos<Type>, Op, Pos<Type>)
}

struct FunctionTypeChecker<'a> {
    pub errors: Vec<Error>,
    scope: &'a mut Scope<Pos<Type>>,
    p_vars: Vec<String>,
    constraints: Vec<Constraint>
}

impl<'a> FunctionTypeChecker<'a> {
    pub fn run(globals: &'a mut Scope<Pos<Type>>, p_vars: Vec<String>, ast: &'a mut Function) -> Vec<Error> {
        let mut checker = Self {
            errors: vec![],
            scope: globals,
            p_vars,
            constraints: vec![],
        };

        checker.scope.push();
        checker.traverse_block(&mut ast.body);
        checker.scope.pop();   

        checker.errors
    }

    fn visit_expr(&mut self, _expr: &Expr) {

    }

    fn visit_print(&mut self, expr: &Expr) {
        self.visit_expr(expr);
    }
    
    fn visit_return(&mut self, expr: &Expr) {
        self.visit_expr(expr);
    }
    
    fn visit_yield(&mut self, expr: &Expr) {
        self.visit_expr(expr);
    }

    fn visit_let(&mut self, _mutable: bool, _name: Pos<&str>, ann: Option<&Expr>, value: &Expr) {
        if let Some(ann) = ann {
            if let Err(e) = annotation(ann, &self.p_vars) {
                self.errors.push(e);
            }
        }

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
                S::Let{mutable, name, ann, value} => self.visit_let(*mutable, name.as_ref(), ann.as_ref(), value),
                S::Assign{place, op, value} => self.visit_assign(place, op, value),
                S::If{cond, true_body, false_body} => {
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
                S::For{iter, lower, upper, body} => {
                    self.scope.push();
                    self.visit_for(iter.as_ref(), lower, upper);
                    self.traverse_block(body);
                    self.scope.pop();
                }
            }
        }
    }
}