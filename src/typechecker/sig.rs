//! Function signatures: building them from annotations, matching them against call sites,
//! and rejecting recursion.

use std::collections::HashMap;

use crate::error::{Errors, error_at};
use crate::parser::ast::{AD, Block, E, Expr, Function, S};
use crate::parser::{Pos, Span};
use crate::solver::poly::coef::Coef;
use crate::solver::poly::{Poly, Var};
use crate::solver::z3::{Cmp, Constraint};

use super::annotation::annotation;
use super::ty::Type;
use super::check::reserved_name;

/// [`Poly`]s here are expressed in function's typevar space,
/// where [`Var`] `i` is `tv_names[i]`; call sites map it into their own with [`Type::instantiate`].
#[derive(Clone, Debug)]
pub struct FnSig {
    pub name: String,
    pub tv_names: Vec<String>,
    pub params: Vec<(String, Type)>,
    pub ret: Type,
    /// Assumed in the body, proven at call sites.
    pub constraints: Vec<Constraint>,
}

impl FnSig {
    pub fn names(&self) -> Vec<&str> {
        self.tv_names.iter().map(String::as_str).collect()
    }
}

pub fn signatures(fns: &[Function]) -> (HashMap<String, FnSig>, Errors) {
    let mut sigs = HashMap::new();
    let mut errors = vec![];

    for f in fns {
        let sig = build(f, &mut errors);
        if sigs.contains_key(&f.name.data) {
            errors.push(error_at!(
                NameResolution,
                &f.name,
                "function \"{}\" is already defined",
                f.name.data
            ));
        } else {
            sigs.insert(f.name.data.clone(), sig);
        }
    }

    (sigs, errors)
}

fn build(f: &Function, errors: &mut Errors) -> FnSig {
    let mut tv_names: Vec<String> = vec![];

    if reserved_name(&f.name.data) {
        errors.push(error_at!(
            NameResolution,
            &f.name,
            "\"{}\" is a reserved name",
            f.name.data
        ));
    }

    for tv in &f.type_args {

        if reserved_name(&tv.data) {
            errors.push(error_at!(
                NameResolution,
                &tv,
                "\"{}\" is a reserved name",
                tv.data
            ));
        }

        if tv_names.contains(&tv.data) {
            errors.push(error_at!(
                NameResolution,
                tv,
                "typevar \"{}\" is already defined",
                tv.data
            ));
        }
        tv_names.push(tv.data.clone());
    }
    let names: Vec<&str> = tv_names.iter().map(String::as_str).collect();

    let parse = |ann: &Expr, errors: &mut Errors| match annotation(ann, &names) {
        Ok(t) => t,
        Err(e) => {
            errors.push(e);
            Type::Error
        }
    };

    for (arg_name, _) in &f.args {
        if reserved_name(&arg_name.data) {
            errors.push(error_at!(
                NameResolution,
                &arg_name,
                "\"{}\" is a reserved name",
                arg_name.data
            ));
        }
    }

    let params = f
        .args
        .iter()
        .map(|(name, ann)| (name.data.clone(), parse(ann, errors)))
        .collect();

    let ret = match &f.ret {
        Some(ann) => parse(ann, errors),
        None => Type::Unit,
    };

    let mut constraints = vec![];
    for (tv, op, ann) in &f.type_constraints {
        let Some(v) = names.iter().position(|n| *n == tv.data) else {
            errors.push(error_at!(
                NameResolution,
                tv,
                "cannot find typevar \"{}\" in signature",
                tv.data
            ));
            continue;
        };
        match annotation(ann, &names) {
            Ok(Type::Size(bound)) => constraints.push(Constraint::new(
                Poly::var(v as Var, 1),
                Cmp::from_lex(op),
                bound.clone(),
            )),
            Ok(t) => errors.push(error_at!(
                NotImplmented,
                ann,
                "typevar constraint must be a poly; found \"{}\"",
                t.render(&names)
            )),
            Err(e) => errors.push(e),
        }
    }

    FnSig {
        name: f.name.data.clone(),
        tv_names,
        params,
        ret,
        constraints,
    }
}

/// Solve for a callee typevar wherever the parameter type pins one down against `arg`.
/// Bindings map the callee's [`Var`]s to polynomials in the caller's typevars.
pub fn match_ty(param: &Type, arg: &Type, subst: &mut HashMap<Var, Poly>) {
    match (param, arg) {
        (
            Type::Array {
                elem: p_elem,
                shape: p_shape,
            },
            Type::Array {
                elem: a_elem,
                shape: a_shape,
            },
        ) if p_shape.len() == a_shape.len() => {
            for (p, a) in p_shape.iter().zip(a_shape) {
                match_poly(p, a, subst);
            }
            match_ty(p_elem, a_elem, subst);
        }
        (Type::Ind(p), Type::Ind(a)) | (Type::Size(p), Type::Size(a)) => match_poly(p, a, subst),
        (Type::Option(p), Type::Option(a)) => match_ty(p, a, subst),
        (Type::Tuple(ps), Type::Tuple(a_s)) if ps.len() == a_s.len() => {
            for (p, a) in ps.iter().zip(a_s) {
                match_ty(p, a, subst);
            }
        }
        _ => {}
    }
}

/// Solve `param = arg` for a single unbound typevar appearing linearly, e.g. `2*N + 1 = 2*M + 1`
/// binds `N := M`. Anything harder is left to the call site's equality obligation.
fn match_poly(param: &Poly, arg: &Poly, subst: &mut HashMap<Var, Poly>) {
    let unbound: Vec<Var> = param
        .vars()
        .into_iter()
        .filter(|v| !subst.contains_key(v))
        .collect();
    if unbound.len() != 1 {
        return;
    }
    let v = unbound[0];

    let mut coef = Coef::new(0);
    let mut rest = vec![];
    for (c, m) in param.terms() {
        match m.exps() {
            [(w, 1)] if *w == v => coef = coef + c,
            _ if m.degree_in(v) > 0 => return,
            _ => rest.push((c, m)),
        }
    }
    if coef.is_zero() {
        return;
    }

    let rest = Poly::from_terms(rest).map_vars(|w| subst[&w].clone());
    if let Some(solution) = arg.sub(&rest).divide_coefs(coef) {
        subst.insert(v, solution);
    }
}

/// Sirius is total, so the call graph must be acyclic.
pub fn check_recursion(fns: &[Function], sigs: &HashMap<String, FnSig>) -> Errors {
    let mut graph: HashMap<&str, Vec<(String, Span)>> = HashMap::new();
    for f in fns {
        let mut calls = vec![];
        block_calls(&f.body, &mut calls);
        calls.retain(|(name, _)| sigs.contains_key(name));
        graph.insert(&f.name.data, calls);
    }

    let mut errors = vec![];
    let mut done: Vec<&str> = vec![];
    for f in fns {
        let mut path = vec![];
        walk(&f.name.data, &graph, &mut path, &mut done, &mut errors);
    }
    errors
}

fn walk<'a>(
    name: &'a str,
    graph: &'a HashMap<&str, Vec<(String, Span)>>,
    path: &mut Vec<&'a str>,
    done: &mut Vec<&'a str>,
    errors: &mut Errors,
) {
    if done.contains(&name) {
        return;
    }
    path.push(name);
    for (callee, pos) in graph.get(name).into_iter().flatten() {
        if path.contains(&callee.as_str()) {
            errors.push(error_at!(
                Flow,
                pos,
                "recursive call to \"{callee}\" is not allowed"
            ));
        } else {
            walk(callee, graph, path, done, errors);
        }
    }
    path.pop();
    done.push(name);
}

fn block_calls(block: &Block, out: &mut Vec<(String, Span)>) {
    for stmt in &block.stmts {
        match &stmt.data {
            S::Print(e) | S::Return(e) | S::Yield(e) | S::YieldFrom(e) => expr_calls(e, out),
            S::Let { ann, value, .. } => {
                if let Some(ann) = ann {
                    expr_calls(ann, out);
                }
                expr_calls(value, out);
            }
            S::Assign { place, value, .. } => {
                expr_calls(place, out);
                expr_calls(value, out);
            }
            S::If {
                cond,
                true_body,
                false_body,
            } => {
                expr_calls(cond, out);
                block_calls(true_body, out);
                if let Some(b) = false_body {
                    block_calls(b, out);
                }
            }
            S::For {
                lower, upper, body, ..
            } => {
                expr_calls(lower, out);
                expr_calls(upper, out);
                block_calls(body, out);
            }
        }
    }
}

fn expr_calls(expr: &Expr, out: &mut Vec<(String, Span)>) {
    match &expr.data {
        E::FnCall(caller, type_args, args) => {
            if let E::Ident(name) = &caller.data {
                out.push((name.clone(), Pos::span(expr.start, expr.end)));
            } else {
                expr_calls(caller, out);
            }
            for e in type_args.iter().chain(args) {
                expr_calls(e, out);
            }
        }
        E::Tuple(items) | E::Array(items) => {
            for e in items {
                expr_calls(e, out);
            }
        }
        E::UnOp(_, inner) => expr_calls(inner, out),
        E::BinOp(lhs, _, rhs) => {
            expr_calls(lhs, out);
            expr_calls(rhs, out);
        }
        E::Access(arr, dims) => {
            expr_calls(arr, out);
            for dim in dims {
                match &dim.data {
                    AD::Point(e) => expr_calls(e, out),
                    AD::Range(from, to) => {
                        for e in from.iter().chain(to) {
                            expr_calls(e, out);
                        }
                    }
                }
            }
        }
        E::Bool(_) | E::Float(_) | E::Int(_) | E::Ident(_) => {}
    }
}
