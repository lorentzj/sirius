//! Function signatures: building them from annotations, matching them against call sites,
//! and rejecting recursion.

use std::collections::HashMap;

use crate::error::{Errors, error_at};
use crate::parser::ast::{AD, Block, E, Expr, Function, S};
use crate::parser::lexer::ArithCmpOp;
use crate::parser::{Pos, Span};
use crate::solver::poly::coef::Coef;
use crate::solver::poly::{Poly, Var};
use crate::solver::{Cmp, Constraint};

use super::annotation::annotation;
use super::check::reserved_name;
use super::ty::Type;

/// [`Poly`]s here are expressed in function's typevar space,
/// where [`Var`] `i` is `tv_names[i]`; call sites map it into their own with [`Type::instantiate`].
///
/// `tv_names[..n_universal]` come from the `{all ..}` block and are chosen by the caller;
/// the rest come from `{ex ..}` and are chosen by the callee. The two halves run the obligations
/// in opposite directions -- see [`constraints`](Self::constraints) and
/// [`ex_constraints`](Self::ex_constraints).
#[derive(Clone, Debug)]
pub struct FnSig {
    pub name: String,
    pub tv_names: Vec<String>,
    pub n_universal: usize,
    pub params: Vec<(String, Type)>,
    pub ret: Type,
    /// Proven at call sites, assumed in the body.
    pub constraints: Vec<Constraint>,
    /// Proven in the body, assumed at call sites.
    pub ex_constraints: Vec<Constraint>,
}

impl FnSig {
    pub fn names(&self) -> Vec<&str> {
        self.tv_names.iter().map(String::as_str).collect()
    }

    /// The typevars a caller may pass, in order.
    pub fn universals(&self) -> &[String] {
        &self.tv_names[..self.n_universal]
    }

    pub fn existentials(&self) -> &[String] {
        &self.tv_names[self.n_universal..]
    }

    pub fn is_existential(&self, v: Var) -> bool {
        (v as usize) >= self.n_universal && (v as usize) < self.tv_names.len()
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

    // universals first, so a call site's positional typevars line up with `{all ..}` and anything
    // past that length is an attempt to fix an existential
    for tv in f.type_args.iter().chain(&f.ex_type_args) {
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
    let n_universal = f.type_args.len();
    let names: Vec<&str> = tv_names.iter().map(String::as_str).collect();
    let is_ex = |v: Var| (v as usize) >= n_universal;

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

    let mut params: Vec<(String, Type)> = f
        .args
        .iter()
        .map(|(name, ann)| (name.data.clone(), parse(ann, errors)))
        .collect();

    // the callee picks its existentials from what the body does, so they cannot describe an input
    for ((_, param), (_, ann)) in params.iter_mut().zip(&f.args) {
        if let Some(v) = type_vars(param).into_iter().find(|v| is_ex(*v)) {
            errors.push(error_at!(
                Type,
                ann,
                "\"{}\" is an existential typevar and cannot appear in an argument type",
                names[v as usize]
            ));
            *param = Type::Error;
        }
    }

    let ret = match &f.ret {
        Some(ann) => parse(ann, errors),
        None => Type::Unit,
    };

    // An existential is witnessed by what the body produces, which is only observable through the
    // return type. One that never appears there is never witnessed, yet its `ex` constraint would
    // still reach every caller as an unproven fact -- and an unsatisfiable one (`B < 0`) would
    // make the caller's whole fact set contradictory.
    if !ret.is_error() {
        let in_ret = type_vars(&ret);
        for (j, tv) in f.ex_type_args.iter().enumerate() {
            if !in_ret.contains(&((n_universal + j) as Var)) {
                errors.push(error_at!(
                    Type,
                    tv,
                    "existential typevar \"{}\" does not appear in the return type, so nothing \
                     can witness it",
                    tv.data
                ));
            }
        }
    }

    let parse_constraint = |(tv, op, ann): &(Pos<String>, ArithCmpOp, Expr),
                            want_ex: bool,
                            errors: &mut Errors|
     -> Option<Constraint> {
        let v = match names.iter().position(|n| *n == tv.data) {
            Some(v) => v as Var,
            None => {
                errors.push(error_at!(
                    NameResolution,
                    tv,
                    "cannot find typevar \"{}\" in signature",
                    tv.data
                ));
                return None;
            }
        };
        if is_ex(v) != want_ex {
            let (found, block) = if want_ex {
                ("universal", "all")
            } else {
                ("existential", "ex")
            };
            errors.push(error_at!(
                Type,
                tv,
                "\"{}\" is a {found} typevar, so its constraint belongs in the \"{block}\" block",
                tv.data
            ));
            return None;
        }
        match annotation(ann, &names) {
            Ok(Type::Size(bound)) => {
                // a caller must be able to discharge an `all` constraint, so it cannot mention
                // a var only the callee knows
                if !want_ex && let Some(ex) = bound.vars().into_iter().find(|v| is_ex(*v)) {
                    errors.push(error_at!(
                        Type,
                        ann,
                        "\"{}\" is an existential typevar and cannot bound a universal one",
                        names[ex as usize]
                    ));
                    return None;
                }
                Some(Constraint::new(Poly::var(v, 1), Cmp::from_lex(op), bound))
            }
            Ok(t) => {
                errors.push(error_at!(
                    NotImplmented,
                    ann,
                    "typevar constraint must be a poly; found \"{}\"",
                    t.render(&names)
                ));
                None
            }
            Err(e) => {
                errors.push(e);
                None
            }
        }
    };

    let constraints = f
        .type_constraints
        .iter()
        .filter_map(|c| parse_constraint(c, false, errors))
        .collect();
    let ex_constraints = f
        .ex_type_constraints
        .iter()
        .filter_map(|c| parse_constraint(c, true, errors))
        .collect();

    FnSig {
        name: f.name.data.clone(),
        tv_names,
        n_universal,
        params,
        ret,
        constraints,
        ex_constraints,
    }
}

/// Every [`Var`] mentioned anywhere in a type.
pub fn type_vars(t: &Type) -> Vec<Var> {
    let mut out = vec![];
    collect_type_vars(t, &mut out);
    out
}

fn collect_type_vars(t: &Type, out: &mut Vec<Var>) {
    let push = |p: &Poly, out: &mut Vec<Var>| {
        for v in p.vars() {
            if !out.contains(&v) {
                out.push(v);
            }
        }
    };
    match t {
        Type::Array { elem, shape } => {
            for d in shape {
                push(d, out);
            }
            collect_type_vars(elem, out);
        }
        Type::Ind(p) | Type::Size(p) => push(p, out),
        Type::Option(inner) => collect_type_vars(inner, out),
        Type::Tuple(items) => {
            for t in items {
                collect_type_vars(t, out);
            }
        }
        _ => {}
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
