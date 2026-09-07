//! Driver and statement checking. Expressions live in [`expr`](super::expr).

use std::collections::HashMap;

use crate::error::{Errors, error_at};
use crate::parser::ast::{Block, E, Expr, Function, S};
use crate::parser::lexer::{ArithCmpOp, ArithOp, AssnOp, BoolOp, Op};
use crate::parser::{Pos, Span, UnaryOp};
use crate::solver::count::Count;
use crate::solver::poly::{Poly, Var};
use crate::solver::{Cmp, Constraint};
use crate::solver::{Solver, Verdict, t0};

use super::annotation::annotation;
use super::scope::{Frame, Scope};
use super::sig::{FnSig, check_recursion, signatures};
use super::ty::Type;
use super::typed::{Binding, BlockKind, Fact, TypedAst, TypedFn};

pub fn check_file(parse: &[Function], solver: &mut Solver) -> Errors {
    check_program(parse, solver).1
}

pub fn reserved_name(name: &str) -> bool {
    matches!(name, "null" | "_" | "len" | "shape")
}

/// Check a whole file, keeping what was learned about every function.
pub fn check_program(parse: &[Function], solver: &mut Solver) -> (TypedAst, Errors) {
    let (sigs, mut errors) = signatures(parse);
    errors.extend(check_recursion(parse, &sigs));

    let mut fns = vec![];
    let mut checked: Vec<&str> = vec![];
    for f in parse {
        // a redefinition is already reported; its body would be checked against the first signature
        if checked.contains(&f.name.data.as_str()) {
            continue;
        }
        checked.push(&f.name.data);

        let (typed, errs) = FnChecker::run(&sigs, solver, f, sigs[&f.name.data].clone());
        fns.push(typed);
        errors.extend(errs);
    }

    errors.sort_by_key(|e| (e.start, e.end));
    (TypedAst { fns }, errors)
}

pub struct FnChecker<'a> {
    pub sigs: &'a HashMap<String, FnSig>,
    pub solver: &'a mut Solver,
    pub sig: FnSig,
    pub scope: Scope,
    pub var_names: Vec<String>,
    pub nonneg: Vec<Var>,
    pub errors: Errors,
    pub exprs: HashMap<(usize, usize), Type>,
    pub yield_pos: Option<Span>,
    pub return_pos: Option<Span>,
    /// Set when a yield could not be counted, so the total is not worth reporting on.
    pub yields_unknown: bool,
}

impl<'a> FnChecker<'a> {
    fn run(
        sigs: &'a HashMap<String, FnSig>,
        solver: &'a mut Solver,
        f: &Function,
        sig: FnSig,
    ) -> (TypedFn, Errors) {
        let vars = sig.tv_names.clone();
        let mut checker = FnChecker {
            sigs,
            solver,
            scope: Scope::new(),
            nonneg: (0..vars.len() as Var).collect(),
            var_names: vars,
            errors: vec![],
            exprs: HashMap::new(),
            yield_pos: None,
            return_pos: None,
            yields_unknown: false,
            sig,
        };

        checker.scope.push(BlockKind::Body);
        for (i, tv) in checker.sig.tv_names.clone().iter().enumerate() {
            checker
                .scope
                .insert(tv, Type::Size(Poly::var(i as Var, 1)), false);
        }
        for (name, ty) in checker.sig.params.clone() {
            let ty = checker.skolemize(&ty, &name);
            checker.scope.insert(&name, ty, false);
        }
        for constraint in checker.sig.constraints.clone() {
            checker.scope.add_constraint(constraint);
        }

        checker.check_block(&f.body);
        let body = checker.scope.pop();
        checker.check_sig_satisfied(f, &body);

        let FnChecker {
            scope,
            var_names: vars,
            errors,
            exprs,
            sig,
            ..
        } = checker;

        (
            TypedFn {
                name: f.name.data.clone(),
                sig,
                vars,
                blocks: scope.into_blocks(),
                yields: body.yields,
                exprs,
            },
            errors,
        )
    }

    /// The function produces its return type: either enough `yield`s, or a `return` on every path.
    fn check_sig_satisfied(&mut self, f: &Function, body: &Frame) {
        if let (Some(y), Some(r)) = (self.yield_pos.clone(), self.return_pos.clone()) {
            let (pos, first) = if y.start < r.start {
                (r, "yield")
            } else {
                (y, "return")
            };
            self.errors.push(error_at!(
                Flow,
                &pos,
                "cannot mix \"yield\" and \"return\"; \"{first}\" appears above"
            ));
            return;
        }

        let ret_pos = match &f.ret {
            Some(ann) => Pos::span(ann.start, ann.end),
            None => Pos::span(f.name.start, f.name.end),
        };

        match (&self.sig.ret, self.yield_pos.clone()) {
            (Type::Error, _) => {}
            (Type::Array { shape, .. }, Some(_)) => {
                if self.yields_unknown {
                    return;
                }
                let size = shape.iter().fold(Poly::constant(1), |acc, d| acc.mul(d));
                let goal = Constraint::new(
                    body.yields.num().clone(),
                    Cmp::Eq,
                    size.mul_scalar(body.yields.den()),
                );
                let ctx = format!("yield count of \"{}\"", self.sig.name);
                self.prove(goal, &ret_pos, &ctx);
            }
            (ret, Some(y)) => {
                let names = self.names();
                self.errors.push(error_at!(
                    Type,
                    &y,
                    "cannot \"yield\" from a function returning \"{}\"",
                    ret.render(&names)
                ));
            }
            (Type::Unit, None) => {}
            (ret, None) if !body.always_returns => {
                let names = self.names();
                self.errors.push(error_at!(
                    Flow,
                    &ret_pos,
                    "\"{}\" must return \"{}\" on every path",
                    self.sig.name,
                    ret.render(&names)
                ));
            }
            _ => {}
        }
    }

    fn check_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            if self.scope.always_returns() {
                self.errors.push(error_at!(
                    Flow,
                    stmt,
                    "unreachable statement after \"return\""
                ));
                break;
            }
            self.check_stmt(stmt);
        }
    }

    fn check_stmt(&mut self, stmt: &Pos<S>) {
        match &stmt.data {
            S::Print(e) => {
                self.check_expr(e, None);
            }

            S::Return(e) => {
                self.return_pos
                    .get_or_insert(Pos::span(stmt.start, stmt.end));
                let ret = self.sig.ret.clone();
                let found = self.check_expr(e, Some(&ret));
                self.coerce(&found, &ret, e, "return value");
                self.scope.set_always_returns();
            }

            S::Yield(e) => {
                self.yield_pos
                    .get_or_insert(Pos::span(stmt.start, stmt.end));
                let elem = self.ret_elem();
                let found = self.check_expr(e, elem.as_ref());
                if let Some(elem) = elem {
                    self.coerce(&found, &elem, e, "yielded value");
                }
                self.scope.add_yields(&Count::constant(1));
            }

            S::YieldFrom(e) => {
                self.yield_pos
                    .get_or_insert(Pos::span(stmt.start, stmt.end));
                let found = self.check_expr(e, None);
                match found {
                    Type::Array { elem, shape } => {
                        if let Some(want) = self.ret_elem() {
                            self.coerce(&elem, &want, e, "yielded value");
                        }
                        let size = shape.iter().fold(Poly::constant(1), |acc, d| acc.mul(d));
                        self.scope.add_yields(&Count::ratio(size, 1));
                    }
                    Type::Error => self.yields_unknown = true,
                    other => {
                        let names = self.names();
                        self.errors.push(error_at!(
                            Type,
                            e,
                            "cannot \"yield from\" \"{}\"; expected an array",
                            other.render(&names)
                        ));
                    }
                }
            }

            S::Let {
                mutable,
                name,
                ann,
                value,
            } => {
                if reserved_name(&name.data) {
                    self.errors.push(error_at!(
                        NameResolution,
                        &name,
                        "\"{}\" is a reserved name",
                        name.data
                    ));

                    return;
                }
                let ann = ann.as_ref().map(|a| self.parse_ann(a));
                let found = self.check_expr(value, ann.as_ref());
                let bound = match ann {
                    Some(want) => {
                        let ctx = format!("initializer for \"{}\"", name.data);
                        self.coerce(&found, &want, value, &ctx);
                        want
                    }
                    // a mutable binding cannot keep a static value, since assignment would change it
                    None if *mutable => widen(&found),
                    None => found,
                };
                // a mutable binding cannot carry a skolem either -- it would go stale on assignment
                let bound = if *mutable {
                    bound
                } else {
                    self.skolemize(&bound, &name.data)
                };
                self.scope.insert(&name.data, bound, *mutable);
            }

            S::Assign { place, op, value } => self.check_assign(stmt, place, op, value),

            S::If {
                cond,
                true_body,
                false_body,
            } => self.check_if(stmt, cond, true_body, false_body.as_ref()),

            S::For {
                iter,
                lower,
                upper,
                body,
            } => self.check_for(iter, lower, upper, body),
        }
    }

    fn check_assign(&mut self, stmt: &Pos<S>, place: &Expr, op: &AssnOp, value: &Expr) {
        match place_root(place) {
            Some(root) => {
                if let Some(b) = self.scope.get(&root.data) {
                    if !b.mutable {
                        self.errors.push(error_at!(
                            Mutation,
                            &root,
                            "\"{}\" is immutable; declare it with \"let mut\"",
                            root.data
                        ));
                    } else if matches!((&place.data, &b.ty), (E::Ident(_), Type::Array { .. })) {
                        self.errors.push(error_at!(
                            Mutation,
                            &root,
                            "only array elements may be mutated",
                        ));
                    }
                }
            }
            None => self
                .errors
                .push(error_at!(Type, place, "expression is not a place")),
        }

        let place_t = self.check_expr(place, None);
        let found = self.check_expr(value, Some(&place_t));

        let found = match op {
            AssnOp::Eq => found,
            AssnOp::Add => self.arith(&place_t, &ArithOp::Add, &found, stmt),
            AssnOp::Sub => self.arith(&place_t, &ArithOp::Sub, &found, stmt),
            AssnOp::Mul => self.arith(&place_t, &ArithOp::Mul, &found, stmt),
            AssnOp::Div => self.arith(&place_t, &ArithOp::Div, &found, stmt),
        };

        self.coerce(&found, &place_t, value, "assigned value");
    }

    fn check_if(
        &mut self,
        stmt: &Pos<S>,
        cond: &Expr,
        true_body: &Block,
        false_body: Option<&Block>,
    ) {
        let cond_t = self.check_expr(cond, Some(&Type::Bool));
        if !matches!(cond_t, Type::Bool | Type::Error) {
            let names = self.names();
            self.errors.push(error_at!(
                Type,
                cond,
                "condition must be \"bool\", found \"{}\"",
                cond_t.render(&names)
            ));
        }

        self.scope.push(BlockKind::If);
        for fact in self.cond_facts(cond, true) {
            self.scope.add_fact(fact);
        }
        self.check_block(true_body);
        let true_frame = self.scope.pop();

        let false_frame = false_body.map(|body| {
            self.scope.push(BlockKind::Else);
            for fact in self.cond_facts(cond, false) {
                self.scope.add_fact(fact);
            }
            self.check_block(body);
            self.scope.pop()
        });

        let false_yields = match &false_frame {
            Some(f) => f.yields.clone(),
            None => Count::zero(),
        };
        if true_frame.yields == false_yields {
            self.scope.add_yields(&true_frame.yields);
        } else {
            self.yields_unknown = true;
            self.errors.push(error_at!(
                NotImplmented,
                stmt,
                "branches yield different counts; existential sizes are not implemented yet"
            ));
        }

        if true_frame.can_return || false_frame.as_ref().is_some_and(|f| f.can_return) {
            self.scope.set_can_return();
        }
        if true_frame.always_returns && false_frame.is_some_and(|f| f.always_returns) {
            self.scope.set_always_returns();
        }
    }

    fn check_for(&mut self, iter: &Pos<String>, lower: &Expr, upper: &Expr, body: &Block) {
        let lower_t = self.check_expr(lower, None);
        let upper_t = self.check_expr(upper, None);
        let bounds = match (
            self.loop_bound(&lower_t, lower),
            self.loop_bound(&upper_t, upper),
        ) {
            (Some(lo), Some(hi)) => Some((lo, hi)),
            _ => None,
        };

        let var = self.fresh_var(&iter.data);
        self.scope.push(BlockKind::For);

        if reserved_name(&iter.data) {
            self.errors.push(error_at!(
                NameResolution,
                &iter,
                "\"{}\" is a reserved name",
                iter.data
            ));
        } else {
            self.scope
                .insert(&iter.data, Type::Size(Poly::var(var, 1)), false);
        }

        if let Some((lo, hi)) = &bounds {
            self.scope
                .add_constraint(Constraint::new(Poly::var(var, 1), Cmp::Ge, lo.clone()));
            self.scope
                .add_constraint(Constraint::new(Poly::var(var, 1), Cmp::Lt, hi.clone()));
            if self.t0(Cmp::Ge, lo, &Poly::zero()) {
                self.nonneg.push(var);
            }
        }

        self.check_block(body);
        let frame = self.scope.pop();

        match &bounds {
            Some((lo, hi)) => {
                let total = frame.yields.sum_range(var, lo, hi);
                self.scope.add_yields(&total);
            }
            // without bounds there is nothing to sum over
            None => self.yields_unknown |= !frame.yields.is_zero(),
        }
        if frame.can_return {
            self.scope.set_can_return();
        }
    }

    /// Loop bounds have to be statically known, or the body cannot be counted.
    fn loop_bound(&mut self, t: &Type, pos: &Expr) -> Option<Poly> {
        match t {
            Type::Size(p) => Some(p.clone()),
            Type::Error => None,
            other => {
                let names = self.names();
                self.errors.push(error_at!(
                    Type,
                    pos,
                    "loop bound must be statically known, found \"{}\"",
                    other.render(&names)
                ));
                None
            }
        }
    }

    /// Facts a condition contributes to its branch. Only comparisons between statically known
    /// values say anything the solver can use.
    fn cond_facts(&self, cond: &Expr, positive: bool) -> Vec<Fact> {
        let mut out = vec![];
        self.collect_facts(cond, positive, &mut out);
        out
    }

    fn collect_facts(&self, cond: &Expr, positive: bool, out: &mut Vec<Fact>) {
        match &cond.data {
            E::BinOp(lhs, Op::ArithCmp(op), rhs) => {
                if let (Some(l), Some(r)) = (self.size_of(lhs), self.size_of(rhs)) {
                    let cmp = match op {
                        ArithCmpOp::Greater => Cmp::Gt,
                        ArithCmpOp::GreaterOrEq => Cmp::Ge,
                        ArithCmpOp::Less => Cmp::Lt,
                        ArithCmpOp::LessOrEq => Cmp::Le,
                        ArithCmpOp::Eq => Cmp::Eq,
                        ArithCmpOp::NotEq => Cmp::Ne,
                    };
                    let cmp = if positive { cmp } else { cmp.negate() };
                    out.push(Fact::Constraint(Constraint::new(l, cmp, r)));
                } else {
                    // x == null, x != null
                    if let (E::Ident(lhs_name), E::Ident(rhs_name)) = (&lhs.data, &rhs.data) {
                        let target_name = if rhs_name == "null" {
                            lhs_name
                        } else if lhs_name == "null" {
                            rhs_name
                        } else {
                            return;
                        };

                        if let Some(Binding { id, .. }) = self.scope.get(target_name) {
                            match (op, positive) {
                                (&ArithCmpOp::Eq, true) | (&ArithCmpOp::NotEq, false) => {
                                    out.push(Fact::IsNull(*id))
                                }
                                (&ArithCmpOp::Eq, false) | (&ArithCmpOp::NotEq, true) => {
                                    out.push(Fact::NotNull(*id))
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            // only the side that distributes over the connective is sound
            E::BinOp(lhs, Op::Bool(BoolOp::And), rhs) if positive => {
                self.collect_facts(lhs, positive, out);
                self.collect_facts(rhs, positive, out);
            }
            E::BinOp(lhs, Op::Bool(BoolOp::Or), rhs) if !positive => {
                self.collect_facts(lhs, positive, out);
                self.collect_facts(rhs, positive, out);
            }
            E::UnOp(UnaryOp::BoolNeg, inner) => self.collect_facts(inner, !positive, out),
            _ => {}
        }
    }

    /// The static value of an already-checked expression.
    fn size_of(&self, e: &Expr) -> Option<Poly> {
        self.exprs
            .get(&(e.start, e.end))
            .and_then(Type::as_size)
            .cloned()
    }

    fn ret_elem(&self) -> Option<Type> {
        match &self.sig.ret {
            Type::Array { elem, .. } => Some((**elem).clone()),
            _ => None,
        }
    }

    fn parse_ann(&mut self, ann: &Expr) -> Type {
        let names = self.sig.names();
        match annotation(ann, &names) {
            Ok(t) => t,
            Err(e) => {
                self.errors.push(e);
                Type::Error
            }
        }
    }

    pub fn names(&self) -> Vec<&str> {
        self.var_names.iter().map(String::as_str).collect()
    }

    pub fn fresh_var(&mut self, name: &str) -> Var {
        self.var_names.push(name.to_string());
        (self.var_names.len() - 1) as Var
    }

    /// Give an `Ind` value a name in the constraint system: a fresh [`Var`] `v` with
    /// `0 <= v < bound` on the enclosing block, exactly like a loop variable. Flow facts can then
    /// mention it (`if k > 0`), and arithmetic on it stays a `Size` instead of decaying to `i64`.
    ///
    /// `Option` and `Tuple` are descended into, since each position is one value. Array elements
    /// are left packed -- one element type stands for many values, which no single var can name.
    pub fn skolemize(&mut self, t: &Type, name: &str) -> Type {
        match t {
            Type::Ind(bound) => {
                let v = self.fresh_var(name);
                let value = Poly::var(v, 1);
                self.nonneg.push(v);
                self.scope
                    .add_constraint(Constraint::new(value.clone(), Cmp::Ge, Poly::zero()));
                self.scope
                    .add_constraint(Constraint::new(value.clone(), Cmp::Lt, bound.clone()));
                Type::Size(value)
            }
            Type::Option(inner) => Type::Option(Box::new(self.skolemize(inner, name))),
            Type::Tuple(items) => Type::Tuple(
                items
                    .iter()
                    .enumerate()
                    .map(|(i, t)| self.skolemize(t, &format!("{name}.{i}")))
                    .collect(),
            ),
            other => other.clone(),
        }
    }

    /// Prove without touching the solver; for choices that must not fail, like joins.
    pub fn t0(&self, cmp: Cmp, lhs: &Poly, rhs: &Poly) -> bool {
        t0::prove(cmp, lhs, rhs, &|v| self.nonneg.contains(&v))
    }

    pub fn prove_quiet(&mut self, goal: &Constraint) -> Option<String> {
        let constraints = self.scope.constraints();
        let verdict = self
            .solver
            .prove(&constraints, goal, &self.var_names, &self.nonneg);
        let var_names: Vec<&str> = self.var_names.iter().map(|x| x.as_ref()).collect();

        match verdict {
            Verdict::Proved => None,
            Verdict::RefutedBy(refutation) => {
                let mut msg = format!(
                    "disproved \"{}\"; {}",
                    goal.display_with(&var_names),
                    refutation.display_with(&var_names)
                );
                let mut goal_vars = goal.lhs.vars();
                goal_vars.extend(goal.rhs.vars());

                let mut relevant_nonneg: Vec<String> = vec![];
                let refutation_lhs_vars = refutation.lhs.vars();
                let refutation_rhs_vars = refutation.rhs.vars();

                for var in &self.nonneg {
                    if refutation_lhs_vars.contains(var) || refutation_rhs_vars.contains(var) {
                        relevant_nonneg.push(format!("{} >= 0", var_names[*var as usize]));
                    }
                }

                if !relevant_nonneg.is_empty() {
                    msg.push_str(&format!(" given {}", relevant_nonneg.join(", ")));
                }

                Some(msg)
            }
            Verdict::RefutedAt(point) => {
                let mut msg = format!("disproved \"{}\"", goal.display_with(&var_names));
                let mut goal_vars = goal.lhs.vars();
                goal_vars.extend(goal.rhs.vars());

                let mut s_point: Vec<String> = vec![];

                for (var, val) in point {
                    if goal_vars.contains(&var) {
                        match var_names.get(var as usize) {
                            Some(var) => {
                                s_point.push(format!("{var} = {val}"));
                            }
                            None => {
                                // nonlinear atom, should be impossible
                                unreachable!();
                            }
                        }
                    }
                }

                if !s_point.is_empty() {
                    msg.push_str(&format!("; counterexample: {}", s_point.join(", ")));
                }

                Some(msg)
            }
            Verdict::Unknown => Some(format!(
                "cannot prove \"{}\"",
                goal.display_with(&var_names)
            )),
        }
    }

    pub fn prove<T>(&mut self, goal: Constraint, pos: &Pos<T>, ctx: &str) -> bool {
        match self.prove_quiet(&goal) {
            None => true,
            Some(detail) => {
                self.errors.push(error_at!(Type, pos, "{ctx}: {detail}"));
                false
            }
        }
    }
}

/// Drop static values a later assignment could invalidate.
fn widen(t: &Type) -> Type {
    match t {
        Type::Size(_) | Type::Ind(_) => Type::I64,
        Type::Array { elem, shape } => Type::new_arr(widen(elem), shape.clone()),
        Type::Tuple(ts) => Type::Tuple(ts.iter().map(widen).collect()),
        Type::Option(t) => Type::Option(Box::new(widen(t))),
        other => other.clone(),
    }
}

/// The variable an assignment ultimately writes to.
fn place_root(place: &Expr) -> Option<Pos<String>> {
    match &place.data {
        E::Ident(name) => Some(Pos::new(place.start, name.clone(), place.end)),
        E::Access(inner, _) => place_root(inner),
        _ => None,
    }
}
