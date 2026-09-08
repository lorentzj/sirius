//! Driver and statement checking. Expressions live in [`expr`](super::expr).

use std::collections::HashMap;

use crate::error::{Errors, error_at};
use crate::parser::ast::{Block, E, Expr, Function, S};
use crate::parser::lexer::{ArithCmpOp, ArithOp, AssnOp, BoolOp, Op};
use crate::parser::{Pos, Span, UnaryOp};
use crate::solver::count::Count;
use crate::solver::poly::coef::Coef;
use crate::solver::poly::{Poly, Var};
use crate::solver::{Cmp, Constraint};
use crate::solver::{Solver, Verdict, t0};

use super::annotation::annotation;
use super::scope::Scope;
use super::sig::{self, FnSig, check_recursion, match_ty, signatures};
use super::ty::Type;
use super::typed::{Binding, BlockKind, Fact, TypedAst, TypedFn};
use super::yields::Yields;

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
    /// Witnesses for existentials read off `return` statements, and whether reading one failed.
    pub ex_witness: HashMap<Var, Poly>,
    pub ex_witness_failed: bool,
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
            ex_witness: HashMap::new(),
            ex_witness_failed: false,
            sig,
        };

        checker.scope.push(BlockKind::Body);
        for (i, tv) in checker.sig.universals().to_vec().iter().enumerate() {
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
        let (yields, always_returns) = {
            let frame = checker.scope.frame();
            (frame.yields.clone(), frame.always_returns)
        };
        checker.check_sig_satisfied(f, &yields, always_returns);
        let body = checker.scope.pop();

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
    fn check_sig_satisfied(&mut self, f: &Function, yields: &Yields, always_returns: bool) {
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
                let size = shape.iter().fold(Poly::constant(1), |acc, d| acc.mul(d));
                self.check_yield_count(&size, yields, &ret_pos);
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
            (ret, None) if !always_returns => {
                let names = self.names();
                self.errors.push(error_at!(
                    Flow,
                    &ret_pos,
                    "\"{}\" must return \"{}\" on every path",
                    self.sig.name,
                    ret.render(&names)
                ));
            }
            (_, None) => self.witness_returned_existentials(&ret_pos),
        }
    }

    /// Reconcile the `yield` count with the declared return size.
    ///
    /// Without existentials the size is fixed, so the count must match it exactly. With one, the
    /// size is whatever the body produced, so the job is to *witness* the existential rather than
    /// check it: the count becomes a fact about it, and the `ex` block becomes the obligation.
    ///
    /// The witness only exists if the declared size can actually take the count's value. `[B]`
    /// always can. `[2*B]` can only when the body yields an even number of times, which is what
    /// [`Yields::stride`] records. Sizes that are not a constant multiple of a single existential
    /// are rejected outright, since nothing here can produce a witness for them.
    fn check_yield_count(&mut self, size: &Poly, yields: &Yields, pos: &Span) {
        let ex: Vec<Var> = sig::type_vars(&self.sig.ret)
            .into_iter()
            .filter(|v| self.sig.is_existential(*v))
            .collect();

        let Some(count) = yields.bound() else {
            // an earlier error already explains why nothing could be counted
            return;
        };
        let (num, den) = (count.num().clone(), count.den());
        let names = self.names();

        if ex.is_empty() {
            if !yields.is_exact() {
                self.errors.push(error_at!(
                    Flow,
                    pos,
                    "\"{}\" yields a variable number of times, so its size cannot be \"{}\"; \
                     declare it existentially, e.g. \"{{ex M st M <= {}}}\" and return \"[M]\"",
                    self.sig.name,
                    self.sig.ret.render(&names),
                    num.display_with(Some(&names))
                ));
                return;
            }
            let goal = Constraint::new(num, Cmp::Eq, size.mul_scalar(den));
            let ctx = format!("yield count of \"{}\"", self.sig.name);
            self.prove(goal, pos, &ctx);
            return;
        }

        // the size has to be `k * B` for one existential `B` and a non-zero integer `k`
        let Some((k, var)) = as_scaled_var(size, &ex) else {
            self.errors.push(error_at!(
                Type,
                pos,
                "cannot witness \"{}\" from the yield count; an existential size must be a \
                 typevar times a constant, like \"[{}]\" or \"[2*{}]\"",
                self.sig.ret.render(&names),
                names[ex[0] as usize],
                names[ex[0] as usize]
            ));
            return;
        };

        // `k*B == count` needs the count to be a multiple of k -- exactly, or (when only bounded)
        // for every count the body can reach
        let divisor = k * den;
        let divides = if yields.is_exact() {
            num.divide_coefs(divisor).is_some()
        } else {
            !divisor.is_zero() && yields.stride().divrem(divisor).1.is_zero()
        };
        if !divides {
            self.errors.push(error_at!(
                Type,
                pos,
                "\"{}\" yields {} {} times, which \"{}\" cannot represent",
                self.sig.name,
                if yields.is_exact() {
                    "exactly"
                } else {
                    "up to"
                },
                num.display_with(Some(&names)),
                self.sig.ret.render(&names)
            ));
            return;
        }

        let value = Poly::var(var, 1).mul_scalar(divisor);
        let cmp = if yields.is_exact() { Cmp::Eq } else { Cmp::Le };
        self.scope.add_constraint(Constraint::new(value, cmp, num));
        self.scope
            .add_constraint(Constraint::new(Poly::var(var, 1), Cmp::Ge, Poly::zero()));

        self.prove_ex_constraints(pos);
    }

    /// The `ex` block is the body's obligation, discharged once the existentials have witnesses.
    fn prove_ex_constraints(&mut self, pos: &Span) {
        for constraint in self.sig.ex_constraints.clone() {
            let ctx = format!("existential constraint of \"{}\"", self.sig.name);
            self.prove(constraint, pos, &ctx);
        }
    }

    fn existentials_in(&self, t: &Type) -> Vec<Var> {
        sig::type_vars(t)
            .into_iter()
            .filter(|v| self.sig.is_existential(*v))
            .collect()
    }

    /// A `return` in a function with existentials in its return type *witnesses* them: the
    /// declared type is matched against what is actually returned rather than checked against it.
    fn check_return_witness(&mut self, e: &Expr, declared: &Type, ex: &[Var]) {
        // no hint -- the declared type still mentions vars this return is about to decide
        let found = self.check_expr(e, None);
        if found.is_error() {
            self.ex_witness_failed = true;
            return;
        }

        // seed the universals as themselves so `match_ty` only solves the existentials
        let mut subst: HashMap<Var, Poly> = (0..self.sig.n_universal as Var)
            .map(|v| (v, Poly::var(v, 1)))
            .collect();
        match_ty(declared, &found, &mut subst);

        let Ok(want) = declared.instantiate(&subst) else {
            let names = self.names();
            let unsolved: Vec<&str> = ex
                .iter()
                .filter(|v| !subst.contains_key(v))
                .map(|v| names[*v as usize])
                .collect();
            self.errors.push(error_at!(
                Type,
                e,
                "cannot tell what \"{}\" is from this return value, which has type \"{}\"",
                unsolved.join("\", \""),
                found.render(&names)
            ));
            self.ex_witness_failed = true;
            return;
        };
        self.coerce(&found, &want, e, "return value");

        for &v in ex {
            let witness = subst[&v].clone();
            match self.ex_witness.get(&v) {
                // every return has to agree, since one signature promises one size
                Some(prior) if *prior != witness => {
                    let names = self.names();
                    self.errors.push(error_at!(
                        Type,
                        e,
                        "\"{}\" is already witnessed as \"{}\" by an earlier return, but this one \
                         gives \"{}\"",
                        names[v as usize],
                        prior.display_with(Some(&names)),
                        witness.display_with(Some(&names))
                    ));
                    self.ex_witness_failed = true;
                }
                _ => {
                    self.ex_witness.insert(v, witness);
                }
            }
        }
    }

    /// The mirror of [`check_yield_count`](Self::check_yield_count) for functions that `return`.
    fn witness_returned_existentials(&mut self, pos: &Span) {
        let ex = self.existentials_in(&self.sig.ret.clone());
        if ex.is_empty() || self.ex_witness_failed {
            return;
        }
        for v in ex {
            let Some(witness) = self.ex_witness.get(&v).cloned() else {
                return;
            };
            self.scope
                .add_constraint(Constraint::new(Poly::var(v, 1), Cmp::Eq, witness));
        }
        self.prove_ex_constraints(pos);
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
                let declared = self.sig.ret.clone();
                let ex = self.existentials_in(&declared);
                if ex.is_empty() {
                    let found = self.check_expr(e, Some(&declared));
                    self.coerce(&found, &declared, e, "return value");
                } else {
                    self.check_return_witness(e, &declared, &ex);
                }
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
                self.scope.add_yields(&Yields::once());
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
                        self.scope.add_yields(&Yields::Exact(Count::ratio(size, 1)));
                    }
                    Type::Error => self.scope.add_yields(&Yields::Unknown),
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
        _stmt: &Pos<S>,
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
            None => Yields::zero(),
        };
        // a branch that yields less than its sibling makes the total an upper bound, which only
        // an existential size can absorb; `check_sig_satisfied` reports it if there is none
        let joined = true_frame.yields.branch(&false_yields, |a, b| {
            b.sub(a)
                .as_poly()
                .is_some_and(|p| self.t0(Cmp::Ge, p, &Poly::zero()))
        });
        self.scope.add_yields(&joined);

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
                let total = frame.yields.repeated(var, lo, hi);
                self.scope.add_yields(&total);
            }
            // without bounds there is nothing to sum over
            None if !frame.yields.is_zero() => self.scope.add_yields(&Yields::Unknown),
            None => {}
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

    /// A new [`Var`] displayed as `name`, suffixed if that name is already taken -- two sibling
    /// `for i` loops, or two calls to the same existential-returning function, are different
    /// variables and a counterexample naming both has to say so.
    pub fn fresh_var(&mut self, name: &str) -> Var {
        let mut display = name.to_string();
        let mut n = 1;
        while self.var_names.contains(&display) {
            n += 1;
            display = format!("{name}_{n}");
        }
        self.var_names.push(display);
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

/// Read `p` as `k * v` for a single `v` drawn from `candidates` and a non-zero integer `k`.
fn as_scaled_var(p: &Poly, candidates: &[Var]) -> Option<(Coef, Var)> {
    let terms = p.terms();
    let [(coef, mono)] = terms.as_slice() else {
        return None;
    };
    let [(v, 1)] = mono.exps() else {
        return None;
    };
    (candidates.contains(v) && coef.is_positive()).then_some((*coef, *v))
}
