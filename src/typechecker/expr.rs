//! Expression checking, coercion, and call-site typevar inference.

use std::collections::HashMap;

use crate::error::error_at;
use crate::parser::ast::{AD, E, Expr};
use crate::parser::lexer::{ArithCmpOp, ArithOp, BoolOp, Op};
use crate::parser::{Pos, UnaryOp};
use crate::solver::poly::{Poly, Var};
use crate::solver::z3::{Cmp, Constraint};

use super::check::FnChecker;
use super::sig::{FnSig, match_ty};
use super::ty::{MAX_POW, Type};

impl FnChecker<'_> {
    /// Type of `expr`. `expected` is a hint pushed inward, never a promise: it decides literal
    /// types and call-site typevars, but the caller still has to [`coerce`](Self::coerce).
    pub fn check_expr(&mut self, expr: &Expr, expected: Option<&Type>) -> Type {
        let t = self.infer(expr, expected);
        self.exprs.insert((expr.start, expr.end), t.clone());
        t
    }

    fn infer(&mut self, expr: &Expr, expected: Option<&Type>) -> Type {
        match &expr.data {
            E::Bool(_) => Type::Bool,

            E::Float(_) => match expected {
                Some(Type::F64) => Type::F64,
                _ => Type::F32,
            },

            E::Int(val) => match expected {
                Some(Type::F32) => Type::F32,
                Some(Type::F64) => Type::F64,
                _ => Type::Size(Poly::constant(*val)),
            },
            E::Ident(name) => {
                if name == "null" {
                    Type::Null
                } else {
                    match self.scope.get(name) {
                        Some(binding) => {
                            if self.scope.not_nulls().contains(&binding.id) {
                                if let Type::Option(t) = &binding.ty {
                                    *t.clone()
                                } else {
                                    binding.ty.clone()
                                }
                            } else if self.scope.is_nulls().contains(&binding.id) {
                                if let Type::Option(_) = &binding.ty {
                                    Type::Null
                                } else {
                                    binding.ty.clone()
                                }
                            } else {
                                binding.ty.clone()
                            }
                        }
                        None if self.sigs.contains_key(name) => {
                            self.errors.push(error_at!(
                                Type,
                                expr,
                                "\"{name}\" is a function; call it with \"{name}(...)\""
                            ));
                            Type::Error
                        }
                        None => {
                            self.errors.push(error_at!(
                                NameResolution,
                                expr,
                                "cannot find name \"{name}\" in scope"
                            ));
                            Type::Error
                        }
                    }
                }
            }

            E::Tuple(items) if items.is_empty() => Type::Unit,

            E::Tuple(items) => {
                let hints = match expected {
                    Some(Type::Tuple(ts)) if ts.len() == items.len() => Some(ts),
                    _ => None,
                };
                let types = items
                    .iter()
                    .enumerate()
                    .map(|(i, item)| self.check_expr(item, hints.map(|h| &h[i])))
                    .collect();
                Type::Tuple(types)
            }

            E::Array(items) => self.infer_array(items, expected),

            E::UnOp(op, inner) => {
                let inner_t = self.check_expr(inner, expected);
                match op {
                    UnaryOp::ArithNeg => self.negate(&inner_t, expr),
                    UnaryOp::BoolNeg => self.bool_negate(&inner_t, expr),
                    UnaryOp::Tick => self.transpose(&inner_t, expr),
                    UnaryOp::Option => {
                        if !inner_t.is_error() {
                            self.errors.push(error_at!(
                                NotImplmented,
                                expr,
                                "\"?\" is not implemented yet"
                            ));
                        }
                        Type::Error
                    }
                }
            }

            E::BinOp(lhs, Op::Dot, rhs) => self.infer_field(lhs, rhs),

            E::BinOp(lhs, op, rhs) => {
                let lhs_t = self.check_expr(lhs, None);
                let rhs_t = self.check_expr(rhs, None);
                match op {
                    Op::Arith(op) => self.arith(&lhs_t, op, &rhs_t, expr),
                    Op::ArithCmp(op) => self.compare(&lhs_t, op, &rhs_t, expr),
                    Op::Bool(op) => self.bool_op(&lhs_t, op, &rhs_t, expr),
                    Op::Apply => {
                        self.errors.push(error_at!(
                            NotImplmented,
                            expr,
                            "\"->\" is not implemented yet"
                        ));
                        Type::Error
                    }
                    other => {
                        self.errors
                            .push(error_at!(Type, expr, "invalid operator \"{other:?}\""));
                        Type::Error
                    }
                }
            }

            E::Access(arr, dims) => self.infer_access(expr, arr, dims),

            E::FnCall(caller, type_args, args) => {
                let name = match &caller.data {
                    E::Ident(name) => name,
                    _ => {
                        self.errors
                            .push(error_at!(Type, caller, "expression is not a function"));
                        self.check_rest(type_args.iter().chain(args));
                        return Type::Error;
                    }
                };

                match self.sigs.get(name) {
                    Some(sig) => {
                        let sig: FnSig = sig.clone();
                        self.infer_call(expr, &sig, type_args, args, expected)
                    }
                    None => {
                        if self.scope.get(name).is_some() {
                            self.errors.push(error_at!(
                                Type,
                                caller,
                                "\"{name}\" is not a function"
                            ));
                        } else {
                            self.errors.push(error_at!(
                                NameResolution,
                                caller,
                                "cannot find function \"{name}\" in scope"
                            ));
                        }
                        self.check_rest(type_args.iter().chain(args));
                        Type::Error
                    }
                }
            }
        }
    }

    fn check_rest<'e>(&mut self, exprs: impl Iterator<Item = &'e Expr>) {
        for e in exprs {
            self.check_expr(e, None);
        }
    }

    // -- literals ------------------------------------------------------------

    fn infer_array(&mut self, items: &[Expr], expected: Option<&Type>) -> Type {
        // an expected `f32[2, 3]` makes each row an expected `f32[3]`
        let hint = match expected {
            Some(Type::Array { elem, shape }) if !shape.is_empty() => Some(match shape.len() {
                1 => (**elem).clone(),
                _ => Type::new_arr((**elem).clone(), shape[1..].to_vec()),
            }),
            _ => None,
        };

        let mut elem_t: Option<Type> = None;
        for item in items {
            let item_t = self.check_expr(item, hint.as_ref());
            elem_t = Some(match elem_t {
                None => item_t,
                Some(prev) => match self.join(&prev, &item_t) {
                    Some(joined) => joined,
                    None => {
                        let names = self.names();
                        self.errors.push(error_at!(
                            Type,
                            item,
                            "array elements have types \"{}\" and \"{}\"",
                            prev.render(&names),
                            item_t.render(&names)
                        ));
                        Type::Error
                    }
                },
            });
        }

        let len = Poly::constant(items.len() as i64);
        match elem_t {
            None => Type::new_arr(hint.unwrap_or(Type::Error), vec![len]),
            // the outer dimension is the outermost one: [[1, 2, 3], [4, 5, 6]] => f32[2, 3]
            Some(Type::Array { elem, shape }) => {
                let mut dims = vec![len];
                dims.extend(shape);
                Type::new_arr(*elem, dims)
            }
            Some(t) => Type::new_arr(t, vec![len]),
        }
    }

    // -- operators -----------------------------------------------------------

    fn negate(&mut self, t: &Type, pos: &Expr) -> Type {
        match t {
            Type::F32 | Type::F64 | Type::I64 => t.clone(),
            Type::Size(p) => Type::Size(p.neg()),
            Type::Ind(_) => Type::I64,
            Type::Error => Type::Error,
            Type::Array { elem, shape } => match self.negate(elem, pos) {
                Type::Error => Type::Error,
                elem => Type::new_arr(elem, shape.clone()),
            },
            other => {
                let names = self.names();
                self.errors.push(error_at!(
                    Type,
                    pos,
                    "cannot negate \"{}\"",
                    other.render(&names)
                ));
                Type::Error
            }
        }
    }

    fn bool_negate(&mut self, t: &Type, pos: &Expr) -> Type {
        match t {
            Type::Bool | Type::Error => t.clone(),
            Type::Array { elem, shape } if matches!(**elem, Type::Bool) => {
                Type::new_arr(Type::Bool, shape.clone())
            }
            other => {
                let names = self.names();
                self.errors.push(error_at!(
                    Type,
                    pos,
                    "cannot negate \"{}\"; expected \"bool\"",
                    other.render(&names)
                ));
                Type::Error
            }
        }
    }

    fn transpose(&mut self, t: &Type, pos: &Expr) -> Type {
        match t {
            Type::Array { elem, shape } => {
                let mut shape = shape.clone();
                match shape.len() {
                    // a vector transposes into a column
                    0 | 1 => shape.push(Poly::constant(1)),
                    n => shape.swap(n - 1, n - 2),
                }
                Type::new_arr((**elem).clone(), shape)
            }
            Type::Error => Type::Error,
            other => {
                let names = self.names();
                self.errors.push(error_at!(
                    Type,
                    pos,
                    "cannot transpose \"{}\"",
                    other.render(&names)
                ));
                Type::Error
            }
        }
    }

    pub fn arith(&mut self, lhs: &Type, op: &ArithOp, rhs: &Type, pos: &Pos<impl Sized>) -> Type {
        match (lhs, rhs) {
            (Type::Error, _) | (_, Type::Error) => Type::Error,
            (Type::F32, Type::F32) => Type::F32,
            (Type::F64, Type::F64) => Type::F64,
            (
                Type::Array {
                    elem: lhs_elem,
                    shape: lhs_shape,
                },
                Type::Array {
                    elem: rhs_elem,
                    shape: rhs_shape,
                },
            ) if lhs_shape.len() == rhs_shape.len() => {
                if !self.same_shape(lhs_shape, rhs_shape, pos) {
                    return Type::Error;
                }
                match self.arith(lhs_elem, op, rhs_elem, pos) {
                    Type::Error => Type::Error,
                    elem => Type::new_arr(elem, lhs_shape.clone()),
                }
            }
            _ if lhs.is_int_like() && rhs.is_int_like() => self.int_arith(lhs, op, rhs, pos),
            (Type::Array { .. }, _) | (_, Type::Array { .. }) => {
                self.errors.push(error_at!(
                    NotImplmented,
                    pos,
                    "broadcasting is not implemented yet"
                ));
                Type::Error
            }
            _ => {
                let names = self.names();
                self.errors.push(error_at!(
                    Type,
                    pos,
                    "cannot perform arithmetic between \"{}\" and \"{}\"",
                    lhs.render(&names),
                    rhs.render(&names)
                ));
                Type::Error
            }
        }
    }

    fn int_arith(&mut self, lhs: &Type, op: &ArithOp, rhs: &Type, pos: &Pos<impl Sized>) -> Type {
        if let (Some(l), Some(r)) = (lhs.as_size(), rhs.as_size()) {
            return match op {
                ArithOp::Add => Type::Size(l.add(r)),
                ArithOp::Sub => Type::Size(l.sub(r)),
                ArithOp::Mul => Type::Size(l.mul(r)),
                ArithOp::Div => {
                    let nonzero = self.prove(
                        Constraint::new(r.clone(), Cmp::Ne, Poly::zero()),
                        pos,
                        "divisor",
                    );
                    match (nonzero, l.try_divide(r)) {
                        (false, _) => Type::Error,
                        // an inexact quotient loses its static value to truncation
                        (true, Some(quotient)) => Type::Size(quotient),
                        (true, None) => Type::I64,
                    }
                }
                ArithOp::Exp => match r.as_constant() {
                    Some(power) if power.get() > MAX_POW => {
                        self.errors
                            .push(error_at!(Type, pos, "power must be at most {MAX_POW}"));
                        Type::Error
                    }
                    Some(power) if power.is_positive() || power.is_zero() => {
                        Type::Size(l.pow(power.get() as u32))
                    }
                    Some(_) => {
                        self.errors
                            .push(error_at!(Type, pos, "power must be non-negative"));
                        Type::Error
                    }
                    None => Type::I64,
                },
            };
        }

        // an index shifted up by a non-negative amount keeps a bound
        match (lhs, op, rhs) {
            (Type::Ind(bound), ArithOp::Add, Type::Size(shift))
            | (Type::Size(shift), ArithOp::Add, Type::Ind(bound))
                if self.t0(Cmp::Ge, shift, &Poly::zero()) =>
            {
                Type::Ind(bound.add(shift))
            }
            _ => Type::I64,
        }
    }

    fn compare(&mut self, lhs: &Type, op: &ArithCmpOp, rhs: &Type, pos: &Expr) -> Type {
        match (lhs, rhs) {
            (Type::Error, _) | (_, Type::Error) => Type::Error,
            (Type::F32, Type::F32) | (Type::F64, Type::F64) => Type::Bool,
            (Type::Bool, Type::Bool) if op.is_eq_or_neq() => Type::Bool,
            (Type::Option(_), Type::Null) | (Type::Null, Type::Option(_)) if op.is_eq_or_neq() => {
                Type::Bool
            }
            (
                Type::Array {
                    elem: lhs_elem,
                    shape: lhs_shape,
                },
                Type::Array {
                    elem: rhs_elem,
                    shape: rhs_shape,
                },
            ) if lhs_shape.len() == rhs_shape.len() => {
                if !self.same_shape(lhs_shape, rhs_shape, pos) {
                    return Type::Error;
                }
                match self.compare(lhs_elem, op, rhs_elem, pos) {
                    Type::Error => Type::Error,
                    elem => Type::new_arr(elem, lhs_shape.clone()),
                }
            }
            _ if lhs.is_int_like() && rhs.is_int_like() => Type::Bool,
            (Type::Array { .. }, _) | (_, Type::Array { .. }) => {
                self.errors.push(error_at!(
                    NotImplmented,
                    pos,
                    "broadcasting is not implemented yet"
                ));
                Type::Error
            }
            _ => {
                let names = self.names();
                self.errors.push(error_at!(
                    Type,
                    pos,
                    "cannot compare \"{}\" with \"{}\"",
                    lhs.render(&names),
                    rhs.render(&names)
                ));
                Type::Error
            }
        }
    }

    fn bool_op(&mut self, lhs: &Type, _op: &BoolOp, rhs: &Type, pos: &Expr) -> Type {
        match (lhs, rhs) {
            (Type::Error, _) | (_, Type::Error) => Type::Error,
            (Type::Bool, Type::Bool) => Type::Bool,
            _ => {
                let names = self.names();
                self.errors.push(error_at!(
                    Type,
                    pos,
                    "cannot combine \"{}\" and \"{}\"; expected \"bool\"",
                    lhs.render(&names),
                    rhs.render(&names)
                ));
                Type::Error
            }
        }
    }

    fn same_shape(&mut self, lhs: &[Poly], rhs: &[Poly], pos: &Pos<impl Sized>) -> bool {
        let mut ok = true;
        for (l, r) in lhs.iter().zip(rhs) {
            ok &= self.prove(
                Constraint::new(l.clone(), Cmp::Eq, r.clone()),
                pos,
                "elementwise operands",
            );
        }
        ok
    }

    // -- projection and indexing ---------------------------------------------

    fn infer_field(&mut self, lhs: &Expr, field: &Expr) -> Type {
        let lhs_t = self.check_expr(lhs, None);
        match (&lhs_t, &field.data) {
            (Type::Error, _) => Type::Error,
            (Type::Tuple(items), E::Int(i)) => match items.get(*i as usize) {
                Some(t) => t.clone(),
                None => {
                    self.errors
                        .push(error_at!(Type, field, "tuple has {} fields", items.len()));
                    Type::Error
                }
            },
            (Type::Array { shape, .. }, E::Ident(name)) if name == "len" => {
                Type::Size(shape.first().cloned().unwrap_or_else(Poly::zero))
            }
            (Type::Array { shape, .. }, E::Ident(name)) if name == "shape" => {
                Type::Tuple(shape.iter().map(|d| Type::Size(d.clone())).collect())
            }
            _ => {
                let names = self.names();
                self.errors.push(error_at!(
                    Type,
                    field,
                    "\"{}\" has no such field",
                    lhs_t.render(&names)
                ));
                Type::Error
            }
        }
    }

    fn infer_access(&mut self, expr: &Expr, arr: &Expr, dims: &[Pos<AD>]) -> Type {
        let arr_t = self.check_expr(arr, None);
        let (elem, shape) = match &arr_t {
            Type::Array { elem, shape } => ((**elem).clone(), shape.clone()),
            Type::Error => return Type::Error,
            other => {
                let names = self.names();
                self.errors.push(error_at!(
                    Type,
                    arr,
                    "cannot index \"{}\"",
                    other.render(&names)
                ));
                return Type::Error;
            }
        };

        if dims.len() > shape.len() {
            let names = self.names();
            self.errors.push(error_at!(
                Type,
                expr,
                "\"{}\" has {} dimensions, found {} indices",
                arr_t.render(&names),
                shape.len(),
                dims.len()
            ));
            return Type::Error;
        }

        for (dim, bound) in dims.iter().zip(&shape) {
            match &dim.data {
                AD::Range(_, _) => {
                    self.errors.push(error_at!(
                        NotImplmented,
                        dim,
                        "range access is not implemented yet"
                    ));
                    return Type::Error;
                }
                AD::Point(point) => {
                    let point_t = self.check_expr(point, None);
                    self.check_index(&point_t, bound, point);
                }
            }
        }

        let rest = shape[dims.len()..].to_vec();
        if rest.is_empty() {
            elem
        } else {
            Type::new_arr(elem, rest)
        }
    }

    /// `Size(p)` needs `0 <= p < bound`; `Ind(p)` already carries its lower bound, so `p <= bound`.
    fn check_index(&mut self, index: &Type, bound: &Poly, pos: &Expr) {
        match index {
            Type::Size(p) => {
                if self.prove(
                    Constraint::new(p.clone(), Cmp::Ge, Poly::zero()),
                    pos,
                    "index",
                ) {
                    self.prove(
                        Constraint::new(p.clone(), Cmp::Lt, bound.clone()),
                        pos,
                        "index",
                    );
                }
            }
            Type::Ind(p) => {
                self.prove(
                    Constraint::new(p.clone(), Cmp::Le, bound.clone()),
                    pos,
                    "index",
                );
            }
            Type::Error => {}
            other => {
                let names = self.names();
                self.errors.push(error_at!(
                    Type,
                    pos,
                    "cannot index with \"{}\"; an index must be statically bounded",
                    other.render(&names)
                ));
            }
        }
    }

    // -- calls ---------------------------------------------------------------

    /// Solve the callee's typevars from explicit arguments, then the argument types, then the
    /// expected type; instantiate the signature and check the call against it.
    fn infer_call(
        &mut self,
        expr: &Expr,
        sig: &FnSig,
        type_args: &[Expr],
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Type {
        let mut subst: HashMap<Var, Poly> = HashMap::new();
        let mut failed = false;

        if type_args.len() > sig.tv_names.len() {
            self.errors.push(error_at!(
                Type,
                expr,
                "\"{}\" takes {} typevars, found {}",
                sig.name,
                sig.tv_names.len(),
                type_args.len()
            ));
            failed = true;
        }

        for (i, arg) in type_args.iter().enumerate() {
            let arg_t = self.check_expr(arg, None);
            match arg_t {
                Type::Size(p) if i < sig.tv_names.len() => {
                    subst.insert(i as Var, p);
                }
                Type::Error => failed = true,
                other if i < sig.tv_names.len() => {
                    let names = self.names();
                    self.errors.push(error_at!(
                        Type,
                        arg,
                        "typevar must be statically known, found \"{}\"",
                        other.render(&names)
                    ));
                    failed = true;
                }
                _ => {}
            }
        }

        if args.len() != sig.params.len() {
            self.errors.push(error_at!(
                Type,
                expr,
                "\"{}\" takes {} arguments, found {}",
                sig.name,
                sig.params.len(),
                args.len()
            ));
            self.check_rest(args.iter());
            return Type::Error;
        }

        // left to right
        // an early argument can pin down a later one's typevars
        let mut arg_types = vec![];
        for (arg, (_, param)) in args.iter().zip(&sig.params) {
            let hint = param.instantiate(&subst).ok();
            let arg_t = self.check_expr(arg, hint.as_ref());
            match_ty(param, &arg_t, &mut subst);
            arg_types.push(arg_t);
        }

        // a second pass picks up typevars that only became solvable now
        for (arg_t, (_, param)) in arg_types.iter().zip(&sig.params) {
            match_ty(param, arg_t, &mut subst);
        }
        if let Some(expected) = expected {
            match_ty(&sig.ret, expected, &mut subst);
        }

        if failed {
            return Type::Error;
        }

        let missing: Vec<&str> = (0..sig.tv_names.len())
            .filter(|v| !subst.contains_key(&(*v as Var)))
            .map(|v| sig.tv_names[v].as_str())
            .collect();
        if !missing.is_empty() {
            self.errors.push(error_at!(
                Type,
                expr,
                "cannot infer typevar \"{}\" of \"{}\"; pass it with \"{}{{...}}\"",
                missing.join("\", \""),
                sig.name,
                sig.name
            ));
            return Type::Error;
        }

        // the body assumes its typevars are sizes, so the call site owes nonnegativity
        for (v, name) in sig.tv_names.iter().enumerate() {
            let value = subst[&(v as Var)].clone();
            let ctx = format!("typevar \"{}\" of \"{}\"", name, sig.name);
            self.prove(Constraint::new(value, Cmp::Ge, Poly::zero()), expr, &ctx);
        }

        for (arg_t, (arg, (name, param))) in arg_types.iter().zip(args.iter().zip(&sig.params)) {
            let want = param.instantiate(&subst).unwrap_or(Type::Error);
            let ctx = format!("argument \"{}\" of \"{}\"", name, sig.name);
            self.coerce(arg_t, &want, arg, &ctx);
        }

        for constraint in &sig.constraints {
            let goal = Constraint::new(
                constraint.lhs.map_vars(|v| subst[&v].clone()),
                constraint.cmp,
                constraint.rhs.map_vars(|v| subst[&v].clone()),
            );
            let ctx = format!("constraint of \"{}\"", sig.name);
            self.prove(goal, expr, &ctx);
        }

        sig.ret.instantiate(&subst).unwrap_or(Type::Error)
    }
}
