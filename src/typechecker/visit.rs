use std::cmp::Ordering;
use std::rc::Rc;

use super::{BlockScopeEntry, Scopes, annotation};
use super::{T, Type};
use crate::error::{Error, error_at};
use crate::parser::Block;
use crate::parser::lexer::{Op as LexerOp, AssnOp};
use crate::parser::{Expr, Pos, ast::E};
use crate::parser::ast::AD;
use crate::solver::poly::{Poly, poly};

fn parse_place(place: &Expr, ctx: &Scopes) -> Result<Type, Error> {
    match &place.data {
        E::Ident(name) => match ctx.get(name) {
            Some(BlockScopeEntry { t, mutable }) => {
                if *mutable {
                    Ok(t.clone())                    
                } else {
                    Err(error_at!(Type, place, "\"{name}\" is immutable"))
                }
            }
            None => Err(error_at!(NameResolution, place, "cannot find name \"{name}\" in scope",)),
        },

        E::Access(inner, dims) => {
            let inner_t = parse_place(inner, ctx)?;
            match &inner_t.data {
                T::Array { elem, shape } => match shape.len().cmp(&dims.len()) {
                    Ordering::Greater => {
                        let new_shape = shape[0..dims.len()].to_vec();
                        Ok(Type::new_array(*elem.clone(), new_shape, inner))
                    }
                    Ordering::Equal => Ok(*elem.clone()),
                    Ordering::Less => Err(error_at!(
                        Type,
                        inner,
                        "\"{:?}\" has {} dimensions",
                        inner_t.data,
                        dims.len()
                    )),
                },
                _ => Err(error_at!(
                    Type,
                    place,
                    "\"{:?}\" is not an array",
                    inner_t.data
                )),
            }
        }
        _ => Err(error_at!(Type, place, "expression is not a place")),
    }
}

impl<'a> super::FunctionTypeChecker<'a> {
    pub fn unify(
        &mut self,
        lhs: &Type,
        rhs: &Type,
        can_demote_lhs: bool,
        can_demote_rhs: bool,
    ) -> Type {
        match lhs.unify(rhs, can_demote_lhs, can_demote_rhs) {
            Ok(t) => t,
            Err(err) => {
                self.errors.push(err);
                Type::error_at(rhs)
            }
        }
    }

    pub fn visit_expr(&mut self, expr: &Expr) -> Type {
        match &expr.data {
            E::Bool(_) => Type::new_at(T::Bool, expr),
            E::Float(_) => Type::new_at(T::F32, expr),
            E::Int(val) => Type::new_at(T::Size(Rc::new(Poly::constant(*val))), expr),
            E::Ident(ident) => match self.ctx.get(ident) {
                Some(entry) => entry.t.clone(),
                None => {
                    self.errors.push(error_at!(
                        NameResolution,
                        expr,
                        "cannot find name \"{ident}\" in scope",
                    ));
                    Type::error_at(expr)
                },
            },
            E::Tuple(items) => {
                let mut i_types = vec![];
                for i in items {
                    i_types.push(self.visit_expr(i));
                }
                Type::new_at(T::Tuple(i_types), expr)
            }
            E::Array(items) => match items.first() {
                Some(item) => {
                    let mut elem_type = self.visit_expr(item);
                    for i in items.iter().skip(1) {
                        let i_type = self.visit_expr(i);
                        elem_type = self.unify(&elem_type, &i_type, true, true);
                        if elem_type.is_error() {
                            return Type::error_at(&i_type);
                        }
                    }

                    match elem_type.data {
                        T::Array { elem, mut shape } => {
                            shape.push(Rc::new(Poly::constant(items.len() as i128)));
                            Type::new_at(T::Array { elem, shape }, expr)
                        }
                        _ => Type::new_at(
                            T::Array {
                                elem: Box::new(elem_type),
                                shape: vec![Rc::new(Poly::constant(items.len() as i128))],
                            },
                            expr,
                        ),
                    }
                }
                None => Type::new_at(
                    T::Array {
                        elem: Box::new(Type::error_at(expr)),
                        shape: vec![Rc::new(Poly::zero())],
                    },
                    expr,
                ),
            },
            E::UnOp(op, inner) => {
                let inner_t = self.visit_expr(inner);
                match op {
                    crate::parser::UnaryOp::ArithNeg => match inner_t.data {
                        T::F32 => inner_t.but_at(expr),
                        T::I32 => inner_t.but_at(expr),
                        T::Size(p) => Type::size(p.neg(), expr),
                        T::Ind(p) => Type::ind(p.neg(), expr),
                        T::Array { elem, shape } => match &elem.data {
                            T::F32 => Type::new_at(
                                T::Array {
                                    elem: elem.clone(),
                                    shape,
                                },
                                expr,
                            ),
                            T::I32 => Type::new_array(*elem, shape, expr),
                            T::Size(p) => Type::new_array(Type::size(p.neg(), expr), shape, expr),
                            T::Ind(p) => Type::new_array(Type::ind(p.neg(), expr), shape, expr),
                            T::Error => Type::error_at(expr),
                            _ => {
                                self.errors.push(error_at!(
                                    Type,
                                    expr,
                                    "cannot negate array with elements of type \"{:?}\"",
                                    elem.data
                                ));
                                Type::error_at(expr)
                            }
                        },
                        T::Error => Type::error_at(expr),
                        _ => {
                            self.errors.push(error_at!(
                                Type,
                                expr,
                                "cannot negate type \"{:?}\"",
                                inner_t.data
                            ));
                            Type::error_at(expr)
                        }
                    },
                    crate::parser::UnaryOp::BoolNeg => {
                        match inner_t.data {
                            T::Bool => inner_t.but_at(expr),
                            T::Array { elem, shape } => match &elem.data {
                                T::Bool => Type::new_array(*elem, shape, expr),
                                T::Error => Type::error_at(expr),
                                _ => {
                                    self.errors.push(error_at!(Type, expr, "cannot negate type \"{:?}\"", elem.data));
                                    Type::error_at(expr)
                                }
                            },
                            T::Error => Type::error_at(expr),
                            _ => {
                                self.errors.push(error_at!(
                                    Type,
                                    expr,
                                    "cannot negate type \"{:?}\"",
                                    inner_t.data
                                ));
                                Type::error_at(expr)
                            }
                        }
                    }
                    crate::parser::UnaryOp::Tick => {
                        match inner_t.data {
                            T::Array { elem, shape } => {
                                let mut dims = shape.clone();
                                if shape.len() >= 2 {
                                    dims.swap(shape.len() - 1, shape.len() - 2);
                                } else {
                                    // make column vector
                                    dims.push(Rc::new(poly!(1)));
                                }
                                Type::new_array(*elem, dims, expr)
                            }
                            T::Error => Type::error_at(expr),
                            _ => {
                                self.errors.push(error_at!(
                                    Type,
                                    expr,
                                    "cannot transpose type \"{:?}\"",
                                    inner_t.data
                                ));
                                Type::error_at(expr)
                            }
                        }
                    }
                    crate::parser::UnaryOp::Option => {
                        match inner_t.data {
                            T::Option(inner_inner_t) => {
                                // TODO: check return type
                                self.ctx.set_can_return();
                                Type::new_at(inner_inner_t.data.clone(), expr)
                            }
                            T::Error => Type::error_at(expr),
                            _ => {
                                self.errors.push(error_at!(
                                    Type,
                                    expr,
                                    "\"?\" operator expects nullable"
                                ));
                                Type::error_at(expr)
                            }
                        }
                    }
                }
            }
            E::BinOp(lhs, op, rhs) => {
                let lhs_t = self.visit_expr(lhs);
                let rhs_t = self.visit_expr(rhs);

                match op {
                    LexerOp::Arith(arith_op) => match lhs_t.try_arithmetic(arith_op, &rhs_t) {
                        Ok(t) => t.but_at(expr),
                        Err(err) => {
                            self.errors.push(err.but_at(expr));
                            Type::error_at(expr)
                        }
                    },
                    LexerOp::ArithCmp(cmp_op) => match lhs_t.try_compare(cmp_op, &rhs_t) {
                        Ok(t) => t.but_at(expr),
                        Err(err) => {
                            self.errors.push(err.but_at(expr));
                            Type::error_at(expr)
                        }
                    },
                    LexerOp::Bool(op) => match lhs_t.try_bool_op(op, &rhs_t) {
                        Ok(t) => t.but_at(expr),
                        Err(err) => {
                            self.errors.push(err.but_at(expr));
                            Type::error_at(expr)
                        }
                    },
                    LexerOp::Dot => todo!(),
                    LexerOp::Tick
                    | LexerOp::Apply
                    | LexerOp::Option
                    | LexerOp::Comma
                    | LexerOp::Not => unreachable!(),
                }
            }
            E::Access(inner, access_dims) => {
                let inner_t = self.visit_expr(inner);
                let (elem, old_shape) = match inner_t.data {
                    T::Array { elem, shape } => (elem, shape),
                    T::Error => return Type::error_at(expr),
                    not_arr => {
                        self.errors.push(error_at!(
                            Type,
                            expr,
                            "cannot index type \"{:?}\"",
                            not_arr
                        ));
                        return Type::error_at(expr);
                    }
                };

                let mut new_shape = vec![];
                let mut reduce_inds = 0;
                for (i, dim) in access_dims.iter().enumerate() {
                    match old_shape.get(i) {
                        Some(expected_dim) => {
                            match &dim.data {
                                AD::Range(None, None) => {
                                    reduce_inds += 1;
                                    new_shape.push(expected_dim.clone());
                                }
                                AD::Range(_, _) => {
                                    self.errors.push(error_at!(
                                        NotImplmented,
                                        expr,
                                        "range access not implemented yet",
                                    ));
                                    return Type::error_at(expr);
                                }
                                AD::Point(point) => {
                                    let point_t = self.visit_expr(point);
                                    match point_t.data {
                                        T::Ind(index) => {
                                            reduce_inds += 1;
                                            if index == *expected_dim {
                                                reduce_inds += 1;
                                                new_shape.push(Rc::new(poly!(1)));
                                            } else {
                                                self.errors.push(error_at!(
                                                    Type,
                                                    expr,
                                                    "\"{:?}\" cannot access dim of size \"{:?}\"",
                                                    index,
                                                    expected_dim
                                                ));
                                                return Type::error_at(expr);
                                            }
                                        }
                                        T::Error => return Type::error_at(expr),
                                        _ => {
                                            self.errors.push(error_at!(
                                                Type,
                                                expr,
                                                "\"{:?}\" cannot index array",
                                                point_t.data
                                            ));
                                            return Type::error_at(expr);
                                        }
                                    }
                                }
                            }
                        }
                        None => {
                            self.errors.push(error_at!(
                                Type,
                                expr,
                                "array has \"{}\" dims",
                                old_shape.len()
                            ));
                            return Type::error_at(expr);
                        }
                    }
                }

                new_shape.extend(old_shape.into_iter().skip(reduce_inds));

                while let Some(p) = new_shape.pop() {
                    if *p != poly!(1) {
                        new_shape.push(p);
                        break;
                    }
                }

                if new_shape.is_empty() {
                    *elem
                } else {
                    Type::new_array(*elem, new_shape, expr)
                }
            }
            E::FnCall(_, _, _) => todo!()
        }
    }

    pub fn visit_print(&mut self, expr: &Expr) {
        self.visit_expr(expr);
    }

    pub fn visit_return(&mut self, expr: &Expr) {
        self.ctx.set_always_returns();
        self.visit_expr(expr);
    }

    pub fn visit_yield(&mut self, expr: &Expr) {
        self.ctx.add_yields(&poly!(1));
        self.visit_expr(expr);
    }

    pub fn visit_yield_from(&mut self, expr: &Expr) {
        self.visit_expr(expr);
    }

    pub fn visit_let(&mut self, mutable: bool, name: Pos<&str>, ann: Option<&Expr>, value: &Expr) {
        let ann = if let Some(ann) = ann {
            match annotation(ann, &self.p_vars) {
                Ok(ann) => Some(ann),
                Err(err) => {
                    self.ctx.insert(name.data, Type::error_at(ann), mutable);
                    self.errors.push(err);
                    None
                }
            }
        } else {
            None
        };

        if let Some(ann) = ann {
            self.ctx.insert(name.data, ann.clone(), mutable);
        } else {
            self.ctx.insert(name.data, Type::error_at(&name), mutable);
        }

        self.visit_expr(value);
    }

    pub fn visit_assign(&mut self, place: &Expr, _op: &AssnOp, value: &Expr) {
        let _expecting = match parse_place(place, self.ctx) {
            Ok(t) => t,
            Err(err) => {
                self.errors.push(err);
                Type::error_at(place)
            }
        };

        self.visit_expr(value);
    }

    pub fn visit_if(&mut self, cond: &Expr, true_body: &Block, false_body: &Option<Block>) {
        self.ctx.push();
        self.visit_expr(cond);
        self.ctx.push();
        self.traverse_block(true_body);
        let true_block = self.ctx.pop().unwrap();
        let true_block_has_return = true_block.can_return;
        let true_block_always_return = true_block.always_returns;

        let mut false_block_has_return = false;
        let mut false_block_always_return = false;

        if let Some(false_body) = false_body {
            self.ctx.push();
            self.traverse_block(false_body);

            let false_block = self.ctx.pop().unwrap();
            false_block_has_return = false_block.can_return;
            false_block_always_return = false_block.always_returns;
        }

        self.ctx.pop();
        if true_block_has_return || false_block_has_return {
            self.ctx.set_can_return();
        }
        if true_block_always_return && false_block_always_return {
            self.ctx.set_always_returns()
        }
    }

    pub fn visit_for(&mut self, _iter: Pos<&str>, lower: &Expr, upper: &Expr, body: &Block) {
        self.ctx.push();
        self.visit_expr(lower);
        self.visit_expr(upper);
        self.ctx.push();
        self.traverse_block(body);

        let inner = self.ctx.pop().unwrap();
        self.ctx.pop();

        if inner.can_return {
            self.ctx.set_can_return();
        }
        if inner.always_returns {
            self.ctx.set_always_returns();
        }
    }
}
