use crate::error::{Error, error_at};
use crate::parser::lexer::{ArithOp, ArithCmpOp, BoolOp};
use crate::{parser::Pos, solver::poly::Poly};

use std::rc::Rc;
#[derive(Clone, PartialEq, Debug)]
pub struct FunctionType {
    pub poly_args: Vec<Pos<String>>,
    pub args: Vec<Type>,
    pub ret: Type,
}

#[derive(Clone, PartialEq, Debug)]
pub enum T {
    Void,
    F32,
    I32,
    Bool,
    Null,
    Option(Box<Type>),
    Tuple(Vec<Type>),
    Size(Rc<Poly>),
    Ind(Rc<Poly>),
    Function(Box<FunctionType>),
    Array {
        elem: Box<Type>,
        shape: Vec<Rc<Poly>>,
    },
    Error,
}

pub type Type = Pos<T>;

impl Type {
    pub fn new_fn(
        poly_args: Vec<Pos<String>>,
        args: Vec<Type>,
        ret: Type,
        name: &Pos<String>,
    ) -> Type {
        Type::new_at(
            T::Function(Box::new(FunctionType {
                poly_args,
                args,
                ret,
            })),
            name,
        )
    }

    pub fn new_array<E>(elem: Self, shape: Vec<Rc<Poly>>, pos: &Pos<E>) -> Self {
        Self::new_at(
            T::Array {
                elem: Box::new(elem),
                shape,
            },
            pos,
        )
    }

    pub fn size<E>(size: Poly, pos: &Pos<E>) -> Self {
        Self::new_at(T::Size(Rc::new(size)), pos)
    }

    pub fn ind<E>(size: Poly, pos: &Pos<E>) -> Self {
        Self::new_at(T::Ind(Rc::new(size)), pos)
    }

    pub fn ind_rc<E>(size: Rc<Poly>, pos: &Pos<E>) -> Self {
        Self::new_at(T::Ind(size), pos)
    }

    pub fn try_arithmetic(&self, op: &ArithOp, other: &Self) -> Result<Self, Error> {
        match (&self.data, op, &other.data) {
            (T::F32, _, T::F32) => Ok(Self::new_at(T::F32, other)),
            (T::I32, _, T::I32) => Ok(Self::new_at(T::I32, other)),
            (T::Size(_), _, T::I32) => Ok(Self::new_at(T::I32, other)),
            (T::I32, _, T::Size(_)) => Ok(Self::new_at(T::I32, other)),
            (T::Size(a), ArithOp::Exp, T::Size(b)) => match b.as_constant() {
                Some(b_const) => {
                    if b_const.is_zero() {
                        Ok(Self::size(Poly::constant(1), other))
                    } else if !b_const.is_positive() {
                        Err(error_at!(Type, other, "power must be non-negative"))
                    } else {
                        Ok(Self::size(a.pow(b_const.get() as u32), other))
                    }
                }
                None => Ok(Self::new_at(T::I32, other)),
            },
            (T::Size(a), ArithOp::Mul, T::Size(b)) => Ok(Self::size(a.mul(b), other)),
            (T::Size(a), ArithOp::Div, T::Size(b)) => match a.try_divide(b) {
                Some(res) => Ok(Type::size(res, other)),
                None => {
                    if b.is_zero() {
                        Err(error_at!(
                            Type,
                            other,
                            "detected guaranteed divison by zero"
                        ))
                    } else {
                        Ok(Type::new_at(T::I32, other))
                    }
                }
            },
            (T::Size(a), ArithOp::Add, T::Size(b)) => Ok(Self::size(a.add(b), other)),
            (T::Size(a), ArithOp::Sub, T::Size(b)) => Ok(Self::size(a.sub(b), other)),
            (T::Size(a), ArithOp::Add, T::Ind(b)) => Ok(Self::ind(a.add(b), other)),
            (T::Size(a), ArithOp::Sub, T::Ind(b)) => Ok(Self::ind(a.sub(b), other)),
            (T::Ind(a), ArithOp::Add, T::Size(b)) => Ok(Self::ind(a.add(b), other)),
            (T::Ind(a), ArithOp::Sub, T::Size(b)) => Ok(Self::ind(a.sub(b), other)),
            (T::Ind(_), _, T::Ind(_)) => Ok(Self::new_at(T::I32, other)),
            (
                T::Array {
                    elem: a_elem,
                    shape: a_shape,
                },
                op,
                T::Array {
                    elem: b_elem,
                    shape: b_shape,
                },
            ) => {
                if a_shape == b_shape {
                    let elementwise = a_elem.try_arithmetic(op, b_elem)?;
                    Ok(Type::new_array(elementwise, a_shape.clone(), other))
                } else {
                    Err(error_at!(
                        Type,
                        other,
                        "array shapes cannot match; found \"{:?}\" and \"{:?}\"",
                        a_shape,
                        b_shape
                    ))
                }
            }
            (T::Error, _, _) | (_, _, T::Error) => Ok(Self::error_at(other)),
            _ => Err(error_at!(
                Type,
                other,
                "cannot perform arithmetic between \"{:?}\" and \"{:?}\"",
                self.data,
                other.data
            )),
        }
    }

    pub fn try_compare(&self, op: &ArithCmpOp, other: &Self) -> Result<Self, Error> {
        match (&self.data, op, &other.data) {
            (T::F32, _, T::F32) => Ok(Self::new_at(T::Bool, other)),
            (T::I32, _, T::I32) => Ok(Self::new_at(T::Bool, other)),
            (T::I32, _, T::Size(_)) | (T::Size(_), _, T::I32) => Ok(Self::new_at(T::Bool, other)),
            (T::I32, _, T::Ind(_)) | (T::Ind(_), _, T::I32) => Ok(Self::new_at(T::Bool, other)),
            (T::Size(_) | T::Ind(_), _, T::Size(_) | T::Ind(_)) => Ok(Self::new_at(T::Bool, other)),
            (T::Bool, _, T::Bool) => match op {
                ArithCmpOp::Eq | ArithCmpOp::NotEq => Ok(Self::new_at(T::Bool, other)),
                _ => Err(error_at!(
                    Type,
                    other,
                    "cannot perform comparison between \"{:?}\" and \"{:?}\"",
                    self.data,
                    other.data
                ))
            }
            (
                T::Array {
                    elem: a_elem,
                    shape: a_shape,
                },
                op,
                T::Array {
                    elem: b_elem,
                    shape: b_shape,
                },
            ) => {
                if a_shape == b_shape {
                    let elementwise = a_elem.try_compare(op, b_elem)?;
                    Ok(Type::new_array(elementwise, a_shape.clone(), other))
                } else {
                    Err(error_at!(
                        Type,
                        other,
                        "array shapes cannot match; found \"{:?}\" and \"{:?}\"",
                        a_shape,
                        b_shape
                    ))
                }
            }
            (T::Error, _, _) | (_, _, T::Error) => Ok(Self::error_at(other)),
            _ => Err(error_at!(
                Type,
                other,
                "cannot perform comparison between \"{:?}\" and \"{:?}\"",
                self.data,
                other.data
            )),
        }
    }

    pub fn try_bool_op(&self, op: &BoolOp, other: &Self) -> Result<Self, Error> {
        match (&self.data, op, &other.data) {
            (T::Bool, _, T::Bool) => Ok(Self::new_at(T::Bool, other)),
            (T::Error, _, _) | (_, _, T::Error) => Ok(Self::error_at(other)),
            (
                T::Array {
                    elem: a_elem,
                    shape: a_shape,
                },
                op,
                T::Array {
                    elem: b_elem,
                    shape: b_shape,
                },
            ) => {
                if a_shape == b_shape {
                    let elementwise = a_elem.try_bool_op(op, b_elem)?;
                    Ok(Type::new_array(elementwise, a_shape.clone(), other))
                } else {
                    Err(error_at!(
                        Type,
                        other,
                        "array shapes cannot match; found \"{:?}\" and \"{:?}\"",
                        a_shape,
                        b_shape
                    ))
                }
            }
            _ => Err(error_at!(
                Type,
                other,
                "cannot perform comparison between \"{:?}\" and \"{:?}\"",
                self.data,
                other.data
            )),
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self.data, T::Error)
    }

    pub fn error_at<E>(pos: &Pos<E>) -> Self {
        Self::new_at(T::Error, pos)
    }

    pub fn unify(
        &self,
        other: &Type,
        can_demote_lhs: bool,
        can_demote_rhs: bool,
    ) -> Result<Type, Error> {
        match (&self.data, &other.data, can_demote_lhs, can_demote_rhs) {
            (ad, bd, _, _) if ad == bd => Ok(other.clone()),
            (T::Size(_) | T::Ind(_), T::I32, true, _) => Ok(other.clone()),
            (T::I32, T::Size(_) | T::Ind(_), _, true) => Ok(self.but_at(other)),
            (T::Error, _, _, _) | (_, T::Error, _, _) => Ok(Self::error_at(other)),
            _ => Err(error_at!(
                Type,
                other,
                "cannot unify \"{:?}\" with \"{:?}\"",
                self.data,
                other.data
            )),
        }
    }
}
