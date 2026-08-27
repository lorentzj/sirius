use crate::{parser::Pos, solver::poly::Poly};
use std::rc::Rc;
#[derive(Clone)]
pub struct FunctionType {
    pub poly_args: Vec<Pos<String>>,
    pub args: Vec<Type>,
    pub ret: Type,
}

#[derive(Clone)]
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
    pub fn new_fn<E>(
        poly_args: Vec<Pos<String>>,
        args: Vec<Type>,
        ret: Type,
        name: &Pos<E>,
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

    pub fn size<E>(inner: Poly, expr: &Pos<E>) -> Self {
        Self::new_at(T::Size(Rc::new(inner)), expr)
    }

    pub fn ind<E>(inner: Rc<Poly>, expr: &Pos<E>) -> Self {
        Self::new_at(T::Ind(inner), expr)
    }

    pub fn ind_rc<E>(inner: Poly, expr: &Pos<E>) -> Self {
        Self::new_at(T::Ind(Rc::new(inner)), expr)
    }

    pub fn as_int(t: Type) -> Option<Type> {
        match &t.data {
            T::Size(_) | T::Ind(_) | T::I32 => Some(Type::new_at(T::I32, &t)),
            _ => None,
        }
    }
}
