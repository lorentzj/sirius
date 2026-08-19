use crate::{parser::Pos, solver::poly::Poly};
use std::rc::Rc;
#[derive(Clone)]
pub struct FunctionType {
    pub p_args: Vec<Pos<String>>,
    pub p_constraints: Vec<Pos<(String, Rc<Poly>)>>,
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
    Tuple(Vec<Type>),
    Poly(Rc<Poly>),
    Function(Box<FunctionType>),
    Array(Box<Type>, Vec<Rc<Poly>>),
}

pub type Type = Pos<T>;

impl Type {
    pub fn new_fn<E>(
        p_args: Vec<Pos<String>>,
        p_constraints: Vec<Pos<(String, Rc<Poly>)>>,
        args: Vec<Type>,
        ret: Type,
        name: &Pos<E>,
    ) -> Type {
        Type::new_at(
            T::Function(Box::new(FunctionType {
                p_args,
                p_constraints,
                args,
                ret,
            })),
            name,
        )
    }

    pub fn poly<E>(inner: Poly, expr: &Pos<E>) -> Self {
        Self::new_at(T::Poly(Rc::new(inner)), expr)
    }
}
