use crate::parser::lexer::ArithCmpOp;

use super::Pos;
use super::lexer::{AssnOp, Op};

pub enum UnaryOp {
    ArithNeg,
    BoolNeg,
    Tick,
    Option,
}

pub enum E {
    Bool(bool),
    Float(f64),
    Int(i64),
    Ident(String),
    Tuple(Vec<Expr>),
    Array(Vec<Expr>),
    UnOp(UnaryOp, Box<Expr>),
    BinOp(Box<Expr>, Op, Box<Expr>),
    FnCall(Box<Expr>, Vec<Expr>, Vec<Expr>),
    Access(Box<Expr>, Vec<AccessDim>),
}

pub type Expr = Pos<E>;

impl Expr {
    pub fn bool(start: usize, val: bool, end: usize) -> Expr {
        Expr {
            start,
            data: E::Bool(val),
            end,
        }
    }

    pub fn float(start: usize, val: f64, end: usize) -> Expr {
        Expr {
            start,
            data: E::Float(val),
            end,
        }
    }

    pub fn int(start: usize, val: i64, end: usize) -> Expr {
        Expr {
            start,
            data: E::Int(val),
            end,
        }
    }

    pub fn ident(start: usize, type_ident: String, end: usize) -> Expr {
        Expr {
            start,
            data: E::Ident(type_ident),
            end,
        }
    }

    pub fn tuple(start: usize, inner: Vec<Expr>, end: usize) -> Expr {
        Expr {
            start,
            data: E::Tuple(inner),
            end,
        }
    }

    pub fn empty_tuple(start: usize, end: usize) -> Expr {
        Expr {
            start,
            data: E::Tuple(vec![]),
            end,
        }
    }

    pub fn un_op(start: usize, op: UnaryOp, inner: Expr, end: usize) -> Expr {
        Expr {
            start,
            data: E::UnOp(op, Box::new(inner)),
            end,
        }
    }

    pub fn bin_op(start: usize, lhs: Expr, op: Op, rhs: Expr, end: usize) -> Expr {
        Expr {
            start,
            data: E::BinOp(Box::new(lhs), op, Box::new(rhs)),
            end,
        }
    }

    pub fn fn_call(
        start: usize,
        caller: Expr,
        type_args: Option<Vec<Expr>>,
        args: Vec<Expr>,
        end: usize,
    ) -> Expr {
        Expr {
            start,
            data: E::FnCall(Box::new(caller), type_args.unwrap_or(vec![]), args),
            end,
        }
    }

    pub fn array(start: usize, inner: Vec<Expr>, end: usize) -> Expr {
        Expr {
            start,
            data: E::Array(inner),
            end,
        }
    }

    pub fn access(start: usize, arr: Expr, dims: Vec<AccessDim>, end: usize) -> Expr {
        Expr {
            start,
            data: E::Access(Box::new(arr), dims),
            end,
        }
    }
}

pub enum AD {
    Range(Option<Box<Expr>>, Option<Box<Expr>>),
    Point(Box<Expr>),
}

pub type AccessDim = Pos<AD>;

impl AccessDim {
    pub fn new_point(start: usize, point: Expr, end: usize) -> AccessDim {
        AccessDim {
            start,
            data: AD::Point(Box::new(point)),
            end,
        }
    }
    pub fn new_range(
        start: usize,
        from: Option<Box<Expr>>,
        to: Option<Box<Expr>>,
        end: usize,
    ) -> AccessDim {
        AccessDim {
            start,
            data: AD::Range(from, to),
            end,
        }
    }
}

pub enum S {
    Print(Expr),
    Return(Expr),
    Yield(Expr),
    YieldFrom(Expr),
    Let {
        mutable: bool,
        name: Pos<String>,
        ann: Option<Expr>,
        value: Expr,
    },
    Assign {
        place: Expr,
        op: AssnOp,
        value: Expr,
    },
    If {
        cond: Expr,
        true_body: Block,
        false_body: Option<Block>,
    },
    For {
        iter: Pos<String>,
        lower: Expr,
        upper: Expr,
        body: Block,
    },
}

pub type Stmt = Pos<S>;

impl Stmt {
    pub fn print(start: usize, data: Expr, end: usize) -> Stmt {
        Stmt {
            start,
            data: S::Print(data),
            end,
        }
    }

    pub fn return_stmt(start: usize, data: Expr, end: usize) -> Stmt {
        Stmt {
            start,
            data: S::Return(data),
            end,
        }
    }

    pub fn yield_stmt(start: usize, data: Expr, end: usize) -> Stmt {
        Stmt {
            start,
            data: S::Yield(data),
            end,
        }
    }

    pub fn yield_from_stmt(start: usize, data: Expr, end: usize) -> Stmt {
        Stmt {
            start,
            data: S::YieldFrom(data),
            end,
        }
    }

    pub fn let_stmt(
        start: usize,
        mutable: bool,
        name: Pos<String>,
        ann: Option<Expr>,
        value: Expr,
        end: usize,
    ) -> Stmt {
        Stmt {
            start,
            data: S::Let {
                mutable,
                name,
                ann,
                value,
            },
            end,
        }
    }

    pub fn assign(start: usize, place: Expr, op: AssnOp, value: Expr, end: usize) -> Stmt {
        Stmt {
            start,
            data: S::Assign { place, op, value },
            end,
        }
    }

    pub fn if_stmt(
        start: usize,
        cond: Expr,
        true_body: Block,
        false_body: Option<Block>,
        end: usize,
    ) -> Stmt {
        Stmt {
            start,
            data: S::If {
                cond,
                true_body,
                false_body,
            },
            end,
        }
    }

    pub fn for_stmt(
        start: usize,
        iter: Pos<String>,
        lower: Expr,
        upper: Expr,
        body: Block,
        end: usize,
    ) -> Stmt {
        Stmt {
            start,
            data: S::For {
                iter,
                lower,
                upper,
                body,
            },
            end,
        }
    }
}

pub struct Block {
    pub stmts: Vec<Stmt>,
}

pub struct Function {
    pub name: Pos<String>,
    pub type_args: Vec<Pos<String>>,
    pub ex_type_args: Vec<Pos<String>>,
    pub type_constraints: Vec<(Pos<String>, ArithCmpOp, Expr)>,
    pub ex_type_constraints: Vec<(Pos<String>, ArithCmpOp, Expr)>,    
    pub args: Vec<(Pos<String>, Expr)>,
    pub ret: Option<Expr>,
    pub body: Block,
}
