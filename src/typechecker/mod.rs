use serde::Serialize;
use std::collections::HashMap;
use std::fmt;

use crate::error::{cardinal, Error, ErrorType};
use crate::lexer::Op;
use crate::parser::{Expression, Statement, UnaryOp, AST};
use crate::stack::Frame;
use crate::stdlib::ExternalGlobals;

mod annotations;
mod equality;
mod number_coersion;

pub use annotations::type_annotations;

#[derive(PartialEq, Serialize, Clone)]
pub enum Type {
    Unknown,
    Void,
    F64,
    I64,
    Bool,
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Void => write!(f, "void"),
            Type::F64 => write!(f, "f64"),
            Type::I64 => write!(f, "i64"),
            Type::Bool => write!(f, "bool"),
            Type::Tuple(v) => {
                if v.is_empty() {
                    write!(f, "())")
                } else {
                    let mut res = "(".to_string();
                    for t in v {
                        res.push_str(&format!("{t:?}"));
                        res.push(',');
                        res.push(' ');
                    }
                    res.pop();
                    res.pop();
                    res.push(')');
                    write!(f, "{res}")
                }
            }
            Type::Function(i, o) => {
                let mut res = "fn(".to_string();
                for t in i {
                    res.push_str(&format!("{t:?}"));
                    res.push(',');
                    res.push(' ');
                }

                res.pop();
                res.pop();
                res.push_str("): ");

                res.push_str(&format!("{o:?}"));

                write!(f, "{res}")
            }
        }
    }
}

type Globals = HashMap<String, Type>;

#[derive(Serialize, Clone, Debug)]
pub enum TypedExpression {
    F64 {
        start: usize,
        val: f64,
        end: usize,
    },
    I64 {
        start: usize,
        val: i64,
        end: usize,
    },
    Bool {
        start: usize,
        val: bool,
        end: usize,
    },
    Identifier {
        t: Type,
        start: usize,
        name: String,
        end: usize,
    },
    BinaryOp {
        t: Type,
        start: usize,
        lhs: Box<TypedExpression>,
        op: Op,
        rhs: Box<TypedExpression>,
        end: usize,
    },
    UnaryOp {
        t: Type,
        start: usize,
        op: UnaryOp,
        inner: Box<TypedExpression>,
        end: usize,
    },
    Tuple {
        t: Type,
        start: usize,
        inner: Vec<TypedExpression>,
        end: usize,
    },
    FnCall {
        t: Type,
        start: usize,
        caller: Box<TypedExpression>,
        args: Vec<TypedExpression>,
        end: usize,
    },
}

impl TypedExpression {
    pub fn get_type(&self) -> Type {
        match self {
            TypedExpression::F64 { .. } => Type::F64,
            TypedExpression::I64 { .. } => Type::I64,
            TypedExpression::Bool { .. } => Type::Bool,
            TypedExpression::Identifier { t, .. } => t.clone(),
            TypedExpression::BinaryOp { t, .. } => t.clone(),
            TypedExpression::UnaryOp { t, .. } => t.clone(),
            TypedExpression::Tuple { t, .. } => t.clone(),
            TypedExpression::FnCall { t, .. } => t.clone(),
        }
    }
}

#[derive(Serialize, Clone, Debug)]
pub enum TypedStatement {
    Let {
        start: usize,
        name: String,
        val: TypedExpression,
        end: usize,
    },
    Print {
        start: usize,
        val: TypedExpression,
        end: usize,
    },
    Return {
        start: usize,
        val: Option<TypedExpression>,
        end: usize,
    },
    If {
        start: usize,
        cond: TypedExpression,
        true_inner: Vec<TypedStatement>,
        false_inner: Option<Vec<TypedStatement>>,
        end: usize,
    },
}

#[derive(Serialize, Debug)]
pub struct TypedFunction {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub return_type: Type,
    pub inner: Vec<TypedStatement>,
}

pub type TypedAST = HashMap<String, TypedFunction>;

pub fn annotation_type(annotation: &Expression) -> Result<Type, Error> {
    match &annotation {
        Expression::Identifier { start, name, end } => {
            if name.eq("f64") {
                Ok(Type::F64)
            } else if name.eq("i64") {
                Ok(Type::I64)
            } else if name.eq("bool") {
                Ok(Type::Bool)
            } else if name.eq("void") {
                Ok(Type::Void)
            } else {
                Err(Error::new(
                    ErrorType::TypeError,
                    "only 'f64', 'i64', 'void', and 'bool' are valid primitives".to_string(),
                    *start,
                    *end,
                ))
            }
        }

        Expression::Tuple { inner, .. } => {
            let mut members = vec![];
            for expression in inner {
                members.push(annotation_type(expression)?);
            }
            Ok(Type::Tuple(members))
        }

        _ => {
            let (start, end) = annotation.range();
            Err(Error::new(
                ErrorType::TypeError,
                "illegal expression in annotation".to_string(),
                start,
                end,
            ))
        }
    }
}

pub fn expression_type(
    expression: &Expression,
    frame: &Frame<Type>,
    globals: &Globals,
    externals: &ExternalGlobals,
) -> Result<TypedExpression, Error> {
    match &expression {
        Expression::Identifier { start, name, end } => match frame.get(name) {
            Some(t) => Ok(TypedExpression::Identifier {
                t: t.clone(),
                start: *start,
                name: name.into(),
                end: *end,
            }),
            None => match globals.get(name) {
                Some(t) => Ok(TypedExpression::Identifier {
                    t: t.clone(),
                    start: *start,
                    name: name.into(),
                    end: *end,
                }),
                None => match externals.get(name) {
                    Some((t, _)) => Ok(TypedExpression::Identifier {
                        t: t.clone(),
                        start: *start,
                        name: name.into(),
                        end: *end,
                    }),
                    None => Err(Error::new(
                        ErrorType::UnboundIdentifierError,
                        format!("identifier '{name}' not found in scope"),
                        *start,
                        *end,
                    )),
                },
            },
        },
        Expression::F64 { start, val, end } => Ok(TypedExpression::F64 {
            start: *start,
            val: *val,
            end: *end,
        }),
        Expression::I64 { start, val, end } => Ok(TypedExpression::I64 {
            start: *start,
            val: *val,
            end: *end,
        }),
        Expression::Bool { start, val, end } => Ok(TypedExpression::Bool {
            start: *start,
            val: *val,
            end: *end,
        }),
        Expression::Tuple { start, inner, end } => {
            let mut members = vec![];
            let mut member_types = vec![];
            for expression in inner {
                members.push(expression_type(expression, frame, globals, externals)?);
                member_types.push(members.last().unwrap().get_type());
            }

            Ok(TypedExpression::Tuple {
                start: *start,
                inner: members,
                t: Type::Tuple(member_types),
                end: *end,
            })
        }

        Expression::OpenTuple { .. } => panic!(),

        Expression::BinaryOp {
            start,
            lhs,
            rhs,
            op,
            end,
        } => {
            let lhs = expression_type(lhs, frame, globals, externals)?;
            let rhs = expression_type(rhs, frame, globals, externals)?;

            match &op {
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Exp | Op::Greater | Op::Less => {
                    let t = number_coersion::arith_coerce(
                        *start,
                        lhs.get_type(),
                        op,
                        rhs.get_type(),
                        *end,
                    )?;
                    Ok(TypedExpression::BinaryOp {
                        t,
                        start: *start,
                        lhs: Box::new(lhs),
                        op: op.clone(),
                        rhs: Box::new(rhs),
                        end: *end,
                    })
                }

                Op::And | Op::Or => match (lhs.get_type(), rhs.get_type()) {
                    (Type::Bool, Type::Bool) => Ok(TypedExpression::BinaryOp {
                        t: Type::Bool,
                        start: *start,
                        lhs: Box::new(lhs),
                        op: op.clone(),
                        rhs: Box::new(rhs),
                        end: *end,
                    }),
                    _ => {
                        if Type::Bool == lhs.get_type() {
                            Err(Error::new(
                                ErrorType::TypeError,
                                format!("cannot apply boolean operator to '{:?}'", rhs.get_type()),
                                *start,
                                *end,
                            ))
                        } else {
                            Err(Error::new(
                                ErrorType::TypeError,
                                format!("cannot apply boolean operator to '{:?}'", lhs.get_type()),
                                *start,
                                *end,
                            ))
                        }
                    }
                },

                Op::Equal | Op::NotEqual => {
                    match equality::equality_check(&lhs.get_type(), &rhs.get_type()) {
                        Ok(()) => Ok(TypedExpression::BinaryOp {
                            t: Type::Bool,
                            start: *start,
                            lhs: Box::new(lhs),
                            op: op.clone(),
                            rhs: Box::new(rhs),
                            end: *end,
                        }),
                        Err(msg) => Err(Error::new(ErrorType::TypeError, msg, *start, *end)),
                    }
                }

                Op::Comma => panic!(),
                Op::Not => panic!(),
                Op::Dot => Err(Error::new(
                    ErrorType::NotImplementedError,
                    "have not implemented dot operator".into(),
                    *start,
                    *end,
                )),
            }
        }

        Expression::UnaryOp {
            start,
            op: UnaryOp::ArithNeg,
            inner,
            end,
        } => {
            let inner_expr = expression_type(inner, frame, globals, externals)?;
            if inner_expr.get_type() == Type::F64 {
                Ok(TypedExpression::UnaryOp {
                    t: Type::F64,
                    start: *start,
                    op: UnaryOp::ArithNeg,
                    inner: Box::new(inner_expr),
                    end: *end,
                })
            } else if inner_expr.get_type() == Type::I64 {
                Ok(TypedExpression::UnaryOp {
                    t: Type::I64,
                    start: *start,
                    op: UnaryOp::ArithNeg,
                    inner: Box::new(inner_expr),
                    end: *end,
                })
            } else {
                Err(Error::new(
                    ErrorType::TypeError,
                    format!(
                        "cannot apply arithmetic negation to '{:?}'",
                        inner_expr.get_type()
                    ),
                    *start,
                    *end,
                ))
            }
        }

        Expression::UnaryOp {
            start,
            op: UnaryOp::BoolNeg,
            inner,
            end,
        } => {
            let inner_expr = expression_type(inner, frame, globals, externals)?;
            if inner_expr.get_type() == Type::Bool {
                Ok(TypedExpression::UnaryOp {
                    t: Type::Bool,
                    start: *start,
                    op: UnaryOp::BoolNeg,
                    inner: Box::new(inner_expr),
                    end: *end,
                })
            } else {
                Err(Error::new(
                    ErrorType::TypeError,
                    format!(
                        "cannot apply boolean negation to '{:?}'",
                        inner_expr.get_type()
                    ),
                    *start,
                    *end,
                ))
            }
        }

        Expression::FnCall {
            start,
            caller,
            args,
            end,
        } => {
            let caller_expr = expression_type(caller, frame, globals, externals)?;
            match caller_expr.get_type() {
                Type::Function(arg_types, return_type) => {
                    let function_name = match caller.as_ref() {
                        Expression::Identifier { name, .. } => format!("function '{name}'"),
                        _ => "anonymous function".into(),
                    };

                    let mut typed_args = vec![];

                    if args.len() != arg_types.len() {
                        Err(Error::new(
                            ErrorType::TypeError,
                            format!(
                                "expected {} argument{} to {}; got {}",
                                arg_types.len(),
                                if arg_types.len() == 1 { "" } else { "s" },
                                function_name,
                                args.len()
                            ),
                            *start,
                            *end,
                        ))
                    } else {
                        for (i, arg) in args.iter().enumerate() {
                            typed_args.push(expression_type(arg, frame, globals, externals)?);
                            if typed_args.last().unwrap().get_type() != arg_types[i] {
                                let (arg_start, arg_end) = arg.range();
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    format!(
                                        "expected '{:?}' as {} argument to {}; got '{:?}'",
                                        arg_types[i],
                                        cardinal(i + 1),
                                        function_name,
                                        typed_args.last().unwrap().get_type()
                                    ),
                                    arg_start,
                                    arg_end,
                                ));
                            }
                        }

                        Ok(TypedExpression::FnCall {
                            t: *return_type,
                            start: *start,
                            caller: Box::new(caller_expr),
                            args: typed_args,
                            end: *end,
                        })
                    }
                }
                t => Err(Error::new(
                    ErrorType::TypeError,
                    format!("cannot call '{t:?}' as function"),
                    *start,
                    *end,
                )),
            }
        }
    }
}

fn typecheck_block(
    statements: &[Statement],
    frame: Option<&mut Frame<Type>>,
    globals: &Globals,
    externals: &ExternalGlobals,
    curr_function: &str,
) -> (Vec<TypedStatement>, Vec<Error>) {
    let mut empty_frame = Frame::default();
    let frame = match frame {
        Some(frame) => frame,
        None => &mut empty_frame,
    };

    frame.push_scope();
    let mut errors = vec![];
    let mut typed_statements = vec![];

    for statement in statements {
        match statement {
            Statement::Let {
                start,
                name,
                ann,
                val,
                end,
            } => match expression_type(val, frame, globals, externals) {
                Ok(typed_expr) => {
                    if let Some(ann) = ann {
                        match annotation_type(ann) {
                            Ok(ann_t) => {
                                if ann_t != typed_expr.get_type() {
                                    let (val_start, val_end) = val.range();

                                    errors.push(Error::new(
                                            ErrorType::TypeError,
                                            format!("annotation '{ann_t:?}' does not match expression '{:?}'", typed_expr.get_type()),
                                            val_start,
                                            val_end,
                                        ));
                                }
                            }
                            Err(error) => errors.push(error),
                        }
                    }
                    frame.insert(name.into(), typed_expr.get_type());
                    typed_statements.push(TypedStatement::Let {
                        start: *start,
                        name: name.into(),
                        val: typed_expr,
                        end: *end,
                    });
                }
                Err(error) => {
                    errors.push(error);
                }
            },

            Statement::Print { start, val, end } => {
                match expression_type(val, frame, globals, externals) {
                    Ok(typed_expr) => typed_statements.push(TypedStatement::Print {
                        start: *start,
                        val: typed_expr,
                        end: *end,
                    }),
                    Err(error) => errors.push(error),
                }
            }

            Statement::If {
                start,
                cond,
                true_inner,
                false_inner,
                end,
            } => {
                let typed_cond = match expression_type(cond, frame, globals, externals) {
                    Ok(typed_cond_expr) => {
                        if typed_cond_expr.get_type() != Type::Bool {
                            let (cond_start, cond_end) = cond.range();

                            errors.push(Error::new(
                                ErrorType::TypeError,
                                format!(
                                    "condition should be 'bool'; found '{:?}'",
                                    typed_cond_expr.get_type()
                                ),
                                cond_start,
                                cond_end,
                            ));
                        }
                        Some(typed_cond_expr)
                    }
                    Err(err) => {
                        errors.push(err);
                        None
                    }
                };

                let (true_inner_statements, mut true_inner_errors) =
                    typecheck_block(true_inner, Some(frame), globals, externals, curr_function);
                frame.pop_scope();
                errors.append(&mut true_inner_errors);

                if let Some(false_inner) = false_inner {
                    let (false_inner_statements, mut false_inner_errors) = typecheck_block(
                        false_inner,
                        Some(frame),
                        globals,
                        externals,
                        curr_function,
                    );

                    frame.pop_scope();
                    errors.append(&mut false_inner_errors);

                    if let Some(typed_cond) = typed_cond {
                        typed_statements.push(TypedStatement::If {
                            start: *start,
                            cond: typed_cond,
                            true_inner: true_inner_statements,
                            false_inner: Some(false_inner_statements),
                            end: *end,
                        });
                    }
                } else if let Some(typed_cond) = typed_cond {
                    typed_statements.push(TypedStatement::If {
                        start: *start,
                        cond: typed_cond,
                        true_inner: true_inner_statements,
                        false_inner: None,
                        end: *end,
                    });
                }
            }

            Statement::Return { start, val, end } => match val {
                Some(val) => {
                    let (val_start, val_end) = val.range();
                    match expression_type(val, frame, globals, externals) {
                        Ok(return_expr) => {
                            if let Some(Type::Function(_, return_type)) = globals.get(curr_function)
                            {
                                if *return_type.as_ref() != return_expr.get_type() {
                                    errors.push(Error::new(
                                        ErrorType::TypeError,
                                        format!(
                                            "return type of function '{curr_function}' should be '{return_type:?}'; found '{:?}'",
                                            return_expr.get_type()
                                        ),
                                        val_start,
                                        val_end,
                                    ));
                                }
                            }

                            typed_statements.push(TypedStatement::Return {
                                start: *start,
                                val: Some(return_expr),
                                end: *end,
                            });
                        }

                        Err(error) => errors.push(error),
                    }
                }
                None => {
                    let Some(Type::Function(_, return_type)) = globals.get(curr_function) else {
                        continue;
                    };

                    if let Type::Void = **return_type {
                        typed_statements.push(TypedStatement::Return {
                            start: *start,
                            val: None,
                            end: *end,
                        });
                    } else {
                        errors.push(Error::new(
                            ErrorType::TypeError,
                            format!(
                                "return type of function '{curr_function}' should be '{:?}'; found 'void'",
                                return_type.as_ref()
                            ),
                            *start,
                            *start + 1,
                        ));
                    }
                }
            },
        }
    }

    (typed_statements, errors)
}

pub fn typecheck(ast: &AST, externals: &ExternalGlobals) -> (TypedAST, Vec<Error>) {
    let mut errors: Vec<Error> = vec![];

    if let Some(function) = ast.get("main") {
        if let Some((_, ann)) = function.args.first() {
            errors.push(Error::new(
                ErrorType::TypeError,
                "main should have no arguments".into(),
                ann.range().0 - 2,
                function.args.last().unwrap().1.range().1,
            ));
        }

        if let Some(ann) = &function.return_type {
            let (ann_start, ann_end) = ann.range();
            let ann_type = annotation_type(ann);
            if Ok(Type::Void) != ann_type {
                errors.push(Error::new(
                    ErrorType::TypeError,
                    "main should have 'void' return type".into(),
                    ann_start - 1,
                    ann_end,
                ));
            }
        }
    }

    let globals: Globals = ast
        .iter()
        .map(|(name, function)| {
            let mut arg_types = vec![];

            for (_, arg_ann) in &function.args {
                match annotation_type(arg_ann) {
                    Ok(t) => arg_types.push(t),
                    Err(error) => {
                        errors.push(error);
                        arg_types.push(Type::Unknown)
                    }
                }
            }

            let return_type = match &function.return_type {
                Some(return_type) => match annotation_type(return_type) {
                    Ok(t) => Box::new(t),
                    Err(error) => {
                        errors.push(error);
                        Box::new(Type::Unknown)
                    }
                },
                None => Box::new(Type::Void),
            };

            (name.clone(), Type::Function(arg_types, return_type))
        })
        .collect();

    let mut typed_ast: HashMap<String, TypedFunction> = HashMap::new();

    for (function_name, function) in ast.iter() {
        let mut frame = Frame::<Type>::default();
        frame.push_scope();
        let mut typed_args = vec![];

        for (name, ann) in &function.args {
            match annotation_type(ann) {
                Ok(ann) => {
                    frame.insert(name.clone(), ann.clone());
                    typed_args.push((name.clone(), ann));
                }
                Err(error) => {
                    errors.push(error);
                }
            }
        }

        let (statements, mut block_errors) = typecheck_block(
            &function.inner,
            Some(&mut frame),
            &globals,
            externals,
            function_name,
        );

        errors.append(&mut block_errors);

        typed_ast.insert(
            function_name.into(),
            TypedFunction {
                name: function_name.clone(),
                args: typed_args,
                return_type: globals[function_name].clone(),
                inner: statements,
            },
        );
    }

    (typed_ast, errors)
}
