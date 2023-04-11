use serde::Serialize;
use std::collections::HashMap;

use crate::error::{Error, ErrorType};
use crate::lexer::Op;
use crate::parser::{Expression, Statement, UnaryOp, AST};
use crate::stack::Frame;
use crate::stdlib::ExternalGlobals;

mod annotations;
mod equality;
mod ind;
mod number_coersion;
mod types;

use ind::Ind;
pub use types::{Substitutions, Type};

pub use annotations::type_annotations;

use self::equality::equality_check;
use self::number_coersion::{arith_coerce, is_arith};

#[derive(Serialize, Clone, Debug)]
pub enum TypedExpression {
    F64 {
        start: usize,
        val: f64,
        end: usize,
    },
    I64 {
        ind: Option<Ind>,
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
    Accessor {
        t: Type,
        start: usize,
        lhs: Box<TypedExpression>,
        rhs: Box<TypedExpression>,
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
            TypedExpression::I64 { ind, .. } => Type::I64(ind.clone()),
            TypedExpression::Bool { .. } => Type::Bool,
            TypedExpression::Identifier { t, .. } => t.clone(),
            TypedExpression::BinaryOp { t, .. } => t.clone(),
            TypedExpression::UnaryOp { t, .. } => t.clone(),
            TypedExpression::Tuple { t, .. } => t.clone(),
            TypedExpression::Accessor { t, .. } => t.clone(),
            TypedExpression::FnCall { t, .. } => t.clone(),
        }
    }

    pub fn start(&self) -> usize {
        match self {
            TypedExpression::F64 { start, .. } => *start,
            TypedExpression::I64 { start, .. } => *start,
            TypedExpression::Bool { start, .. } => *start,
            TypedExpression::Identifier { start, .. } => *start,
            TypedExpression::BinaryOp { start, .. } => *start,
            TypedExpression::UnaryOp { start, .. } => *start,
            TypedExpression::Tuple { start, .. } => *start,
            TypedExpression::Accessor { start, .. } => *start,
            TypedExpression::FnCall { start, .. } => *start,
        }
    }

    pub fn end(&self) -> usize {
        match self {
            TypedExpression::F64 { end, .. } => *end,
            TypedExpression::I64 { end, .. } => *end,
            TypedExpression::Bool { end, .. } => *end,
            TypedExpression::Identifier { end, .. } => *end,
            TypedExpression::BinaryOp { end, .. } => *end,
            TypedExpression::UnaryOp { end, .. } => *end,
            TypedExpression::Tuple { end, .. } => *end,
            TypedExpression::Accessor { end, .. } => *end,
            TypedExpression::FnCall { end, .. } => *end,
        }
    }
}

#[derive(Serialize, Clone, Debug)]
pub enum TypedStatement {
    Let {
        start: usize,
        ann: Option<Type>,
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
    For {
        start: usize,
        iterator: String,
        from: TypedExpression,
        to: TypedExpression,
        inner: Vec<TypedStatement>,
        end: usize,
    },
}

#[derive(Serialize, Debug)]
pub struct TypedFunction {
    pub name: String,
    pub type_args: Vec<String>,
    pub args: Vec<(String, Type)>,
    pub return_type: Type,
    pub inner: Vec<TypedStatement>,
}

pub type TypedAST = HashMap<String, TypedFunction>;

pub fn annotation_type(
    annotation: &Expression,
    type_vars: &[String],
    curr_forall_var: &mut Option<&mut usize>,
) -> Result<Type, Error> {
    match &annotation {
        Expression::Identifier {
            start,
            name,
            type_args,
            end,
        } => {
            if type_args.is_some() {
                Err(Error::new(
                    ErrorType::TypeError,
                    "cannot pass type arguments to type".into(),
                    *start,
                    *end,
                ))
            } else if name.eq("f64") {
                Ok(Type::F64)
            } else if name.eq("i64") {
                Ok(Type::I64(None))
            } else if name.eq("bool") {
                Ok(Type::Bool)
            } else if name.eq("void") {
                Ok(Type::Void)
            } else if type_vars.contains(name) {
                Ok(Type::TypeVar(name.into()))
            } else if name.eq("_") {
                if let Some(curr_forall_var) = curr_forall_var {
                    **curr_forall_var += 1;
                    Ok(Type::ForAll(**curr_forall_var - 1))
                } else {
                    Err(Error::new(
                        ErrorType::TypeError,
                        "cannot use wildcard here".into(),
                        *start,
                        *end,
                    ))
                }
            } else {
                Err(Error::new(
                    ErrorType::TypeError,
                    format!("could not find \"{name}\" in type context"),
                    *start,
                    *end,
                ))
            }
        }

        Expression::Tuple { inner, .. } => {
            let mut members = vec![];
            for expression in inner {
                members.push(annotation_type(expression, type_vars, curr_forall_var)?);
            }
            Ok(Type::Tuple(members))
        }

        Expression::BinaryOp {
            lhs: box Expression::Tuple { inner, .. },
            op: Op::Apply,
            rhs,
            ..
        } => {
            let mut args = vec![];
            for expression in inner {
                args.push(annotation_type(expression, type_vars, curr_forall_var)?);
            }
            let return_type = annotation_type(rhs, type_vars, curr_forall_var)?;

            Ok(Type::Function(vec![], args, Box::new(return_type)))
        }

        Expression::BinaryOp {
            lhs,
            op: Op::Apply,
            rhs,
            ..
        } => {
            let args = vec![annotation_type(lhs, type_vars, curr_forall_var)?];
            let return_type = annotation_type(rhs, type_vars, curr_forall_var)?;
            Ok(Type::Function(vec![], args, Box::new(return_type)))
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

fn initialize_typed_expression(
    expression: &Expression,
    curr_forall_var: &mut usize,
    context: &Frame<Type>,
    type_vars: &[String],
    errors: &mut Vec<Error>,
) -> TypedExpression {
    match expression {
        Expression::Bool { start, val, end } => TypedExpression::Bool {
            start: *start,
            val: *val,
            end: *end,
        },
        Expression::F64 { start, val, end } => TypedExpression::F64 {
            start: *start,
            val: *val,
            end: *end,
        },
        Expression::I64 { start, val, end } => TypedExpression::I64 {
            ind: Ind::constant(*val),
            start: *start,
            val: *val,
            end: *end,
        },
        Expression::UnaryOp {
            start,
            op,
            inner,
            end,
        } => {
            *curr_forall_var += 1;
            TypedExpression::UnaryOp {
                t: Type::ForAll(*curr_forall_var - 1),
                start: *start,
                op: op.clone(),
                inner: Box::new(initialize_typed_expression(
                    inner,
                    curr_forall_var,
                    context,
                    type_vars,
                    errors,
                )),
                end: *end,
            }
        }
        Expression::BinaryOp {
            start,
            lhs,
            op,
            rhs,
            end,
        } => {
            *curr_forall_var += 1;
            TypedExpression::BinaryOp {
                t: Type::ForAll(*curr_forall_var - 1),
                start: *start,
                lhs: Box::new(initialize_typed_expression(
                    lhs,
                    curr_forall_var,
                    context,
                    type_vars,
                    errors,
                )),
                op: op.clone(),
                rhs: Box::new(initialize_typed_expression(
                    rhs,
                    curr_forall_var,
                    context,
                    type_vars,
                    errors,
                )),
                end: *end,
            }
        }
        Expression::Accessor {
            start,
            lhs,
            rhs,
            end,
        } => {
            *curr_forall_var += 1;
            TypedExpression::Accessor {
                t: Type::ForAll(*curr_forall_var - 1),
                start: *start,
                lhs: Box::new(initialize_typed_expression(
                    lhs,
                    curr_forall_var,
                    context,
                    type_vars,
                    errors,
                )),
                rhs: Box::new(initialize_typed_expression(
                    rhs,
                    curr_forall_var,
                    context,
                    type_vars,
                    errors,
                )),
                end: *end,
            }
        }
        Expression::Tuple { start, inner, end } => {
            *curr_forall_var += 1;
            TypedExpression::Tuple {
                t: Type::ForAll(*curr_forall_var - 1),
                start: *start,
                inner: inner
                    .iter()
                    .map(|e| {
                        initialize_typed_expression(e, curr_forall_var, context, type_vars, errors)
                    })
                    .collect(),
                end: *end,
            }
        }
        Expression::FnCall {
            start,
            caller,
            args,
            end,
        } => {
            *curr_forall_var += 1;
            TypedExpression::FnCall {
                t: Type::ForAll(*curr_forall_var - 1),
                start: *start,
                caller: Box::new(initialize_typed_expression(
                    caller,
                    curr_forall_var,
                    context,
                    type_vars,
                    errors,
                )),
                args: args
                    .iter()
                    .map(|e| {
                        initialize_typed_expression(e, curr_forall_var, context, type_vars, errors)
                    })
                    .collect(),
                end: *end,
            }
        }
        Expression::Identifier {
            start,
            name,
            type_args,
            end,
        } => match context.get(name) {
            Some(t) => TypedExpression::Identifier {
                t: match t {
                    Type::Function(function_type_vars, _, _) => {
                        let mut subs: Vec<_> = (*curr_forall_var
                            ..(*curr_forall_var + function_type_vars.len()))
                            .map(Type::ForAll)
                            .collect();
                        *curr_forall_var += function_type_vars.len();

                        if let Some((type_args, _)) = type_args {
                            if function_type_vars.len() < type_args.len() {
                                errors.push(Error::new(
                                    ErrorType::TypeError,
                                    format!(
                                        "expected {} maximum type arguments; found {}",
                                        function_type_vars.len(),
                                        type_args.len()
                                    ),
                                    *start,
                                    *end,
                                ));
                            } else {
                                for i in 0..type_args.len() {
                                    match annotation_type(
                                        &type_args[i],
                                        type_vars,
                                        &mut Some(curr_forall_var),
                                    ) {
                                        Ok(t) => subs[i] = t,
                                        Err(err) => {
                                            errors.push(err);
                                            subs[i] = Type::Unknown
                                        }
                                    }
                                }
                            }
                        }

                        t.instantiate_fn(function_type_vars, &subs)
                    }
                    _ => {
                        if type_args.is_some() {
                            errors.push(Error::new(
                                ErrorType::TypeError,
                                "type arguments only allowed for functions".into(),
                                *start,
                                *end,
                            ));
                            Type::Unknown
                        } else {
                            t.clone()
                        }
                    }
                },
                start: *start,
                name: name.clone(),
                end: *end,
            },
            None => {
                errors.push(Error::new(
                    ErrorType::UnboundIdentifierError,
                    format!("identifier \"{name}\" not found in scope"),
                    *start,
                    *end,
                ));
                TypedExpression::Identifier {
                    t: Type::Unknown,
                    start: *start,
                    name: name.clone(),
                    end: *end,
                }
            }
        },
        Expression::OpenTuple { .. } => panic!(),
    }
}

fn initialize_typed_statement(
    statement: &Statement,
    curr_forall_var: &mut usize,
    context: &mut Frame<Type>,
    errors: &mut Vec<Error>,
    type_vars: &[String],
) -> TypedStatement {
    match statement {
        Statement::Print { start, val, end } => TypedStatement::Print {
            start: *start,
            val: initialize_typed_expression(val, curr_forall_var, context, type_vars, errors),
            end: *end,
        },
        Statement::Let {
            start,
            name,
            ann,
            val,
            end,
        } => {
            let val = initialize_typed_expression(val, curr_forall_var, context, type_vars, errors);

            context.insert(name.clone(), val.get_type());

            let ann = match ann {
                Some(ann) => match annotation_type(ann, type_vars, &mut Some(curr_forall_var)) {
                    Ok(t) => Some(t),
                    Err(e) => {
                        errors.push(e);
                        Some(Type::Unknown)
                    }
                },
                None => None,
            };

            TypedStatement::Let {
                start: *start,
                ann,
                name: name.clone(),
                val,
                end: *end,
            }
        }
        Statement::If {
            start,
            cond,
            true_inner,
            false_inner,
            end,
        } => {
            context.push_scope();
            let true_inner = {
                true_inner
                    .iter()
                    .map(|s| {
                        initialize_typed_statement(s, curr_forall_var, context, errors, type_vars)
                    })
                    .collect()
            };
            context.pop_scope();
            context.push_scope();
            let false_inner = false_inner.as_ref().map(|false_inner| {
                false_inner
                    .iter()
                    .map(|s| {
                        initialize_typed_statement(s, curr_forall_var, context, errors, type_vars)
                    })
                    .collect()
            });
            context.pop_scope();
            TypedStatement::If {
                start: *start,
                cond: initialize_typed_expression(
                    cond,
                    curr_forall_var,
                    context,
                    type_vars,
                    errors,
                ),
                true_inner,
                false_inner,
                end: *end,
            }
        }
        Statement::For {
            start,
            iterator,
            from,
            to,
            inner,
            end,
        } => {
            context.push_scope();
            context.insert(iterator.clone(), Type::I64(None));
            let inner = inner
                .iter()
                .map(|s| initialize_typed_statement(s, curr_forall_var, context, errors, type_vars))
                .collect();
            context.pop_scope();

            TypedStatement::For {
                start: *start,
                iterator: iterator.clone(),
                from: initialize_typed_expression(
                    from,
                    curr_forall_var,
                    context,
                    type_vars,
                    errors,
                ),
                to: initialize_typed_expression(to, curr_forall_var, context, type_vars, errors),
                inner,
                end: *end,
            }
        }
        Statement::Return { start, val, end } => TypedStatement::Return {
            start: *start,
            val: val.as_ref().map(|v| {
                initialize_typed_expression(v, curr_forall_var, context, type_vars, errors)
            }),
            end: *end,
        },
    }
}

fn unify_expression(
    expression: &TypedExpression,
    context: &Frame<Type>,
    curr_forall_var: &mut usize,
) -> Result<Substitutions, Error> {
    match expression {
        TypedExpression::Bool { .. } => Ok(Substitutions::new()),
        TypedExpression::F64 { .. } => Ok(Substitutions::new()),
        TypedExpression::I64 { .. } => Ok(Substitutions::new()),
        TypedExpression::Accessor {
            t,
            start,
            lhs,
            rhs,
            end,
        } => {
            let lhs_subs = unify_expression(lhs, context, curr_forall_var)?;
            let rhs_subs = unify_expression(rhs, context, curr_forall_var)?;

            match lhs.get_type() {
                Type::Tuple(lhs_members) => match rhs.get_type() {
                    Type::I64(Some(ind)) => match ind.constant_val() {
                        Some((sign, val)) => {
                            if val >= lhs_members.len() || sign {
                                Err(Error::new(
                                    ErrorType::TypeError,
                                    format!("index out of bounds of {}-tuple", lhs_members.len()),
                                    *start,
                                    *end,
                                ))
                            } else {
                                let mut subs = lhs_subs;
                                subs.extend(rhs.start(), rhs_subs, rhs.end())?;
                                match t.unify(&lhs_members[val]) {
                                    Some(s) => {
                                        subs.extend(lhs.start(), s, lhs.end())?;
                                        Ok(subs)
                                    }
                                    None => Err(Error::new(
                                        ErrorType::TypeError,
                                        format!(
                                            "cannot unify types \"{:?}\" and \"{:?}\"",
                                            t, &lhs_members[val]
                                        ),
                                        *start,
                                        *end,
                                    )),
                                }
                            }
                        }
                        None => Err(Error::new(
                            ErrorType::TypeError,
                            "tuple index must be statically known".into(),
                            *start,
                            *end,
                        )),
                    },
                    Type::I64(None) => Err(Error::new(
                        ErrorType::TypeError,
                        "tuple index must be statically known".into(),
                        *start,
                        *end,
                    )),
                    Type::ForAll(_) => {
                        let mut subs = lhs_subs;
                        subs.extend(*start, rhs_subs, *end)?;
                        Ok(subs)
                    }
                    t => Err(Error::new(
                        ErrorType::TypeError,
                        format!("cannot index tuple with type \"{t:?}\""),
                        *start,
                        *end,
                    )),
                },
                Type::ForAll(_) => match rhs.get_type() {
                    Type::I64(Some(ind)) => match ind.constant_val() {
                        Some(_) => {
                            let mut subs = lhs_subs;
                            subs.extend(*start, rhs_subs, *end)?;
                            Ok(subs)
                        }
                        None => Err(Error::new(
                            ErrorType::TypeError,
                            "tuple index must be statically known".into(),
                            *start,
                            *end,
                        )),
                    },
                    Type::I64(None) => Err(Error::new(
                        ErrorType::TypeError,
                        "tuple index must be statically known".into(),
                        *start,
                        *end,
                    )),
                    Type::ForAll(_) => Ok(Substitutions::new()),
                    t => Err(Error::new(
                        ErrorType::TypeError,
                        format!("cannot index tuple with type \"{t:?}\""),
                        *start,
                        *end,
                    )),
                },
                t => Err(Error::new(
                    ErrorType::TypeError,
                    format!("cannot index into type \"{t:?}\""),
                    *start,
                    *end,
                )),
            }
        }
        TypedExpression::Identifier {
            start,
            t,
            name,
            end,
        } => match context.get(name) {
            Some(var_t) => match var_t {
                Type::Function(_, _, _) => Ok(Substitutions::new()),
                _ => match t.unify(var_t) {
                    Some(subs) => Ok(subs),
                    None => Err(Error::new(
                        ErrorType::TypeError,
                        format!("cannot unify types \"{t:?}\" and \"{var_t:?}\""),
                        *start,
                        *end,
                    )),
                },
            },
            None => match t.unify(&Type::Unknown) {
                Some(subs) => Ok(subs),
                None => Err(Error::new(
                    ErrorType::TypeError,
                    format!("cannot unify types \"{t:?}\" and \"{:?}\"", Type::Unknown),
                    *start,
                    *end,
                )),
            },
        },
        TypedExpression::UnaryOp {
            t,
            start,
            op,
            inner,
            end,
        } => {
            let mut subs = unify_expression(inner, context, curr_forall_var)?;

            if inner.get_type().forall_vars().is_empty() {
                match op {
                    UnaryOp::ArithNeg => match inner.get_type() {
                        Type::F64 => match Type::F64.unify(t) {
                            Some(s) => subs.extend(*start, s, *end)?,
                            None => {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    format!("cannot unify types \"{t:?}\" and \"{:?}\"", Type::F64),
                                    *start,
                                    *end,
                                ))
                            }
                        },
                        Type::I64(_) => match Type::I64(None).unify(t) {
                            Some(s) => subs.extend(*start, s, *end)?,
                            None => {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    format!(
                                        "cannot unify types \"{t:?}\" and \"{:?}\"",
                                        Type::I64(None)
                                    ),
                                    *start,
                                    *end,
                                ))
                            }
                        },
                        t => {
                            return Err(Error::new(
                                ErrorType::TypeError,
                                format!("cannot apply arithmetic negation to type \"{t:?}\""),
                                *start,
                                *end,
                            ))
                        }
                    },
                    UnaryOp::BoolNeg => match inner.get_type() {
                        Type::Bool => match Type::Bool.unify(t) {
                            Some(s) => subs.extend(*start, s, *end)?,
                            None => {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    format!(
                                        "cannot unify types \"{t:?}\" and \"{:?}\"",
                                        Type::Bool
                                    ),
                                    *start,
                                    *end,
                                ))
                            }
                        },
                        t => {
                            return Err(Error::new(
                                ErrorType::TypeError,
                                format!("cannot apply boolean negation to type \"{t:?}\""),
                                *start,
                                *end,
                            ))
                        }
                    },
                    UnaryOp::Tick => {
                        return Err(Error::new(
                            ErrorType::NotImplementedError,
                            "tick operator not implemented yet".into(),
                            *start,
                            *end,
                        ))
                    }
                }
            }

            Ok(subs)
        }
        TypedExpression::Tuple {
            t,
            start,
            inner,
            end,
        } => {
            let mut subs = Substitutions::new();
            let inner_types = inner.iter().map(|e| e.get_type()).collect();
            for e in inner {
                subs.extend(*start, unify_expression(e, context, curr_forall_var)?, *end)?;
            }

            let tuple_t = Type::Tuple(inner_types);

            match t.unify(&tuple_t) {
                Some(s) => subs.extend(*start, s, *end)?,
                None => {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        format!("cannot unify types \"{t:?}\" and \"{tuple_t:?}\""),
                        *start,
                        *end,
                    ))
                }
            }

            Ok(subs)
        }
        TypedExpression::BinaryOp {
            t,
            start,
            lhs,
            op,
            rhs,
            end,
        } => {
            let lhs_subs = unify_expression(lhs, context, curr_forall_var)?;
            let rhs_subs = unify_expression(rhs, context, curr_forall_var)?;

            let mut subs = lhs_subs;
            subs.extend(rhs.start(), rhs_subs, rhs.end())?;

            let lhs_type = lhs.get_type();
            let rhs_type = rhs.get_type();

            if lhs_type.forall_vars().is_empty() && rhs_type.forall_vars().is_empty() {
                if is_arith(op) {
                    let arith_result = arith_coerce(*start, lhs_type, op, rhs_type, *end)?;

                    match t.unify(&arith_result) {
                        Some(s) => subs.extend(*start, s, *end)?,
                        None => {
                            return Err(Error::new(
                                ErrorType::TypeError,
                                format!("cannot unify types \"{t:?}\" and \"{arith_result:?}\""),
                                *start,
                                *end,
                            ))
                        }
                    }
                } else {
                    match op {
                        Op::Equal | Op::NotEqual => match equality_check(&lhs_type, &rhs_type) {
                            Ok(_) => match t.unify(&Type::Bool) {
                                Some(s) => subs.extend(*start, s, *end)?,
                                None => {
                                    return Err(Error::new(
                                        ErrorType::TypeError,
                                        format!(
                                            "cannot unify types \"{t:?}\" and \"{:?}\"",
                                            Type::Bool
                                        ),
                                        *start,
                                        *end,
                                    ))
                                }
                            },
                            Err(msg) => {
                                return Err(Error::new(ErrorType::TypeError, msg, *start, *end))
                            }
                        },

                        Op::Dot => {
                            return Err(Error::new(
                                ErrorType::NotImplementedError,
                                "dot operator not implemented yet".into(),
                                *start,
                                *end,
                            ))
                        }
                        Op::And | Op::Or => {
                            if lhs_type != Type::Bool {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    format!(
                                        "cannot apply boolean operator to type \"{lhs_type:?}\"",
                                    ),
                                    *start,
                                    *end,
                                ));
                            } else if rhs_type != Type::Bool {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    format!(
                                        "cannot apply boolean operator to type \"{rhs_type:?}\"",
                                    ),
                                    *start,
                                    *end,
                                ));
                            } else {
                                match t.unify(&Type::Bool) {
                                    Some(s) => subs.extend(*start, s, *end)?,
                                    None => {
                                        return Err(Error::new(
                                            ErrorType::TypeError,
                                            format!(
                                                "cannot unify types \"{t:?}\" and \"{:?}\"",
                                                Type::Bool
                                            ),
                                            *start,
                                            *end,
                                        ))
                                    }
                                }
                            }
                        }
                        Op::Apply => {
                            return Err(Error::new(
                                ErrorType::TypeError,
                                "cannot use function operator in expression".into(),
                                *start,
                                *end,
                            ))
                        }
                        _ => panic!(),
                    }
                }
            }

            Ok(subs)
        }
        TypedExpression::FnCall {
            t,
            start,
            caller,
            args,
            end,
        } => {
            let mut subs = unify_expression(caller, context, curr_forall_var)?;
            let arg_types: Vec<_> = args.iter().map(|e| e.get_type()).collect();
            for e in args {
                subs.extend(*start, unify_expression(e, context, curr_forall_var)?, *end)?;
            }

            match caller.get_type() {
                Type::Function(type_vars, _, _) => match caller.get_type() {
                    Type::Function(_, i, o) => {
                        if i.len() != arg_types.len() {
                            return Err(Error::new(
                                ErrorType::TypeError,
                                format!(
                                    "expected {} arguments to function; found {}",
                                    i.len(),
                                    arg_types.len()
                                ),
                                *start,
                                *end,
                            ));
                        } else {
                            for (given_arg, expected_arg) in arg_types.iter().zip(i) {
                                match given_arg.unify(&expected_arg) {
                                    Some(s) => {
                                        subs.extend(*start, s, *end)?;
                                    },
                                    None => {
                                        return Err(Error::new(
                                            ErrorType::TypeError,
                                            format!("cannot unify types \"{given_arg:?}\" and \"{expected_arg:?}\""),
                                            *start,
                                            *end,
                                        ))
                                    }
                                }
                            }
                        }

                        match t.unify(o.as_ref()) {
                            Some(s) => subs.extend(*start, s, *end)?,
                            None => {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    format!("cannot unify types \"{t:?}\" and \"{o:?}\""),
                                    *start,
                                    *end,
                                ))
                            }
                        }

                        *curr_forall_var += type_vars.len();
                    }
                    _ => panic!(),
                },
                Type::ForAll(_) => (),
                t => {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        format!("cannot call type \"{t:?}\" as function"),
                        *start,
                        *end,
                    ))
                }
            }

            Ok(subs)
        }
    }
}

pub fn unify(
    block: &[TypedStatement],
    context: &mut Frame<Type>,
    curr_forall_var: &mut usize,
    return_type: &Type,
    errors: &mut Vec<Error>,
    subs: &mut Substitutions,
) {
    for statement in block {
        match statement {
            TypedStatement::Print { val, .. } => {
                match unify_expression(val, context, curr_forall_var) {
                    Ok(s) => match subs.extend(val.start(), s, val.end()) {
                        Ok(()) => (),
                        Err(error) => errors.push(error),
                    },
                    Err(error) => errors.push(error),
                }
            }
            TypedStatement::Let { ann, name, val, .. } => {
                match unify_expression(val, context, curr_forall_var) {
                    Ok(s) => match subs.extend(val.start(), s, val.end()) {
                        Ok(()) => (),
                        Err(error) => errors.push(error),
                    },
                    Err(error) => errors.push(error),
                }

                context.insert(name.clone(), val.get_type());

                if let Some(ann) = ann {
                    match val.get_type().unify(ann) {
                        Some(s) => match subs.extend(val.start(), s, val.end()) {
                            Ok(()) => (),
                            Err(error) => errors.push(error),
                        },
                        None => errors.push(Error::new(
                            ErrorType::TypeError,
                            format!(
                                "cannot unify type \"{:?}\" with annotation \"{ann:?}\"",
                                val.get_type()
                            ),
                            val.start(),
                            val.end(),
                        )),
                    }
                }

                match context.get(name) {
                    Some(var_t) => match val.get_type().unify(var_t) {
                        Some(s) => match subs.extend(val.start(), s, val.end()) {
                            Ok(()) => (),
                            Err(error) => errors.push(error),
                        },
                        None => errors.push(Error::new(
                            ErrorType::TypeError,
                            format!(
                                "cannot unify types \"{:?}\" and \"{var_t:?}\"",
                                val.get_type()
                            ),
                            val.start(),
                            val.end(),
                        )),
                    },
                    None => panic!(),
                }
            }
            TypedStatement::Return { start, val, end } => match val {
                Some(val) => {
                    if return_type == &Type::Void {
                        errors.push(Error::new(
                            ErrorType::TypeError,
                            "cannot return value from void function".into(),
                            *start,
                            *end,
                        ));
                    } else {
                        match unify_expression(val, context, curr_forall_var) {
                            Ok(s) => match subs.extend(val.start(), s, val.end()) {
                                Ok(()) => (),
                                Err(error) => errors.push(error),
                            },
                            Err(error) => errors.push(error),
                        }

                        match val.get_type().unify(return_type) {
                            Some(s) => match subs.extend(val.start(), s, val.end()) {
                                Ok(()) => (),
                                Err(error) => errors.push(error),
                            },
                            None => errors.push(Error::new(
                                ErrorType::TypeError,
                                format!(
                                    "cannot unify type \"{:?}\" with return type \"{return_type:?}\"",
                                    val.get_type()
                                ),
                                val.start(),
                                val.end(),
                            )),
                        }
                    }
                }
                None => {
                    if return_type != &Type::Void {
                        errors.push(Error::new(
                            ErrorType::TypeError,
                            "cannot return void from non-void function".into(),
                            *start,
                            *end,
                        ));
                    }
                }
            },
            TypedStatement::If {
                cond,
                true_inner,
                false_inner,
                ..
            } => {
                match unify_expression(cond, context, curr_forall_var) {
                    Ok(s) => {
                        match subs.extend(cond.start(), s, cond.end()) {
                            Ok(()) => (),
                            Err(error) => errors.push(error),
                        };
                        match cond.get_type().unify(&Type::Bool) {
                            Some(s) => match subs.extend(cond.start(), s, cond.end()) {
                                Ok(()) => (),
                                Err(error) => errors.push(error),
                            },
                            None => errors.push(Error::new(
                                ErrorType::TypeError,
                                "condition must be boolean type".into(),
                                cond.start(),
                                cond.end(),
                            )),
                        }
                    }
                    Err(error) => errors.push(error),
                }

                context.push_scope();
                unify(
                    true_inner,
                    context,
                    curr_forall_var,
                    return_type,
                    errors,
                    subs,
                );
                context.pop_scope();

                match false_inner {
                    Some(false_inner) => {
                        context.push_scope();
                        unify(
                            false_inner,
                            context,
                            curr_forall_var,
                            return_type,
                            errors,
                            subs,
                        );
                        context.pop_scope();
                    }
                    None => (),
                }
            }
            TypedStatement::For {
                iterator,
                from,
                to,
                inner,
                ..
            } => {
                match unify_expression(from, context, curr_forall_var) {
                    Ok(s) => {
                        match subs.extend(from.start(), s, from.end()) {
                            Ok(()) => (),
                            Err(error) => errors.push(error),
                        };
                        match from.get_type().unify(&Type::I64(None)) {
                            Some(s) => match subs.extend(from.start(), s, from.end()) {
                                Ok(()) => (),
                                Err(error) => errors.push(error),
                            },
                            None => errors.push(Error::new(
                                ErrorType::TypeError,
                                "iteration start must be integer type".into(),
                                from.start(),
                                from.end(),
                            )),
                        }
                    }
                    Err(error) => errors.push(error),
                }

                match unify_expression(to, context, curr_forall_var) {
                    Ok(s) => {
                        match subs.extend(to.start(), s, to.end()) {
                            Ok(()) => (),
                            Err(error) => errors.push(error),
                        };
                        match to.get_type().unify(&Type::I64(None)) {
                            Some(s) => match subs.extend(to.start(), s, to.end()) {
                                Ok(()) => (),
                                Err(error) => errors.push(error),
                            },
                            None => errors.push(Error::new(
                                ErrorType::TypeError,
                                "iteration end must be integer type".into(),
                                to.start(),
                                to.end(),
                            )),
                        }
                    }
                    Err(error) => errors.push(error),
                }

                context.push_scope();
                context.insert(iterator.clone(), Type::I64(None));
                unify(inner, context, curr_forall_var, return_type, errors, subs);
                context.pop_scope();
            }
        }
    }
}

pub fn substitute_expression(expression: &mut TypedExpression, subs: &Substitutions) {
    match expression {
        TypedExpression::Accessor { t, lhs, rhs, .. } => {
            for sub in subs.iter() {
                *t = t.substitute(sub);
            }
            substitute_expression(lhs, subs);
            substitute_expression(rhs, subs);
        }
        TypedExpression::BinaryOp { t, lhs, rhs, .. } => {
            for sub in subs.iter() {
                *t = t.substitute(sub);
            }
            substitute_expression(lhs, subs);
            substitute_expression(rhs, subs);
        }
        TypedExpression::UnaryOp { t, inner, .. } => {
            for sub in subs.iter() {
                *t = t.substitute(sub);
            }
            substitute_expression(inner, subs);
        }
        TypedExpression::FnCall {
            t, caller, args, ..
        } => {
            for sub in subs.iter() {
                *t = t.substitute(sub);
            }
            substitute_expression(caller, subs);
            for arg in args {
                substitute_expression(arg, subs);
            }
        }
        TypedExpression::Identifier { t, .. } => {
            for sub in subs.iter() {
                *t = t.substitute(sub);
            }
        }
        TypedExpression::Tuple { t, inner, .. } => {
            for sub in subs.iter() {
                *t = t.substitute(sub);
            }
            for e in inner {
                substitute_expression(e, subs);
            }
        }
        _ => (),
    }
}

pub fn substitute(block: &mut [TypedStatement], context: &mut Frame<Type>, subs: &Substitutions) {
    for statement in block.iter_mut() {
        match statement {
            TypedStatement::Print { val, .. } => substitute_expression(val, subs),
            TypedStatement::Let { name, ann, val, .. } => {
                substitute_expression(val, subs);
                if let Some(ann) = ann {
                    for sub in subs.iter() {
                        *ann = ann.substitute(sub);
                    }
                }
                context.insert(name.clone(), val.get_type());
            }
            TypedStatement::If {
                cond,
                true_inner,
                false_inner,
                ..
            } => {
                substitute_expression(cond, subs);
                substitute(true_inner, context, subs);
                match false_inner {
                    Some(false_inner) => substitute(false_inner, context, subs),
                    None => (),
                }
            }
            TypedStatement::For {
                iterator,
                from,
                to,
                inner,
                ..
            } => {
                substitute_expression(from, subs);
                substitute_expression(to, subs);
                context.push_scope();
                context.insert(iterator.clone(), Type::I64(None));
                substitute(inner, context, subs);
                context.pop_scope();
            }
            TypedStatement::Return { val, .. } => match val {
                Some(val) => {
                    substitute_expression(val, subs);
                }
                None => (),
            },
        }
    }
}

fn check_expr_for_foralls(expression: &TypedExpression) -> Vec<Error> {
    let mut errors: Vec<Error> = vec![];
    match expression {
        TypedExpression::Accessor {
            start,
            t,
            lhs,
            rhs,
            end,
        } => {
            if !t.forall_vars().is_empty() {
                errors.push(Error::new(
                    ErrorType::TypeError,
                    format!(
                        "type \"{t:?}\" is not concrete; try adding annotations or type arguments"
                    ),
                    *start,
                    *end,
                ));
            }

            errors.extend(check_expr_for_foralls(lhs));
            errors.extend(check_expr_for_foralls(rhs));
            errors
        }
        TypedExpression::BinaryOp {
            t,
            start,
            lhs,
            rhs,
            end,
            ..
        } => {
            if !t.forall_vars().is_empty() {
                errors.push(Error::new(
                    ErrorType::TypeError,
                    format!(
                        "type \"{t:?}\" is not concrete; try adding annotations or type arguments"
                    ),
                    *start,
                    *end,
                ));
            }

            errors.extend(check_expr_for_foralls(lhs));
            errors.extend(check_expr_for_foralls(rhs));
            errors
        }
        TypedExpression::Tuple {
            t,
            start,
            inner,
            end,
        } => {
            if !t.forall_vars().is_empty() {
                errors.push(Error::new(
                    ErrorType::TypeError,
                    format!(
                        "type \"{t:?}\" is not concrete; try adding annotations or type arguments"
                    ),
                    *start,
                    *end,
                ));
            }

            for expr in inner {
                errors.extend(check_expr_for_foralls(expr));
            }
            errors
        }
        TypedExpression::UnaryOp {
            t,
            start,
            inner,
            end,
            ..
        } => {
            if !t.forall_vars().is_empty() {
                errors.push(Error::new(
                    ErrorType::TypeError,
                    format!(
                        "type \"{t:?}\" is not concrete; try adding annotations or type arguments"
                    ),
                    *start,
                    *end,
                ));
            }

            errors.extend(check_expr_for_foralls(inner));
            errors
        }
        TypedExpression::FnCall {
            t,
            start,
            caller,
            args,
            end,
        } => {
            if !t.forall_vars().is_empty() {
                errors.push(Error::new(
                    ErrorType::TypeError,
                    format!(
                        "type \"{t:?}\" is not concrete; try adding annotations or type arguments"
                    ),
                    *start,
                    *end,
                ));
            }

            errors.extend(check_expr_for_foralls(caller));
            for expr in args {
                errors.extend(check_expr_for_foralls(expr));
            }

            errors
        }
        TypedExpression::Identifier { t, start, end, .. } => {
            if !t.forall_vars().is_empty() {
                errors.push(Error::new(
                    ErrorType::TypeError,
                    format!(
                        "type \"{t:?}\" is not concrete; try adding annotations or type arguments"
                    ),
                    *start,
                    *end,
                ));
            }
            errors
        }
        _ => errors,
    }
}

fn check_stmt_for_foralls(statement: &TypedStatement) -> Vec<Error> {
    let mut errors = vec![];

    match statement {
        TypedStatement::Print { val, .. } => errors.extend(check_expr_for_foralls(val)),
        TypedStatement::Return { val: Some(val), .. } => errors.extend(check_expr_for_foralls(val)),
        TypedStatement::Return { val: None, .. } => (),
        TypedStatement::Let { val, .. } => errors.extend(check_expr_for_foralls(val)),
        TypedStatement::If {
            cond,
            true_inner,
            false_inner,
            ..
        } => {
            errors.extend(check_expr_for_foralls(cond));
            for stmt in true_inner {
                errors.extend(check_stmt_for_foralls(stmt));
            }
            if let Some(false_inner) = false_inner {
                for stmt in false_inner {
                    errors.extend(check_stmt_for_foralls(stmt));
                }
            }
        }
        TypedStatement::For {
            from, to, inner, ..
        } => {
            errors.extend(check_expr_for_foralls(from));
            errors.extend(check_expr_for_foralls(to));
            for stmt in inner {
                errors.extend(check_stmt_for_foralls(stmt));
            }
        }
    }

    errors
}

pub fn typecheck(ast: &AST, externals: &ExternalGlobals) -> (TypedAST, Vec<Error>) {
    let mut errors: Vec<Error> = vec![];

    let mut frame: Frame<Type> = Frame::default();
    frame.push_scope();

    for (name, (t, _)) in externals.iter() {
        frame.insert(name.clone(), t.clone());
    }

    for (name, function) in ast.iter() {
        let mut type_arg_names = vec![];
        for (type_arg_name, (type_arg_start, type_arg_end)) in function.type_args.iter() {
            if type_arg_names.contains(type_arg_name) {
                errors.push(Error::new(
                    ErrorType::NameError,
                    format!("duplicate type argument name \"{type_arg_name}\""),
                    *type_arg_start,
                    *type_arg_end,
                ));
            }

            type_arg_names.push(type_arg_name.clone());
        }

        let mut arg_names = vec![];
        for (arg_name, (arg_start, arg_end), _) in function.args.iter() {
            if arg_names.contains(arg_name) {
                errors.push(Error::new(
                    ErrorType::NameError,
                    format!("duplicate argument name \"{arg_name}\""),
                    *arg_start,
                    *arg_end,
                ));
            }

            if type_arg_names.contains(arg_name) {
                errors.push(Error::new(
                    ErrorType::NameError,
                    format!("duplicate argument/type argument name \"{arg_name}\""),
                    *arg_start,
                    *arg_end,
                ));
            }

            arg_names.push(arg_name.clone());
        }

        let mut arg_types = vec![];

        for (_, _, arg_ann) in &function.args {
            match annotation_type(arg_ann, &function.get_type_arg_names(), &mut None) {
                Ok(t) => arg_types.push(t),
                Err(error) => {
                    errors.push(error);
                    arg_types.push(Type::Unknown)
                }
            }
        }

        let return_type = match &function.return_type {
            Some(return_type) => {
                match annotation_type(return_type, &function.get_type_arg_names(), &mut None) {
                    Ok(t) => Box::new(t),
                    Err(error) => {
                        errors.push(error);
                        Box::new(Type::Unknown)
                    }
                }
            }
            None => Box::new(Type::Void),
        };

        frame.insert(
            name.clone(),
            Type::Function(function.get_type_arg_names(), arg_types, return_type),
        );
    }

    if let Some(f) = ast.get("main") {
        let return_type = match frame.get("main") {
            Some(Type::Function(_, _, o)) => o.as_ref().clone(),
            _ => panic!(),
        };

        if !f.args.is_empty() || !f.type_args.is_empty() || return_type != Type::Void {
            errors.push(Error::new(
                ErrorType::TypeError,
                format!(
                    "function \"main\" must have type \"{:?}\"",
                    Type::Function(vec![], vec![], Box::new(Type::Void))
                ),
                f.sig_range.0,
                f.sig_range.1,
            ));
        }
    }

    let typed_ast: TypedAST = ast
        .iter()
        .map(|(name, function)| {
            frame.push_scope();

            let mut curr_forall_var = 0;

            let (args, return_type) = match frame.get(name) {
                Some(Type::Function(_, i, o)) => (
                    function
                        .args
                        .iter()
                        .map(|x| x.0.clone())
                        .zip(i.clone())
                        .collect::<Vec<(String, Type)>>(),
                    *o.clone(),
                ),
                _ => panic!(),
            };

            for (arg_name, arg_type) in &args {
                frame.insert(arg_name.clone(), arg_type.clone());
            }

            frame.push_scope();

            let mut typed_function = TypedFunction {
                name: name.clone(),
                type_args: function.get_type_arg_names(),
                args,
                return_type,
                inner: function
                    .inner
                    .iter()
                    .map(|s| {
                        initialize_typed_statement(
                            s,
                            &mut curr_forall_var,
                            &mut frame,
                            &mut errors,
                            &function.get_type_arg_names(),
                        )
                    })
                    .collect(),
            };

            frame.pop_scope();

            frame.push_scope();
            let mut subs = Substitutions::new();

            unify(
                &typed_function.inner,
                &mut frame,
                &mut curr_forall_var,
                &typed_function.return_type,
                &mut errors,
                &mut subs,
            );
            frame.pop_scope();

            while !subs.is_empty() && errors.is_empty() {
                frame.push_scope();
                substitute(&mut typed_function.inner, &mut frame, &subs);
                frame.pop_scope();

                subs = Substitutions::new();
                frame.push_scope();
                unify(
                    &typed_function.inner,
                    &mut frame,
                    &mut curr_forall_var,
                    &typed_function.return_type,
                    &mut errors,
                    &mut subs,
                );
                frame.pop_scope();
            }

            if errors.is_empty() {
                for statement in &typed_function.inner {
                    errors.extend(check_stmt_for_foralls(statement));
                }
            }

            (name.clone(), typed_function)
        })
        .collect();

    (typed_ast, errors)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::Ind;
    use super::Type;
    use super::{typecheck, TypedStatement};
    use crate::error::ErrorType;
    use crate::parser::parse;

    #[test]
    fn basic_typecheck() {
        let code = "
fn three_tuple_map{A, B}(x: (A, A, A), f: A->B) -> (B, B, B):
    return (f(x[0]), f(x[1]), f(x[2]))

fn map_test_1(x: f64) -> bool:
    return x > 0

fn map_test_2(x: f64) -> i64:
    if x > 0:
        return 1
    else:
        return -1

fn main():
    let a = (1., -1., -0.34)
    print three_tuple_map(a, map_test_1)
    print three_tuple_map{f64}(a, map_test_1)
    print three_tuple_map{f64, bool}(a, map_test_1)
    print three_tuple_map{_, bool}(a, map_test_1)
    print three_tuple_map(a, map_test_2)
        ";

        let ast = parse(code).ast;
        let typed_ast = typecheck(&ast, &HashMap::default());

        assert!(typed_ast.1.is_empty());
    }

    #[test]
    fn error_param_basic() {
        let code = "
fn factorial(x: i64) -> i64:
    if x < 1:
        return 1
    else:
        return x * factorial(x - 1)

fn main():
    print factorial((1, 1))
";

        let ast = parse(code).ast;
        let typed_ast = typecheck(&ast, &HashMap::default());

        assert_eq!(ErrorType::TypeError, typed_ast.1[0].error_type);
    }

    #[test]
    fn error_param_generic() {
        let code = "
fn three_tuple_map{A, B}(x: (A, A, A), f: A->B) -> (B, B, B):
    return (f(x[0]), f(x[1]), f(x[2]))

fn map_test(x: i64) -> bool:
    return x > 0

fn main():
    let z = (1., -1., -0.34)
    print three_tuple_map(z, map_test)
";

        let ast = parse(code).ast;
        let typed_ast = typecheck(&ast, &HashMap::default());

        assert_eq!(ErrorType::TypeError, typed_ast.1[0].error_type);
    }

    #[test]
    fn ind_val() {
        let code = "
fn main():
    let x = 1 + 1 + 1
";

        let ast = parse(code).ast;
        let typed_ast = typecheck(&ast, &HashMap::default());

        assert!(
            if let TypedStatement::Let { val, .. } = &typed_ast.0["main"].inner[0] {
                val.get_type() == Type::I64(Ind::constant(3))
            } else {
                false
            }
        );
    }

    #[test]
    fn inference_error() {
        let code = "
fn dbl{A}(a: A) -> (A, A):
    return (a, a)

fn main():
    let x = dbl{(_, bool)}((5, true))[0][1] || 1
    print x
";

        let ast = parse(code).ast;
        let (_, errors) = typecheck(&ast, &HashMap::default());
        assert_eq!(errors[0].error_type, ErrorType::TypeError);
    }
}
