use std::collections::HashMap;

use super::{Type, TypedAST, TypedExpression, TypedStatement};

pub fn type_annotations(ast: &TypedAST) -> HashMap<usize, String> {
    let mut annotations = HashMap::default();

    fn annotations_from_expression(expr: TypedExpression) -> HashMap<usize, String> {
        let mut annotations = HashMap::default();

        match expr {
            TypedExpression::Identifier { t, start, .. } => {
                annotations.insert(start, format!("{t:?}"));
            }
            TypedExpression::Bool { start, .. } => {
                annotations.insert(start, format!("{:?}", Type::Bool));
            }
            TypedExpression::F64 { start, .. } => {
                annotations.insert(start, format!("{:?}", Type::F64));
            }
            TypedExpression::I64 { start, val, .. } => {
                if val >= 0 && val <= usize::MAX as i64 {
                    annotations.insert(start, format!("i64(nat={val})"));
                } else {
                    annotations.insert(start, "i64".into());
                }
            }
            TypedExpression::BinaryOp { lhs, rhs, .. } => {
                annotations.extend(annotations_from_expression(*lhs));
                annotations.extend(annotations_from_expression(*rhs));
            }
            TypedExpression::UnaryOp { inner, .. } => {
                annotations.extend(annotations_from_expression(*inner));
            }
            TypedExpression::Accessor { lhs, rhs, .. } => {
                annotations.extend(annotations_from_expression(*lhs));
                annotations.extend(annotations_from_expression(*rhs));
            }
            TypedExpression::FnCall { caller, args, .. } => {
                annotations.extend(annotations_from_expression(*caller));
                for arg in args {
                    annotations.extend(annotations_from_expression(arg));
                }
            }
            TypedExpression::Tuple { inner, .. } => {
                for v in inner {
                    annotations.extend(annotations_from_expression(v));
                }
            }
        };

        annotations
    }

    fn annotations_from_block(block: Vec<TypedStatement>) -> HashMap<usize, String> {
        let mut annotations = HashMap::default();

        for statement in block {
            match statement {
                TypedStatement::Let { val, .. } => {
                    annotations.extend(annotations_from_expression(val))
                }
                TypedStatement::Print { val, .. } => {
                    annotations.extend(annotations_from_expression(val))
                }
                TypedStatement::Return { val, .. } => {
                    if let Some(val) = val {
                        annotations.extend(annotations_from_expression(val));
                    }
                }
                TypedStatement::If {
                    cond,
                    true_inner,
                    false_inner,
                    ..
                } => {
                    annotations.extend(annotations_from_expression(cond));
                    annotations.extend(annotations_from_block(true_inner));
                    if let Some(false_inner) = false_inner {
                        annotations.extend(annotations_from_block(false_inner));
                    }
                }
            }
        }

        annotations
    }

    for function in ast.values() {
        annotations.extend(annotations_from_block(function.inner.clone()))
    }

    annotations
}
