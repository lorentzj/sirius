// use crate::lexer::Op;
// use crate::parser::{Function, Statement, S, Expression, E, Positioned, UnaryOp};
// use crate::error::{Error, ErrorType};
// use crate::scope::Scope;

// use super::{Type, Ind};

// fn apply_ann(ann: &Type, t: &Type) -> Result<Type, String> {
//     match ann {
//         Type::Tuple(inner_anns) => {
//             if let Type::Tuple(inner_t) = t {
//                 let mut new_ts = vec![];
//                 for (ann, t) in inner_anns.iter().zip(inner_t) {
//                     new_ts.push(apply_ann(&ann, &t)?);
//                 }

//                 Ok(Type::Tuple(new_ts))
//             } else {
//                 panic!()
//             }
//         }

//         Type::I64(Some(ann_val)) => {
//             if let Type::I64(Some(t_val)) = t {
//                 if ann_val == t_val {
//                     let mut r_val = t_val.clone();
//                     r_val.strict = true;
//                     Ok(Type::I64(Some(r_val)))
//                 } else {
//                     Ok(t.clone())
//                 }
//             } else {
//                 Err(format!("annotation requires strict value \"{ann_val:?}\""))
//             }
//         },
//         _ => Ok(t.clone())
//     }
// }

// // fn value_check_expr(expr: &mut Expression, scope: &Scope<Type>) -> Result<(), Error> {
// //     match &mut expr.data {
// //         E::Accessor(_, _) => panic!(),
// //         E::BinaryOp(lhs, op, rhs) => {
// //             value_check_expr(lhs, scope)?;
// //             value_check_expr(lhs, scope)?;

// //             if let (Type::I64(Some(l_val)), Type::I64(Some(r_val))) = (&lhs.t, &rhs.t) {
// //                 match op {
// //                     Op::Add => { expr.t = Type::I64(Some(l_val.clone() + r_val.clone())); }
// //                     Op::Sub => { expr.t = Type::I64(Some(l_val.clone() - r_val.clone())); }
// //                     Op::Mul => { expr.t = Type::I64(Some(l_val.clone() * r_val.clone())); }
// //                     _ => ()
// //                 }
// //             }

// //             Ok(())
// //         },
// //         E::UnaryOp(op, inner) => {
// //             value_check_expr(inner, scope)?;
// //             if let Type::I64(Some(inner_val)) = &inner.t {
// //                 if let UnaryOp::ArithNeg = op {
// //                     expr.t = Type::I64(Some(Ind::constant(-1) * inner_val.clone()))
// //                 }
// //             }

// //             Ok(())
// //         },
// //         E::Ident(name, type_params) => {

// //         }
// //         _ => Ok(())
// //     }
// // }

// fn value_check_block(block: &mut[Statement], scope: &mut Scope<Type>) -> Vec<Error> {
//     scope.push();

//     let mut errors = vec![];

//     for statement in block {
//         match &mut statement.data {
//             S::Let(name, ann, val) => {
//                 if let Some(Positioned { inner: ann, .. }) = ann {

//                 }
//             },
//             _ => ()
//         }
//     }

//     scope.pop();

//     errors
// }

// pub fn value_check(f: &mut Function, scope: &mut Scope<Type>) -> Vec<Error> {
//     value_check_block(&mut f.body, scope)
// }
