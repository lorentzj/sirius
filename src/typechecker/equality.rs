use super::{Constraint, Type};

pub fn equality_check(
    start: usize,
    lhs: &Type,
    rhs: &Type,
    end: usize,
) -> Result<Vec<Constraint>, String> {
    if let Type::Function(_, _, _) = lhs {
        Err("cannot check function equality".into())
    } else if let Type::Function(_, _, _) = rhs {
        Err("cannot check function equality".into())
    } else if matches!(lhs, Type::I64(_)) && matches!(rhs, Type::I64(_)) {
        match (lhs, rhs) {
            (Type::I64(None), _) => Ok(vec![]),
            (_, Type::I64(None)) => Ok(vec![]),
            (Type::I64(Some(a)), Type::I64(Some(b))) => {
                let mut c = Constraint::new_eq(a.clone(), b.clone());
                c.start = start;
                c.end = end;
                Ok(vec![c])
            }
            _ => unreachable!(),
        }
    } else if lhs == rhs || lhs == &Type::Unknown || rhs == &Type::Unknown {
        return Ok(vec![]);
    } else {
        match lhs {
            Type::Tuple(lhsv) => match rhs {
                Type::Tuple(rhsv) => {
                    if lhsv.len() != rhsv.len() {
                        Err("cannot check equality between tuples of different length".to_string())
                    } else {
                        let mut all_cs = vec![];
                        for (lv, rv) in lhsv.iter().zip(rhsv.iter()) {
                            all_cs.append(&mut equality_check(start, lv, rv, end)?)
                        }
                        if all_cs.is_empty() {
                            Ok(vec![])
                        } else {
                            Ok(all_cs)
                        }
                    }
                }
                _ => Err(format!(
                    "cannot check equality between \"{lhs:?}\" and \"{rhs:?}\""
                )),
            },
            Type::F64 => {
                if let Type::I64 { .. } = rhs {
                    Ok(vec![])
                } else {
                    Err(format!(
                        "cannot check equality between \"{lhs:?}\" and \"{rhs:?}\""
                    ))
                }
            }
            Type::I64 { .. } => {
                if rhs == &Type::F64 {
                    Ok(vec![])
                } else {
                    Err(format!(
                        "cannot check equality between \"{lhs:?}\" and \"{rhs:?}\""
                    ))
                }
            }
            _ => Err(format!(
                "cannot check equality between \"{lhs:?}\" and \"{rhs:?}\""
            )),
        }
    }
}
