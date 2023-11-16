use super::{Constraint, Type};

pub fn equality_check(
    start: usize,
    lhs: &Type,
    rhs: &Type,
    end: usize,
) -> Result<Option<Vec<Constraint>>, String> {
    if let Type::Function(_, _, _) = lhs {
        Err("cannot check function equality".into())
    } else if let Type::Function(_, _, _) = rhs {
        Err("cannot check function equality".into())
    } else if matches!(lhs, Type::I64(_)) && matches!(rhs, Type::I64(_)) {
        match (lhs, rhs) {
            (Type::I64(None), _) => Ok(None),
            (_, Type::I64(None)) => Ok(None),
            (Type::I64(Some(a)), Type::I64(Some(b))) => {
                let mut c = Constraint::new_eq_z(a.clone() - b.clone());
                c.start = start;
                c.end = end;
                Ok(Some(vec![c]))
            }
            _ => unreachable!(),
        }
    } else if lhs == rhs || lhs == &Type::Unknown || rhs == &Type::Unknown {
        return Ok(None);
    } else {
        match lhs {
            Type::Tuple(lhsv) => match rhs {
                Type::Tuple(rhsv) => {
                    if lhsv.len() != rhsv.len() {
                        Err("cannot check equality between tuples of different length".to_string())
                    } else {
                        let mut all_cs = vec![];
                        for (lv, rv) in lhsv.iter().zip(rhsv.iter()) {
                            if let Some(mut cs) = equality_check(start, lv, rv, end)? {
                                all_cs.append(&mut cs)
                            }
                        }
                        if all_cs.is_empty() {
                            Ok(None)
                        } else {
                            Ok(Some(all_cs))
                        }
                    }
                }
                _ => Err(format!(
                    "cannot check equality between \"{lhs:?}\" and \"{rhs:?}\""
                )),
            },
            Type::F64 => {
                if let Type::I64 { .. } = rhs {
                    Ok(None)
                } else {
                    Err(format!(
                        "cannot check equality between \"{lhs:?}\" and \"{rhs:?}\""
                    ))
                }
            }
            Type::I64 { .. } => {
                if rhs == &Type::F64 {
                    Ok(None)
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
