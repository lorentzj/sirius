use super::Type;

pub fn equality_check(lhs: &Type, rhs: &Type) -> Result<(), String> {
    if let Type::Function(_, _, _) = lhs {
        Err("cannot check function equality".into())
    } else if let Type::Function(_, _, _) = rhs {
        Err("cannot check function equality".into())
    } else if lhs == rhs {
        return Ok(());
    } else if matches!(lhs, Type::I64(_)) && matches!(rhs, Type::I64(_)) {
        Ok(())
    } else {
        match lhs {
            Type::Tuple(lhsv) => match rhs {
                Type::Tuple(rhsv) => {
                    if lhsv.len() != rhsv.len() {
                        Err(format!(
                            "cannot check equality between \"{lhs:?}\" and \"{rhs:?}\""
                        ))
                    } else {
                        for (lv, rv) in lhsv.iter().zip(rhsv.iter()) {
                            equality_check(lv, rv)?;
                        }
                        Ok(())
                    }
                }
                _ => Err(format!(
                    "cannot check equality between \"{lhs:?}\" and \"{rhs:?}\""
                )),
            },
            Type::F64 => {
                if let Type::I64 { .. } = rhs {
                    Ok(())
                } else {
                    Err(format!(
                        "cannot check equality between \"{lhs:?}\" and \"{rhs:?}\""
                    ))
                }
            }
            Type::I64 { .. } => {
                if rhs == &Type::F64 {
                    Ok(())
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
