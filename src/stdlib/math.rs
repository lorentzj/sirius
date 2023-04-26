use std::collections::HashMap;

use crate::typechecker::Type;

pub fn math_mod() -> HashMap<String, Type> {
    let mut vals = HashMap::default();

    vals.insert("pi".into(), Type::F64);
    vals.insert(
        "sin".into(),
        Type::Function(vec![], vec![Type::F64], Box::new(Type::F64)),
    );
    vals.insert(
        "cos".into(),
        Type::Function(vec![], vec![Type::F64], Box::new(Type::F64)),
    );
    vals.insert(
        "tan".into(),
        Type::Function(vec![], vec![Type::F64], Box::new(Type::F64)),
    );
    vals.insert(
        "log10".into(),
        Type::Function(vec![], vec![Type::F64], Box::new(Type::F64)),
    );
    vals.insert(
        "ln".into(),
        Type::Function(vec![], vec![Type::F64], Box::new(Type::F64)),
    );

    vals
}
