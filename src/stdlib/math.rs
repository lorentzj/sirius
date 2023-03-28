use std::collections::HashMap;

use crate::interpreter::Value;
use crate::typechecker::Type;

fn sin(v: &[Value]) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.sin()))
    } else {
        panic!()
    }
}

fn cos(v: &[Value]) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.cos()))
    } else {
        panic!()
    }
}

fn tan(v: &[Value]) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.tan()))
    } else {
        panic!()
    }
}

fn log10(v: &[Value]) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.log10()))
    } else {
        panic!()
    }
}

fn ln(v: &[Value]) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.ln()))
    } else {
        panic!()
    }
}

pub fn math_mod() -> HashMap<String, (Type, Value)> {
    let mut vals = HashMap::default();

    vals.insert("pi".into(), (Type::F64, Value::F64(std::f64::consts::PI)));
    vals.insert(
        "sin".into(),
        (
            Type::Function(vec![], vec![Type::F64], Box::new(Type::F64)),
            Value::ExternalFunction(sin),
        ),
    );

    vals.insert(
        "cos".into(),
        (
            Type::Function(vec![], vec![Type::F64], Box::new(Type::F64)),
            Value::ExternalFunction(cos),
        ),
    );

    vals.insert(
        "tan".into(),
        (
            Type::Function(vec![], vec![Type::F64], Box::new(Type::F64)),
            Value::ExternalFunction(tan),
        ),
    );

    vals.insert(
        "log10".into(),
        (
            Type::Function(vec![], vec![Type::F64], Box::new(Type::F64)),
            Value::ExternalFunction(log10),
        ),
    );

    vals.insert(
        "ln".into(),
        (
            Type::Function(vec![], vec![Type::F64], Box::new(Type::F64)),
            Value::ExternalFunction(ln),
        ),
    );

    vals
}
