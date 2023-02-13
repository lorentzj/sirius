use std::collections::HashMap;

use super::ExternalFunctionPointer;
use crate::interpreter::Value;
use crate::typechecker::Type;

fn sin(v: &[Value]) -> Option<Value> {
    if let Some(Value::Float(v)) = v.first() {
        Some(Value::Float(v.sin()))
    } else {
        panic!()
    }
}

fn cos(v: &[Value]) -> Option<Value> {
    if let Some(Value::Float(v)) = v.first() {
        Some(Value::Float(v.cos()))
    } else {
        panic!()
    }
}

fn tan(v: &[Value]) -> Option<Value> {
    if let Some(Value::Float(v)) = v.first() {
        Some(Value::Float(v.tan()))
    } else {
        panic!()
    }
}

fn log10(v: &[Value]) -> Option<Value> {
    if let Some(Value::Float(v)) = v.first() {
        Some(Value::Float(v.log10()))
    } else {
        panic!()
    }
}

fn ln(v: &[Value]) -> Option<Value> {
    if let Some(Value::Float(v)) = v.first() {
        Some(Value::Float(v.ln()))
    } else {
        panic!()
    }
}

pub fn math_constants() -> HashMap<String, (Type, Value)> {
    let mut constants: HashMap<String, (Type, Value)> = HashMap::default();

    constants.insert("pi".into(), (Type::F64, Value::Float(std::f64::consts::PI)));

    constants
}

pub fn math_functions() -> HashMap<String, (Type, ExternalFunctionPointer)> {
    let mut functions: HashMap<String, (Type, ExternalFunctionPointer)> = HashMap::default();

    functions.insert(
        "sin".into(),
        (
            Type::Function(vec![Type::F64], Some(Box::new(Type::F64))),
            sin,
        ),
    );

    functions.insert(
        "cos".into(),
        (
            Type::Function(vec![Type::F64], Some(Box::new(Type::F64))),
            cos,
        ),
    );

    functions.insert(
        "tan".into(),
        (
            Type::Function(vec![Type::F64], Some(Box::new(Type::F64))),
            tan,
        ),
    );

    functions.insert(
        "log10".into(),
        (
            Type::Function(vec![Type::F64], Some(Box::new(Type::F64))),
            log10,
        ),
    );

    functions.insert(
        "ln".into(),
        (
            Type::Function(vec![Type::F64], Some(Box::new(Type::F64))),
            ln,
        ),
    );

    functions
}
