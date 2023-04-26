use std::collections::HashMap;

use super::Value;

fn sin(v: Vec<Value>) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.sin()))
    } else {
        panic!()
    }
}

fn cos(v: Vec<Value>) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.cos()))
    } else {
        panic!()
    }
}

fn tan(v: Vec<Value>) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.tan()))
    } else {
        panic!()
    }
}

fn log10(v: Vec<Value>) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.log10()))
    } else {
        panic!()
    }
}

fn ln(v: Vec<Value>) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.ln()))
    } else {
        panic!()
    }
}

pub fn stdlib<'a>() -> HashMap<String, Value<'a>> {
    let mut lib = HashMap::<String, Value>::default();

    lib.insert("pi".into(), Value::F64(std::f64::consts::PI));
    lib.insert("sin".into(), Value::ExternalFunction(sin));
    lib.insert("cos".into(), Value::ExternalFunction(cos));
    lib.insert("tan".into(), Value::ExternalFunction(tan));
    lib.insert("log10".into(), Value::ExternalFunction(log10));
    lib.insert("ln".into(), Value::ExternalFunction(ln));

    lib
}
