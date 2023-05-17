use std::collections::HashMap;

use super::Value;

fn sin(v: Vec<Value>) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.sin()))
    } else {
        unreachable!()
    }
}

fn cos(v: Vec<Value>) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.cos()))
    } else {
        unreachable!()
    }
}

fn tan(v: Vec<Value>) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.tan()))
    } else {
        unreachable!()
    }
}

fn log10(v: Vec<Value>) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.log10()))
    } else {
        unreachable!()
    }
}

fn ln(v: Vec<Value>) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::F64(v.ln()))
    } else {
        unreachable!()
    }
}

fn round(v: Vec<Value>) -> Option<Value> {
    if let Some(Value::F64(v)) = v.first() {
        Some(Value::I64(v.round() as i64))
    } else {
        unreachable!()
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
    lib.insert("round".into(), Value::ExternalFunction(round));

    lib
}
