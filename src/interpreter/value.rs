use super::ExternalFunctionPointer;
use crate::parser::Statement;

#[derive(Clone)]
pub enum Value {
    F64(f64),
    I64(i64),
    Bool(bool),
    Tuple(Vec<Value>),
    Array(usize, Vec<Value>),
    Function(Vec<String>, Vec<Statement>),
    ExternalFunction(ExternalFunctionPointer),
}

pub fn print_value(value: Value) -> String {
    match value {
        Value::F64(val) => val.to_string(),
        Value::I64(val) => val.to_string(),
        Value::Bool(val) => val.to_string(),
        Value::Tuple(v) => {
            if v.is_empty() {
                "()".into()
            } else {
                let mut out = "(".to_string();
                for val in v {
                    out.push_str(&print_value(val));
                    out.push(',');
                    out.push(' ');
                }
                out.pop();
                out.pop();
                out.push(')');
                out
            }
        }
        Value::Array(_, v) => {
            if v.is_empty() {
                "[]".into()
            } else {
                let mut out = "[".to_string();
                for val in v {
                    out.push_str(&print_value(val));
                    out.push(',');
                    out.push(' ');
                }
                out.pop();
                out.pop();
                out.push(']');
                out
            }
        }
        Value::Function { .. } => "<function>".into(),
        Value::ExternalFunction { .. } => "<external_function>".into(),
    }
}
