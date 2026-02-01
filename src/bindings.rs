use crate::parser::ParserOutput;

use super::error::Error;
use super::parser::lexer::{Tok, Token, tokenize};
use super::parser::parse;
use wasm_bindgen::prelude::*;

extern crate console_error_panic_hook;

pub fn serialize_token(t: &Token) -> Option<JsValue> {
    let d = match &t.data {
        Tok::Identifier(_) => 0,
        Tok::Op(_) => 1,
        Tok::AssignOp(_) => 1,
        Tok::Float(_) => 2,
        Tok::Int(_) => 2,
        Tok::Keyword(_) => 3,
        Tok::Error(_) => 4,
        Tok::IndentError(_) => 4,
        Tok::Comment => 5,
        _ => return None,
    };

    let obj = js_sys::Object::new();
    js_sys::Reflect::set(&obj, &JsValue::from("line"), &JsValue::from(t.line)).ok()?;
    js_sys::Reflect::set(&obj, &JsValue::from("start"), &JsValue::from(t.start)).ok()?;
    js_sys::Reflect::set(&obj, &JsValue::from("end"), &JsValue::from(t.end)).ok()?;
    js_sys::Reflect::set(&obj, &JsValue::from("data"), &JsValue::from(d)).ok()?;

    Some(obj.into())
}

pub fn serialize_error(e: &Error, tokens: &[Token]) -> Option<JsValue> {
    let obj = js_sys::Object::new();
    js_sys::Reflect::set(
        &obj,
        &JsValue::from("type"),
        &JsValue::from(e.error_type.to_string()),
    )
    .ok()?;
    js_sys::Reflect::set(
        &obj,
        &JsValue::from("message"),
        &JsValue::from(e.message.clone()),
    )
    .ok()?;
    js_sys::Reflect::set(
        &obj,
        &JsValue::from("start_line"),
        &JsValue::from(tokens[e.start].line),
    )
    .ok()?;
    js_sys::Reflect::set(
        &obj,
        &JsValue::from("start_column"),
        &JsValue::from(tokens[e.start].start),
    )
    .ok()?;
    js_sys::Reflect::set(
        &obj,
        &JsValue::from("end_line"),
        &JsValue::from(tokens[e.end].line),
    )
    .ok()?;
    js_sys::Reflect::set(
        &obj,
        &JsValue::from("end_column"),
        &JsValue::from(tokens[e.end].end),
    )
    .ok()?;
    Some(obj.into())
}

pub fn serialize_type_tokens(c: &ParserOutput) -> Option<JsValue> {
    let mut type_token_pos = vec![];
    for i in 0..c.tokens.len() {
        let t = &c.tokens[i];
        if t.is_type_ann {
            let obj = js_sys::Object::new();
            js_sys::Reflect::set(&obj, &JsValue::from("line"), &JsValue::from(t.line)).ok()?;
            js_sys::Reflect::set(&obj, &JsValue::from("start"), &JsValue::from(t.start)).ok()?;
            if i < c.tokens.len() - 1 && c.tokens[i + 1].is_type_ann {
                js_sys::Reflect::set(
                    &obj,
                    &JsValue::from("end"),
                    &JsValue::from(c.tokens[i + 1].start),
                )
                .ok()?;
            } else {
                js_sys::Reflect::set(&obj, &JsValue::from("end"), &JsValue::from(t.end)).ok()?;
            }
            type_token_pos.push(obj);
        }
    }
    Some(type_token_pos.into())
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    pub fn log(s: &str);
}

#[wasm_bindgen(start)]
fn start() {
    console_error_panic_hook::set_once()
}

#[wasm_bindgen]
pub fn lex(code: &str) -> Vec<JsValue> {
    let tokens = tokenize(code);
    tokens.iter().filter_map(serialize_token).collect()
}

#[wasm_bindgen]
pub fn compile(code: &str) -> JsValue {
    let output = parse(code.to_string());
    let errors = output
        .errors
        .iter()
        .filter_map(|e| serialize_error(e, &output.tokens))
        .collect::<Vec<JsValue>>();
    let type_tokens = serialize_type_tokens(&output);
    let obj = js_sys::Object::new();
    let _ = js_sys::Reflect::set(&obj, &JsValue::from("errors"), &JsValue::from(errors));
    let _ = js_sys::Reflect::set(
        &obj,
        &JsValue::from("type_tokens"),
        &JsValue::from(type_tokens),
    );
    obj.into()
}
