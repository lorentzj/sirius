use crate::interpreter;
use crate::parser;
use wasm_bindgen::prelude::*;

extern crate console_error_panic_hook;

#[wasm_bindgen]
pub fn parse(code: String) -> String {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let parsed = parser::parse(&code);
    serde_json::to_string(&parsed).unwrap_or_else(|_| "Serialization Error".to_string())
}

#[wasm_bindgen]
pub fn interpret(code: String) -> String {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let parsed = parser::parse(&code);
    let output = interpreter::interpret(parsed.statements);
    serde_json::to_string(&output).unwrap_or_else(|_| "Serialization Error".to_string())
}
