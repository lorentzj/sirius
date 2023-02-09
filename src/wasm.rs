use crate::interpreter;
use crate::parser;
use crate::typechecker;

use wasm_bindgen::prelude::*;

extern crate console_error_panic_hook;

#[wasm_bindgen]
pub fn parse(code: String) -> String {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let mut parsed = parser::parse(&code);
    let mut typecheck = typechecker::typecheck(&parsed.ast);
    parsed.errors.append(&mut typecheck);

    serde_json::to_string(&parsed).unwrap()
}

#[wasm_bindgen]
pub fn interpret(code: String) -> String {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let mut parsed = parser::parse(&code);
    let mut typecheck = typechecker::typecheck(&parsed.ast);
    parsed.errors.append(&mut typecheck);

    let output = interpreter::interpret(&parsed);

    serde_json::to_string(&output).unwrap()
}
