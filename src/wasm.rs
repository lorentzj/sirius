use crate::interpreter;
use crate::parser;
use crate::typechecker;

use wasm_bindgen::prelude::*;

extern crate console_error_panic_hook;

#[wasm_bindgen]
pub fn parse(code: String) -> String {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let mut parsed = parser::parse(&code);
    let mut typechecker_output = typechecker::typecheck(&parsed.ast);
    parsed.errors.append(&mut typechecker_output);

    serde_json::to_string(&parsed).unwrap()
}

#[wasm_bindgen]
pub fn interpret(code: String) -> String {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let mut parsed = parser::parse(&code);
    let mut typecheck_output = typechecker::typecheck(&parsed.ast);
    parsed.errors.append(&mut typecheck_output);

    let output = interpreter::interpret(&parsed.ast);
    serde_json::to_string(&output).unwrap()
}
