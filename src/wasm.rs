use std::collections::HashSet;

use crate::interpreter;
use crate::parser;
use crate::typechecker;

use wasm_bindgen::prelude::*;

extern crate console_error_panic_hook;

#[wasm_bindgen]
pub fn parse(code: String) -> String {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let mut parsed = parser::parse(&code);
    let mut global_context = typechecker::Context::new();
    let mut typecheck = typechecker::typecheck(&parsed.ast, &mut global_context);
    parsed.errors.append(&mut typecheck);

    serde_json::to_string(&parsed).unwrap()
}

#[wasm_bindgen]
pub fn interpret(code: String) -> String {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let mut parsed = parser::parse(&code);
    let mut global_context = typechecker::Context::new();
    let mut typecheck = typechecker::typecheck(&parsed.ast, &mut global_context);
    parsed.errors.append(&mut typecheck);

    let output = if parsed.errors.is_empty() {
        let mut global_context = interpreter::Context::new();
        interpreter::interpret(&parsed.ast, &mut global_context)
    } else {
        interpreter::InterpreterOutput {
            output: "".into(),
            error: None,
            defined_idents: HashSet::new(),
        }
    };

    serde_json::to_string(&output).unwrap()
}
