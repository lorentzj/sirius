use crate::interpreter;
use crate::parser;
use crate::stdlib::stdlib;
use crate::typechecker;

use wasm_bindgen::prelude::*;

extern crate console_error_panic_hook;

#[wasm_bindgen]
pub fn parse(code: String) -> String {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let mut state = parser::parse(&code);

    typechecker::typecheck(&mut state, stdlib());

    serde_json::to_string(&state).unwrap()
}

#[wasm_bindgen]
pub fn interpret(code: String) -> String {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let mut state = parser::parse(&code);
    let stdlib = stdlib();

    typechecker::typecheck(&mut state, stdlib);

    if state.errors.is_empty() {
        let output = if state.errors.is_empty() {
            interpreter::interpret(state.ast)
        } else {
            interpreter::InterpreterOutput {
                stdout: "".into(),
                value: None,
                error: None,
            }
        };

        serde_json::to_string(&output).unwrap()
    } else {
        let output = interpreter::InterpreterOutput {
            stdout: "".into(),
            value: None,
            error: None,
        };
        serde_json::to_string(&output).unwrap()
    }
}
