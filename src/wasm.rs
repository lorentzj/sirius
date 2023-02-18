use crate::flow;
use crate::interpreter;
use crate::parser;
use crate::stdlib::stdlib;
use crate::typechecker;

use wasm_bindgen::prelude::*;

extern crate console_error_panic_hook;

#[wasm_bindgen]
pub fn parse(code: String) -> String {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let mut parsed = parser::parse(&code);

    if parsed.errors.is_empty() {
        let (typed_ast, mut typecheck) = typechecker::typecheck(&parsed.ast, &stdlib());
        parsed.errors.append(&mut typecheck);
        parsed
            .annotations
            .extend(typechecker::type_annotations(&typed_ast));

        let mut flowcheck = flow::check_flow(&parsed.ast);
        parsed.errors.append(&mut flowcheck);
    }

    serde_json::to_string(&parsed).unwrap()
}

#[wasm_bindgen]
pub fn interpret(code: String) -> String {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let mut parsed = parser::parse(&code);
    let stdlib = stdlib();

    if parsed.errors.is_empty() {
        let (typed_ast, mut typecheck) = typechecker::typecheck(&parsed.ast, &stdlib);
        parsed.errors.append(&mut typecheck);
        parsed
            .annotations
            .extend(typechecker::type_annotations(&typed_ast));

        let mut flowcheck = flow::check_flow(&parsed.ast);
        parsed.errors.append(&mut flowcheck);

        let output = if parsed.errors.is_empty() {
            interpreter::interpret(typed_ast, &stdlib)
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

#[cfg(test)]
mod tests {
    #[test]
    fn pythagoraas() {
        let code = "
        fn hyp(a: f64, b: f64): f64 {
            return (a^2 + b^2)^0.5;
        }
        
        fn main() {
            print hyp(3., 4.);
        }
        "
        .to_string();

        let output = super::interpret(code);
        assert_eq!(output, "{\"stdout\":\"5\\n\",\"error\":null}");
    }

    #[test]
    fn factorial() {
        let code = "
        fn factorial(x: i64): i64 {
            if x < 1 {
                return 1;
            } else {
                return x * factorial(x - 1);
            }
        }
        
        fn main() {
            print factorial(5);
        }
        "
        .to_string();

        let output = super::interpret(code);
        assert_eq!(output, "{\"stdout\":\"120\\n\",\"error\":null}");
    }

    #[test]
    fn tuple_eq() {
        let code = "
        fn main() {
            let a = (1, 2, (true, 4));
            let b = (-2 + 3, 4 - 2, (false || true, 16/4));
            
            if a == b {
                print true;
            }
        }
        "
        .to_string();

        let output = super::interpret(code);
        assert_eq!(output, "{\"stdout\":\"true\\n\",\"error\":null}");
    }

    #[test]
    fn externals() {
        let code = "
        fn main() {
            let theta = 0.37;
            if sin(theta)^2 + cos(theta)^2 == 1 {
                print true;
            }
        
            print sin, cos, tan, ln, log10;
            print pi;
        }
        "
        .to_string();

        let output = super::interpret(code);
        assert_eq!(output, "{\"stdout\":\"true\\n(<external_function>, <external_function>, <external_function>, <external_function>, <external_function>)\\n3.141592653589793\\n\",\"error\":null}");
    }
}
