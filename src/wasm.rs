use crate::flow;
use crate::interpreter;
use crate::parser;
use crate::typechecker;
use wasm_bindgen::prelude::*;

extern crate console_error_panic_hook;

#[wasm_bindgen]
pub fn parse(code: String) -> String {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let mut parsed = parser::parse(&code);

    if parsed.errors.is_empty() {
        let mut typecheck = typechecker::typecheck(&parsed.ast);
        parsed.errors.append(&mut typecheck);

        let mut flowcheck = flow::check_flow(&parsed.ast);
        parsed.errors.append(&mut flowcheck);
    }

    serde_json::to_string(&parsed).unwrap()
}

#[wasm_bindgen]
pub fn interpret(code: String) -> String {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let mut parsed = parser::parse(&code);

    if parsed.errors.is_empty() {
        let mut typecheck = typechecker::typecheck(&parsed.ast);
        parsed.errors.append(&mut typecheck);

        let mut flowcheck = flow::check_flow(&parsed.ast);
        parsed.errors.append(&mut flowcheck);
    }

    let output = if parsed.errors.is_empty() {
        interpreter::interpret(&parsed.ast)
    } else {
        interpreter::InterpreterOutput {
            stdout: "".into(),
            value: None,
            error: None,
        }
    };

    serde_json::to_string(&output).unwrap()
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
            print hyp(3, 4);
        }
        "
        .to_string();

        let output = super::interpret(code);
        assert_eq!(output, "{\"stdout\":\"5\\n\",\"error\":null}");
    }

    #[test]
    fn factorial() {
        let code = "
        fn factorial(x: f64): f64 {
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
            let b = (5^0, 4 - 2, (false | true, 16^0.5));
            
            if a == b {
                print true;
            }
        }
        "
        .to_string();

        let output = super::interpret(code);
        assert_eq!(output, "{\"stdout\":\"true\\n\",\"error\":null}");
    }
}
