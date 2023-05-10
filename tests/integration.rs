extern crate sirius;

use sirius::wasm;

#[test]
fn pythagoras() {
    let code = "
fn hyp(a: f64, b: f64) -> f64:
    return (a^2 + b^2)^0.5

fn main():
    print hyp(3., 4.)"
        .into();

    let output = wasm::interpret(code);
    assert_eq!(output, "{\"stdout\":\"5\\n\",\"error\":null}");
}

#[test]
fn factorial() {
    let code = "
fn factorial(x: i64) -> i64:
    if x < 1:
        return 1
    else:
        return x * factorial(x - 1)

fn main():
    print factorial(5)"
        .into();

    let output = wasm::interpret(code);
    assert_eq!(output, "{\"stdout\":\"120\\n\",\"error\":null}");
}

#[test]
fn tuple_eq() {
    let code = "
fn main():
    let a = (1, 2, (true, 4.))
    let b = (-2 + 3, 4 - 2, (false || true, 16.0/4))

    if a == b:
        print true

    if a.2.1 == b.0 + 3:
        print true"
        .into();

    let output = wasm::interpret(code);
    assert_eq!(output, "{\"stdout\":\"true\\ntrue\\n\",\"error\":null}");
}

#[test]
fn externals() {
    let code = "
fn main():
    let theta = 0.37
    if sin(theta)^2 + cos(theta)^2 == 1:
        print true

    print sin, cos, tan, ln, log10
    print pi"
        .into();

    let output = wasm::interpret(code);
    assert_eq!(output, "{\"stdout\":\"true\\n(<external_function>, <external_function>, <external_function>, <external_function>, <external_function>)\\n3.141592653589793\\n\",\"error\":null}");
}

#[test]
fn coercion() {
    let code = "
fn main():
    let a = (1, 1, 1)
    let b = (1., 1., 1.)
    print a == b"
        .into();

    let output = wasm::interpret(code);
    assert_eq!(output, "{\"stdout\":\"true\\n\",\"error\":null}");
}

#[test]
fn generic() {
    let code = "
fn double{T}(t: T) -> (T, T):
    return (t, t)

fn main():
    print double(1.)
    print double((false, true))
"
    .into();

    let output = wasm::interpret(code);
    assert_eq!(
        output,
        "{\"stdout\":\"(1, 1)\\n((false, true), (false, true))\\n\",\"error\":null}"
    );
}

#[test]
fn scope_test() {
    let code = "
fn main():
    let i = true
    if true:
        print i
        if i:
            let i = (1, 2)
            print i
        
        print i

    for i from 1 to 3 + 1:
        print i"
        .into();

    let output = wasm::interpret(code);
    assert_eq!(
        output,
        "{\"stdout\":\"true\\n(1, 2)\\ntrue\\n1\\n2\\n3\\n\",\"error\":null}"
    );
}
