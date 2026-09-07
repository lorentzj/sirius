use crate::error::Errors;
use crate::parser::ParserOutput;
use crate::solver::Solver;

use super::check::check_program;
use super::typed::TypedAst;

fn check(src: &str) -> (TypedAst, Errors) {
    let parse = ParserOutput::parse(src);
    assert!(parse.errors.is_empty(), "parse errors: {:?}", parse.errors);
    let mut solver = Solver::new_cli(None).expect("z3 must be on PATH");
    check_program(parse.tree.as_ref().expect("no parse tree"), &mut solver)
}

fn expect_ok(src: &str) -> TypedAst {
    let (ast, errors) = check(src);
    assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    ast
}

fn expect_error(src: &str) -> String {
    let (_, errors) = check(src);
    assert_eq!(errors.len(), 1, "expected one error, got {:?}", errors);
    errors[0].data.message.clone()
}

const DOT: &str = "
fn dot{N}(a: f32[N], b: f32[N]) -> f32:
    let mut sum = 0.0
    for i from 0 to N:
        sum += a[i] * b[i]
    return sum
";

#[test]
fn dot_product() {
    let ast = expect_ok(DOT);
    let dot = ast.get("dot").unwrap();

    // block 0 is the body, block 1 the `for`
    assert_eq!(dot.render_var(0, "a").unwrap(), "f32[N]");
    assert_eq!(dot.render_var(0, "N").unwrap(), "N");
    assert_eq!(dot.render_var(0, "sum").unwrap(), "f32");
    assert_eq!(dot.render_var(1, "i").unwrap(), "i");
    assert_eq!(dot.render_var(1, "sum").unwrap(), "f32");
    assert!(dot.render_var(0, "i").is_none());

    // the loop contributes `0 <= i` and `i < N`
    assert_eq!(dot.constraints(1).len(), 2);
    assert!(dot.yields.is_zero());
}

#[test]
fn out_of_bounds_access() {
    let src = DOT.replace("a[i]", "a[i + 1]");
    let msg = expect_error(&src);
    assert!(msg.starts_with("index: disproved \"i + 1 < N\""), "{msg}");
    assert!(msg.contains("counterexample"), "{msg}");
}

#[test]
fn matmul_yield_count() {
    let src = format!(
        "{DOT}
fn matmul{{I, J, K}}(a: f32[I, J], b: f32[J, K]) -> f32[I, K]:
    for i from 0 to I:
        for k from 0 to K:
            yield dot(a[i], b'[k])
"
    );
    let ast = expect_ok(&src);
    let matmul = ast.get("matmul").unwrap();
    assert_eq!(
        matmul.yields.num().display_with(Some(&matmul.names())),
        "I*K"
    );
    assert_eq!(matmul.yields.den(), 1.into());

    // one yield too many
    let extra = src.replace(
        "            yield dot(a[i], b'[k])",
        "            yield dot(a[i], b'[k])\n            yield 0.0",
    );
    let msg = expect_error(&extra);
    assert!(
        msg.starts_with("yield count of \"matmul\": cannot prove"),
        "{msg}"
    );
}

#[test]
fn yields_are_counted_symbolically() {
    let ast = expect_ok(
        "
fn concat{A, B}(a: f32[A], b: f32[B]) -> f32[A + B]:
    yield from a
    yield from b

fn push{N}(arr: f32[N], item: f32) -> f32[N + 1]:
    yield from arr
    yield item

fn triangle{N}() -> f32[N^2 - N + 1]:
    for i from 0 to N:
        for j from 0 to i:
            yield 0.0
            yield 1.0
    yield 2.0
",
    );

    let triangle = ast.get("triangle").unwrap();
    let names = triangle.names();
    assert_eq!(
        triangle.yields.num().display_with(Some(&names)),
        "N^2 - N + 1"
    );
}

const FILL: &str = "
fn fill{N}(val: f32) -> f32[N]:
    for i from 0 to N:
        yield val
";

#[test]
fn typevars_inferred_from_context() {
    // `dot` fixes N = 3 from `x`, which in turn fixes `fill`'s N through the expected type
    let ast = expect_ok(&format!(
        "{DOT}{FILL}
fn test():
    let x = [1.0, 2.0, 3.0]
    let y = dot(x, fill(2))
"
    ));

    let test = ast.get("test").unwrap();
    assert_eq!(test.render_var(0, "x").unwrap(), "f32[3]");
    assert_eq!(test.render_var(0, "y").unwrap(), "f32");
}

#[test]
fn explicit_typevar_conflicts() {
    let msg = expect_error(&format!(
        "{DOT}{FILL}
fn test():
    let x = [1.0, 2.0, 3.0]
    let z = dot(x, fill{{4}}(2))
"
    ));
    assert!(
        msg.starts_with("argument \"b\" of \"dot\": expected \"f32[3]\", found \"f32[4]\""),
        "{msg}"
    );
}

#[test]
fn uninferrable_typevar() {
    let msg = expect_error(&format!(
        "{FILL}
fn test():
    let x = fill(1.0)
"
    ));
    assert!(
        msg.starts_with("cannot infer typevar \"N\" of \"fill\""),
        "{msg}"
    );
}

#[test]
fn flow_facts_from_conditions() {
    let guarded = "
fn shift{N}(a: f32[N]) -> f32:
    let mut sum = 0.0
    for i from 0 to N:
        if i < N - 1:
            sum += a[i + 1]
    return sum
";
    expect_ok(guarded);

    let msg = expect_error(&guarded.replace("if i < N - 1:", "if i < N:"));
    assert!(msg.starts_with("index: disproved \"i + 1 < N\""), "{msg}");
}

#[test]
fn find() {
    let guarded_nullable = "
fn find{N}(needle: f32, haystack: f32[N]) -> Ind(N)?:
    for i from 0 to N:
        if needle == haystack[i]:
            # 'Ind(N)' constraint is proven here
            return i
    # Ind(N)? is nullable Ind(N)
    return null

fn test():
    let mut arr = [1.0, 2.0, 3.0]
    let k = find(2.0, arr)
    if k != null:
        # find() constraint is available at call sites
        # so this access is proven safe
        arr[k] += 1.0";

    expect_ok(guarded_nullable);

    let msg = expect_error(&guarded_nullable.replace("k != null", "k == null"));
    assert!(msg.starts_with("cannot index with \"null\""), "{msg}");
}

#[test]
fn index_types_cross_call_boundaries() {
    let ast = expect_ok(
        "
fn first{N}(a: f32[N + 1]) -> Ind(N + 1):
    return 0

fn test{M}(a: f32[M + 1]) -> f32:
    return a[first(a)]
",
    );
    let first = ast.get("first").unwrap();
    assert_eq!(first.sig.ret.render(&first.names()), "Ind(N + 1)");

    // an index into a shorter array is not safe
    let msg = expect_error(
        "
fn first{N}(a: f32[N + 1]) -> Ind(N + 1):
    return 0

fn test{M}(a: f32[M + 1], b: f32[M]) -> f32:
    return b[first(a)]
",
    );
    assert!(msg.starts_with("index: disproved \"M + 1 <= M\""), "{msg}");
}

#[test]
fn typevar_constraints() {
    let src = "
fn get{N, K, st K < N}(a: f32[N]) -> f32:
    return a[K]

fn test():
    let a = [1.0, 2.0, 3.0]
    let x = get{3, 2}(a)
";
    expect_ok(src);

    let msg = expect_error(&src.replace("get{3, 2}(a)", "get{3, 3}(a)"));
    assert!(
        msg.starts_with("constraint of \"get\": disproved \"3 < 3\""),
        "{msg}"
    );
}

#[test]
fn mutable_bindings_lose_static_values() {
    let ast = expect_ok(
        "
fn test():
    let a = 3
    let mut b = 3
    print a
    print b
",
    );
    let test = ast.get("test").unwrap();
    assert_eq!(test.render_var(0, "a").unwrap(), "3");
    assert_eq!(test.render_var(0, "b").unwrap(), "i64");

    let msg = expect_error(
        "
fn test():
    let a = [1.0, 2.0]
    a[0] = 3.0
",
    );
    assert!(msg.starts_with("\"a\" is immutable"), "{msg}");
}

#[test]
fn conditional_yields_need_existentials() {
    let msg = expect_error(
        "
fn evens{N}(a: f32[N]) -> f32[N]:
    for i from 0 to N:
        if a[i] > 0.0:
            yield a[i]
",
    );
    assert!(msg.contains("existential sizes"), "{msg}");
}

#[test]
fn recursion_is_rejected() {
    let msg = expect_error(
        "
fn loop{N}(a: f32[N]) -> f32:
    return loop(a)
",
    );
    assert!(msg.starts_with("recursive call to \"loop\""), "{msg}");
}

#[test]
fn tuples_and_shapes() {
    let ast = expect_ok(
        "
fn test{N}(a: f32[N + 1, 2]) -> f32:
    let s = a.shape
    let pair = (1, a.len)
    print s.0
    return a[s.0 - N - 1, pair.0]
",
    );
    let test = ast.get("test").unwrap();
    assert_eq!(test.render_var(0, "s").unwrap(), "(N + 1, 2)");
    assert_eq!(test.render_var(0, "pair").unwrap(), "(1, N + 1)");
}

#[test]
fn call_sites_check_shapes() {
    let src = "
fn matmul{I, J, K}(a: f32[I, J], b: f32[J, K]) -> f32[I, K]:
    for i from 0 to I:
        for k from 0 to K:
            yield 0.0

fn test(a: f32[2, 3], b: f32[3, 4]) -> f32[2, 4]:
    return matmul(a, b)
";
    expect_ok(src);

    let msg = expect_error(&src.replace("b: f32[3, 4]", "b: f32[2, 4]"));
    assert!(
        msg.starts_with(
            "argument \"b\" of \"matmul\": expected \"f32[3, 4]\", found \"f32[2, 4]\""
        ),
        "{msg}"
    );
}

#[test]
fn typevars_solved_across_arguments() {
    // `A + B` has two unknowns until `y` fixes `A`, so it takes a second pass
    let ast = expect_ok(
        "
fn split{A, B}(x: f32[A + B], y: f32[A]) -> f32[B]:
    for i from 0 to B:
        yield x[i]

fn test(u: f32[5], v: f32[2]) -> f32[3]:
    return split(u, v)
",
    );
    let test = ast.get("test").unwrap();
    // the call's result type is the instantiated return type
    let call = test
        .exprs
        .values()
        .find(|t| matches!(t, super::Type::Array { .. }));
    assert!(call.is_some());
}

#[test]
fn flow_control_reaches_every_path() {
    expect_ok(
        "
fn f{N}(a: f32[N]) -> f32:
    if N > 2:
        if N > 3:
            return 1.0
        else:
            return 2.0
    else:
        return 3.0
",
    );

    // a loop body may never run, so it cannot discharge the return
    let msg = expect_error(
        "
fn f{N}(a: f32[N]) -> f32:
    for i from 0 to N:
        return a[i]
",
    );
    assert!(
        msg.starts_with("\"f\" must return \"f32\" on every path"),
        "{msg}"
    );
}

#[test]
fn intro_page_programs() {
    let start = std::time::Instant::now();
    let ast = expect_ok(
        "
fn dot{N}(a: f32[N], b: f32[N]) -> f32:
    let mut sum = 0.0
    for i from 0 to N:
        sum += a[i] * b[i]
    return sum

fn matmul{I, J, K}(a: f32[I, J], b: f32[J, K]) -> f32[I, K]:
    for i from 0 to I:
        for k from 0 to K:
            yield dot(a[i], b'[k])

fn concat{A, B}(a: f32[A], b: f32[B]) -> f32[A + B]:
    yield from a
    yield from b

fn push{N}(arr: f32[N], item: f32) -> f32[N + 1]:
    yield from arr
    yield item

fn fill{N}(val: f32) -> f32[N]:
    for i from 0 to N:
        yield val

fn triangle{N}() -> f32[N^2 - N + 1]:
    for i from 0 to N:
        for j from 0 to i:
            yield 0.0
            yield 1.0
    yield 2.0

fn test():
    let x = [1.0, 2.0, 3.0]
    let y = dot(x, fill(2))
",
    );
    println!("checked in {:?}", start.elapsed());
    assert_eq!(ast.fns.len(), 7);
}

/// Pins where linearization gives up. A product of two typevars is abstracted to an opaque
/// atom, so `i < A` no longer implies `i*B < A*B`. See the notes on Handelman certificates.
#[test]
fn constant_strides() {
    expect_ok(
        "
fn even{A}(arr: f32[2*A]) -> f32[A]:
    for i from 0 to A:
        yield arr[i*2]

fn odd{A}(arr: f32[2*A]) -> f32[A]:
    for i from 0 to A:
        yield arr[i*2 + 1]

fn tile{A}(arr: f32[A*4]) -> f32[A*4]:
    for i from 0 to A:
        for j from 0 to 4:
            yield arr[i*4 + j]
",
    );
}

/// Symbolic strides survive the atom abstraction only via a [`t2`](super::t2) certificate.
#[test]
fn symbolic_strides() {
    expect_ok(
        "
fn every_n{A, B st B > 0}(arr: f32[A*B]) -> f32[A]:
    for i from 0 to A:
        yield arr[i*B]

fn blocks{A, B st B > 0}(arr: f32[A*B]) -> f32[A*B]:
    for i from 0 to A:
        for j from 0 to B:
            yield arr[i*B + j]

fn nest3{A, B, C}(arr: f32[A*B*C]) -> f32[A*B*C]:
    for i from 0 to A:
        for j from 0 to B:
            for k from 0 to C:
                yield arr[i*B*C + j*C + k]
",
    );

    // B = 0 makes the array empty, so the access is genuinely unsafe without the constraint
    let msg = expect_error(
        "
fn every_n{A, B}(arr: f32[A*B]) -> f32[A]:
    for i from 0 to A:
        yield arr[i*B]
",
    );
    assert!(
        msg.starts_with("index: cannot prove \"B*i < A*B\""),
        "{msg}"
    );
    // no counterexample is quoted, since the model is over abstracted atoms
    assert!(!msg.contains("counterexample"), "{msg}");
}

#[test]
fn nullable_place_narrowing() {
    let msg = expect_error(
        "
fn get(a: f32?) -> f32:
    let mut x = a
    if x != null:
        x = null        # rejected
        return x
    return 0.0
",
    );

    assert!(msg.starts_with("assigned value: expected \"f32\""));
}

#[test]
fn argument_order_does_not_matter() {
    let prelude = format!("{DOT}{FILL}");

    // `x` fixes dot's N, which fixes fill's N through the expected type -- either way round
    for call in ["dot(x, fill(2))", "dot(fill(2), x)"] {
        let ast = expect_ok(&format!(
            "{prelude}
fn test() -> f32:
    let x = [1.0, 2.0, 3.0]
    return {call}
"
        ));
        let test = ast.get("test").unwrap();
        assert_eq!(test.render_var(0, "x").unwrap(), "f32[3]");
    }

    // a later argument can also inform an earlier one across a gap
    expect_ok(&format!(
        "{FILL}
fn f{{N, M}}(a: f32[N], b: f32[M], c: f32[N]) -> f32:
    return 0.0

fn test() -> f32:
    return f(fill(0.0), fill{{2}}(0.0), [1.0, 2.0, 3.0])
"
    ));

    // with nothing to pin it down, the error still surfaces exactly once per unsolved call
    let (_, errors) = check(&format!(
        "{prelude}
fn test() -> f32:
    return dot(fill(1.0), fill(2.0))
"
    ));
    assert_eq!(errors.len(), 2, "{errors:?}");
    assert!(
        errors.iter().all(|e| e
            .data
            .message
            .starts_with("cannot infer typevar \"N\" of \"fill\"")),
        "{errors:?}"
    );
}

/// A retry must not swallow an error that was never about the missing hint.
#[test]
fn argument_retry_keeps_real_errors() {
    let msg = expect_error(&format!(
        "{DOT}
fn test() -> f32:
    let x = [1.0, 2.0, 3.0]
    return dot(nonexistent, x)
"
    ));
    assert!(
        msg.starts_with("cannot find name \"nonexistent\" in scope"),
        "{msg}"
    );
}

/// t2 may only ever turn an error into a pass, so anything it wrongly proves is unsound.
#[test]
fn certificates_reject_unsafe_strides() {
    let cases = [
        // one past the end of the row
        (
            "index",
            "
fn f{A, B st B > 0}(arr: f32[A*B]) -> f32:
    for i from 0 to A:
        print arr[i*B + B]
    return 0.0
",
        ),
        // below the start
        (
            "index",
            "
fn f{A, B st B > 0}(arr: f32[A*B]) -> f32:
    for i from 0 to A:
        print arr[i*B - 1]
    return 0.0
",
        ),
        // one row too many
        (
            "index",
            "
fn f{A, B st B > 0}(arr: f32[A*B]) -> f32:
    for i from 0 to A + 1:
        print arr[i*B]
    return 0.0
",
        ),
        // quadratic stride outruns a linear array
        (
            "index",
            "
fn f{A, B st B > 1}(arr: f32[A*B]) -> f32:
    for i from 0 to A:
        print arr[i*B*B]
    return 0.0
",
        ),
        // the count is off by one, and no certificate exists for an equality that is false
        (
            "yield count",
            "
fn f{A, B st B > 0}(a: f32[A*B]) -> f32[A*B + 1]:
    for i from 0 to A:
        for j from 0 to B:
            yield a[i*B + j]
",
        ),
    ];

    for (kind, src) in cases {
        let msg = expect_error(src);
        assert!(msg.starts_with(kind), "expected a {kind} error, got: {msg}");
    }
}

/// `B >= 1` need not be declared when an inner loop already implies it.
#[test]
fn inner_loop_supplies_the_stride_bound() {
    expect_ok(
        "
fn blocks{A, B}(arr: f32[A*B]) -> f32[A*B]:
    for i from 0 to A:
        for j from 0 to B:
            yield arr[i*B + j]
",
    );
}

/// A bound `Ind` gets a name in the constraint system, so conditions can talk about it and
/// arithmetic on it stays a `Size` instead of decaying to `i64`.
#[test]
fn bound_indices_are_named() {
    let find = "
fn find{N}(needle: f32, haystack: f32[N]) -> Ind(N)?:
    for i from 0 to N:
        if needle == haystack[i]:
            return i
    return null
";
    let guarded = format!(
        "{find}
fn test():
    let mut arr = [1.0, 2.0, 3.0]
    let k = find(2.0, arr)
    if k != null:
        if k > 0:
            arr[k - 1] += 1.0
"
    );
    let ast = expect_ok(&guarded);
    // the binding carries the skolem, not the packed `Ind`
    assert_eq!(ast.render_var("test", 0, "k").unwrap(), "Option(k)");

    // without the guard `k - 1` can underflow, and the counterexample names the skolem
    let msg = expect_error(&format!(
        "{find}
fn test():
    let mut arr = [1.0, 2.0, 3.0]
    let k = find(2.0, arr)
    if k != null:
        arr[k - 1] += 1.0
"
    ));
    assert!(
        msg.starts_with("index: disproved \"k - 1 >= 0\"; counterexample: k = 0"),
        "{msg}"
    );

    // parameters work the same way
    expect_ok(
        "
fn f{N}(a: f32[N], k: Ind(N)) -> f32:
    if k < N - 1:
        return a[k + 1]
    return 0.0
",
    );

    // but array elements stay packed -- one element type stands for many values
    let ast = expect_ok(
        "
fn f{N}(a: f32[N], ks: Ind(N)[3]) -> f32:
    return a[ks[0]]
",
    );
    assert_eq!(ast.render_var("f", 0, "ks").unwrap(), "Ind(N)[3]");
}
