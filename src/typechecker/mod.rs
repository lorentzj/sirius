use crate::parser::{CompilerState, Positioned};
use crate::scope::Scope;
use crate::stdlib::ExternalGlobals;

mod concreteness_check;
mod equality;
mod flow;
mod ind;
mod initialize_ast;
mod name_rules;
mod number_coersion;
mod types;
mod unify;

use concreteness_check::concreteness_check;
use initialize_ast::initialize_statement_types;
pub use initialize_ast::{annotation_type, populate_annotation};
use unify::{substitute, unify};

pub use ind::Ind;
pub use types::{Substitutions, Type};

pub fn typecheck(state: &mut CompilerState, externals: ExternalGlobals) {
    if !state.errors.is_empty() {
        return;
    }

    let mut scope = Scope::init(externals);

    for (name, function) in state.ast.iter_mut() {
        let type_arg_names = function.type_arg_names();
        for (_, arg_type) in function.args.iter_mut() {
            *arg_type = match populate_annotation(&arg_type.inner, &mut None, &type_arg_names) {
                Ok(t) => Positioned::new(arg_type.start, t, arg_type.end),
                Err(mut error) => {
                    error.start = arg_type.start;
                    error.end = arg_type.end;
                    state.errors.push(error);
                    Positioned::new(arg_type.start, Type::Unknown, arg_type.end)
                }
            }
        }

        function.return_type =
            match populate_annotation(&function.return_type.inner, &mut None, &type_arg_names) {
                Ok(t) => Positioned::new(function.return_type.start, t, function.return_type.end),
                Err(mut error) => {
                    error.start = function.return_type.start;
                    error.end = function.return_type.end;
                    state.errors.push(error);
                    Positioned::new(
                        function.return_type.start,
                        Type::Unknown,
                        function.return_type.end,
                    )
                }
            };

        scope.insert(
            name.clone(),
            Type::Function(
                function.type_arg_names(),
                function.arg_types(),
                Box::new(function.return_type.inner.clone()),
            ),
        );

        state.errors.extend(name_rules::check_fn_args(function));
    }

    state.errors.extend(flow::check_flow(&state.ast));

    if !state.errors.is_empty() {
        return;
    }

    // the type inference algorithm runs on each function independently

    for function in state.ast.values_mut() {
        // first pass:
        //     - initializes type holes
        //     - resolves variable names
        //     - checks assignment & annotation legality

        // if this pass succeeds, we can try unification

        let mut function_errors = vec![];

        let mut curr_forall_var = 0;
        let type_args = function.type_arg_names();

        scope.push();

        for (arg, arg_type) in &function.args {
            scope.insert(arg.inner.clone(), arg_type.inner.clone());
        }

        scope.push();

        for statement in function.body.iter_mut() {
            initialize_statement_types(
                statement,
                &mut curr_forall_var,
                &mut scope,
                &mut function_errors,
                &type_args,
            );
        }

        scope.pop();

        if !function_errors.is_empty() {
            state.errors.extend(function_errors);
            continue;
        }

        // begin unification/substitution loop
        // unification fn walks the statements and prepares a list of substitutions
        // substitution fn updates the type info in the statements and may make new unifications possible
        // terminate if unification produces no more substitutions or on error

        scope.push();

        let mut subs = Substitutions::new();

        unify(
            &function.body,
            &mut scope,
            &mut curr_forall_var,
            &function.return_type.inner,
            &mut function_errors,
            &mut subs,
        );

        scope.pop();

        while !subs.is_empty() && function_errors.is_empty() {
            scope.push();
            substitute(&mut function.body, &mut scope, &subs);
            scope.pop();

            subs = Substitutions::new();
            scope.push();
            unify(
                &function.body,
                &mut scope,
                &mut curr_forall_var,
                &function.return_type.inner,
                &mut function_errors,
                &mut subs,
            );
            scope.pop();
        }

        if !function_errors.is_empty() {
            state.errors.extend(function_errors);
            continue;
        }

        // concreteness check - no unbound type vars should be left in the function body

        function_errors.extend(concreteness_check(function));

        if !function_errors.is_empty() {
            state.errors.extend(function_errors);
            continue;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::typecheck;
    use super::Ind;
    use super::Type;
    use crate::error::ErrorType;
    use crate::parser::{parse, S};

    #[test]
    fn basic_typecheck() {
        let code = "
fn three_tuple_map{A, B}(x: (A, A, A), f: A->B) -> (B, B, B):
    return (f(x.0), f(x.1), f(x.2))
fn map_test_1(x: f64) -> bool:
    return x > 0
fn map_test_2(x: f64) -> i64:
    if x > 0:
        return 1
    else:
        return -1
fn main():
    let a = (1., -1., -0.34)
    print three_tuple_map(a, map_test_1)
    print three_tuple_map{f64}(a, map_test_1)
    print three_tuple_map{f64, bool}(a, map_test_1)
    print three_tuple_map{_, bool}(a, map_test_1)
    print three_tuple_map(a, map_test_2)
        ";

        let mut state = parse(code);
        typecheck(&mut state, HashMap::default());

        assert!(state.errors.is_empty());
    }

    #[test]
    fn error_param_basic() {
        let code = "
fn factorial(x: i64) -> i64:
    if x < 1:
        return 1
    else:
        return x * factorial(x - 1)
fn main():
    print factorial((1, 1))
";

        let mut state = parse(code);
        typecheck(&mut state, HashMap::default());

        assert_eq!(ErrorType::TypeError, state.errors[0].error_type);
    }

    #[test]
    fn error_param_generic() {
        let code = "
fn three_tuple_map{A, B}(x: (A, A, A), f: A->B) -> (B, B, B):
    return (f(x.0), f(x.1), f(x.2))
fn map_test(x: i64) -> bool:
    return x > 0
fn main():
    let z = (1., -1., -0.34)
    print three_tuple_map(z, map_test)
";

        let mut state = parse(code);
        typecheck(&mut state, HashMap::default());

        assert_eq!(ErrorType::TypeError, state.errors[0].error_type);
    }

    #[test]
    fn ind_val() {
        let code = "
fn main():
    let x = 1 + 1 + 1
";

        let mut state = parse(code);
        typecheck(&mut state, HashMap::default());

        assert!(if let S::Let(_, _, val) = &state.ast["main"].body[0].data {
            val.t == Type::I64(Some(Ind::constant(3)))
        } else {
            false
        });
    }

    #[test]
    fn inference_error() {
        let code = "
fn dbl{A}(a: A) -> (A, A):
    return (a, a)
fn main():
    let x = dbl{(_, bool)}((5, true)).0.1 || 1
    print x
";

        let mut state = parse(code);
        typecheck(&mut state, HashMap::default());

        assert_eq!(ErrorType::TypeError, state.errors[0].error_type);
    }

    #[test]
    fn non_concrete() {
        let code = "
fn double{T}(x: T) -> (T, T):
    return (x, x)
fn main():
    let f = double
    ";

        let mut state = parse(code);
        typecheck(&mut state, HashMap::default());

        assert_eq!(ErrorType::TypeError, state.errors[0].error_type);
    }
}
