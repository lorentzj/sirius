use std::rc::Rc;

use crate::parser::{CompilerState, Positioned};
use crate::scope::Scope;
use crate::stdlib::ExternalGlobals;

mod add_preconditions;
mod arithmetic;
mod concreteness_check;
mod constraint_check;
mod eq_classes;
mod equality;
mod flow;
mod initialize_ast;
mod name_rules;
mod substitute;
mod types;
mod unify;

use crate::solver::Constraint;
use concreteness_check::concreteness_check;
use constraint_check::constraint_check;
use initialize_ast::initialize_statement_types;
pub use initialize_ast::{annotation_type, populate_annotation};

pub use types::{Substitution, Type};
use unify::unify;

use self::substitute::substitute;
use self::unify::UnificationResult;

#[derive(Clone)]
pub struct ScopeEntry {
    t: Rc<Type>,
    mutable: bool,
    decl_site: Option<usize>,
}

pub fn typecheck(state: &mut CompilerState, externals: ExternalGlobals) {
    if !state.errors.is_empty() {
        return;
    }

    let mut scope: Scope<ScopeEntry> = Scope::init(
        externals
            .into_iter()
            .map(|(name, t)| {
                (
                    name,
                    ScopeEntry {
                        t: Rc::new(t),
                        mutable: false,
                        decl_site: None,
                    },
                )
            })
            .collect(),
    );

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
                Ok(t) => Positioned::new(
                    function.return_type.start,
                    Rc::new(t),
                    function.return_type.end,
                ),
                Err(mut error) => {
                    error.start = function.return_type.start;
                    error.end = function.return_type.end;
                    state.errors.push(error);
                    Positioned::new(
                        function.return_type.start,
                        Rc::new(Type::Unknown),
                        function.return_type.end,
                    )
                }
            };

        scope.insert(
            name.clone(),
            ScopeEntry {
                t: Rc::new(Type::Function(
                    function.type_arg_names(),
                    function.arg_types(),
                    Box::new(function.return_type.inner.as_ref().clone()),
                )),
                mutable: false,
                decl_site: Some(function.name.start),
            },
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
        let mut curr_ind_forall_var = 0;
        let type_args = function.type_arg_names();

        scope.push();

        for (arg, arg_type) in &function.args {
            let arg_type = arg_type.inner.promote_inds(&mut curr_ind_forall_var);

            scope.insert(
                arg.inner.clone(),
                ScopeEntry {
                    t: Rc::new(arg_type),
                    mutable: false,
                    decl_site: Some(arg.start),
                },
            );
        }

        scope.push();

        for statement in function.body.0.iter_mut() {
            initialize_statement_types(
                statement,
                &mut curr_forall_var,
                &mut curr_ind_forall_var,
                &mut scope,
                &mut function_errors,
                &type_args,
                &mut state.highlight_map,
            );
        }

        scope.pop();

        if !function_errors.is_empty() {
            state.errors.extend(function_errors);
            continue;
        }

        let mut unification_result = UnificationResult {
            any_changes: true,
            eq_classes: vec![],
        };

        while unification_result.any_changes && function_errors.is_empty() {
            unification_result = unify(
                &mut function.body.0,
                scope.clone(),
                &function.return_type.inner,
                &mut function_errors,
                &mut curr_ind_forall_var,
                &mut function.body.1,
            );

            if function_errors.is_empty() {
                for eqc in &unification_result.eq_classes {
                    substitute(&mut function.body.0, eqc);
                }
            }
        }

        scope.pop();

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

        // if and for preconditions
        add_preconditions::add_preconditions(&mut function.body.0);

        // constraint check - pass value constraints to solver

        function_errors.extend(constraint_check(function));

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
    use super::Type;
    use crate::error::ErrorType;
    use crate::parser::{parse, S};
    use crate::solver::poly::Poly;
    use crate::solver::rational::Rat;

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

        assert_eq!(state.errors, vec![]);
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

        assert_eq!(ErrorType::Type, state.errors[0].error_type);
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

        assert_eq!(ErrorType::Type, state.errors[0].error_type);
    }

    #[test]
    fn ind_val() {
        let code = "
fn main():
    let x = 1 + 1 + 1
";

        let mut state = parse(code);
        typecheck(&mut state, HashMap::default());

        assert!(
            if let S::Let(_, _, _, _, val) = &state.ast["main"].body.0[0].data {
                val.t.as_ref() == &Type::I64(Some(Poly::constant(Rat::from(3))))
            } else {
                false
            }
        );
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

        assert_eq!(ErrorType::Type, state.errors[0].error_type);
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

        assert_eq!(ErrorType::Type, state.errors[0].error_type);
    }

    #[test]
    fn mutation_tracking() {
        let code = "
fn main():
    let mut a = ((1, 2), 3)
    let mut b = 4
    let c = 5
    let d = (a.0, b, c)
    let e = d.0.1
";

        let mut state = parse(code);
        typecheck(&mut state, HashMap::default());

        assert!(state.errors.is_empty());

        if let S::Let(_, _, _, a_type, _) = &state.ast["main"].body.0[0].data {
            let a_req = Type::Tuple(vec![
                Type::Tuple(vec![Type::I64(None), Type::I64(None)]),
                Type::I64(None),
            ]);
            assert_eq!(a_type.as_ref(), &a_req);
        } else {
            panic!()
        }

        if let S::Let(_, _, _, b_type, _) = &state.ast["main"].body.0[1].data {
            let b_req = Type::I64(None);
            assert_eq!(b_type.as_ref(), &b_req);
        } else {
            panic!()
        }

        if let S::Let(_, _, _, c_type, _) = &state.ast["main"].body.0[2].data {
            let c_req = Type::I64(Some(Poly::constant(Rat::from(5))));
            assert_eq!(c_type.as_ref(), &c_req);
        } else {
            panic!()
        }

        if let S::Let(_, _, _, d_type, _) = &state.ast["main"].body.0[3].data {
            if let Type::Tuple(inner) = d_type.as_ref() {
                if let Type::Tuple(a_0) = &inner[0] {
                    assert!(matches!(&a_0[0], Type::I64(Some(_a_0_0_var))));

                    if let Type::I64(Some(a_0_1_var)) = &a_0[1] {
                        if let S::Let(_, _, _, e_type, _) = &state.ast["main"].body.0[4].data {
                            if let Type::I64(Some(d_0_1_var)) = e_type.as_ref() {
                                assert_eq!(d_0_1_var, a_0_1_var);
                            } else {
                                panic!();
                            }
                        } else {
                            panic!();
                        }
                    }
                }

                assert!(matches!(&inner[1], Type::I64(Some(_b_var))));
                if let Type::I64(Some(five)) = &inner[2] {
                    assert_eq!(five, &Poly::constant(Rat::from(5)));
                } else {
                    panic!();
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }

    #[test]
    fn nested_block_checks() {
        let code = "
fn main():
    let mut a = 1
    let mut b = 2

    if a + b < 10:
        let x = a
        let y = b
        if a > 2:
            let z = x + y
";

        let mut state = parse(code);
        typecheck(&mut state, HashMap::default());

        assert!(state.errors.is_empty());
    }

    #[test]
    fn consistent_poly_vars_in_flow() {
        let code = "
fn untracked() -> i64:
    return 1

fn main():
    let x = untracked()
    let y = untracked()

    if true:
        let z = x + y
";

        let mut state = parse(code);
        typecheck(&mut state, HashMap::default());

        let first_let_stmt = &state.ast["main"].body.0[0].data;
        let second_let_stmt = &state.ast["main"].body.0[1].data;
        let if_stmt = &state.ast["main"].body.0[2].data;

        if let (S::Let(_, _, _, t1, _), S::Let(_, _, _, t2, _), S::If(_, _, (inner, _), _)) =
            (first_let_stmt, second_let_stmt, if_stmt)
        {
            let inner_let_stmt = &inner[0].data;
            if let S::Let(_, _, _, t3, _) = inner_let_stmt {
                assert_eq!(
                    "i64(p='a), i64(p='b), i64(p='a + 'b)",
                    format!("{t1:?}, {t2:?}, {t3:?}")
                )
            } else {
                panic!("if shoud have one let")
            }
        } else {
            panic!("main should have let, let, if")
        }
    }
}
