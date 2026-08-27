#[doc(hidden)]
#[macro_export]
macro_rules! __poly_term {
    () => {{ std::collections::VecDeque::new() }};
    ($var:ident ^ $pow:literal $(* $($rest:tt)*)?) => {{
        use $crate::solver::poly::mono::{Var, __read_var_name};
        const VAR_NAME: Var = __read_var_name(stringify!($var));

        let mut rest = $crate::__poly_term!($($($rest)*)?);
        if rest.is_empty() {
            rest.push_front((1, vec![(VAR_NAME, $pow)]));
        } else {
            rest[0].1.push((VAR_NAME, $pow));
        }
        rest
    }};

    ($var:ident ^ $pow:literal $(+ $($rest:tt)*)?) => {{
        use $crate::solver::poly::mono::{Var, __read_var_name};
        const VAR_NAME: Var = __read_var_name(stringify!($var));

        let mut rest = $crate::__poly_sum!($($($rest)*)?);
        rest.push_front((1, vec![(VAR_NAME, $pow)]));
        rest
    }};

    ($var:ident ^ $pow:literal $(- $($rest:tt)*)?) => {{
        use $crate::solver::poly::mono::{Var, __read_var_name};
        const VAR_NAME: Var = __read_var_name(stringify!($var));

        let mut rest = $crate::__poly_sum!($($($rest)*)?);
        match rest.get_mut(0) {
            Some(term) => term.0 *= -1,
            None => {}
        }
        rest.push_front((1, vec![(VAR_NAME, $pow)]));
        rest
    }};

    ($var:ident $(* $($rest:tt)*)?) => {{
        $crate::__poly_term!($var ^ 1 * $($($rest)*)?)
    }};

    ($var:ident $(+ $($rest:tt)*)?) => {{
        $crate::__poly_term!($var ^ 1 + $($($rest)*)?)
    }};

    ($var:ident $(- $($rest:tt)*)?) => {{
        $crate::__poly_term!($var ^ 1 - $($($rest)*)?)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! __poly_sum {
    ()             => {{ std::collections::VecDeque::new() }};
    ($c:literal)   => {{ std::collections::VecDeque::from([($c, vec![])]) }};
    ($c:literal+$($rest:tt)+) => {{
        let mut rest = $crate::__poly_sum!($($rest)*);
        rest.push_front(($c, vec![]));
        rest
    }};
    ($c:literal-$($rest:tt)+) => {{
        let mut rest = $crate::__poly_sum!($($rest)*);
        rest[0].0 *= -1;
        rest.push_front(($c, vec![]));
        rest
    }};
    ($c:literal*$($rest:tt)+) => {{
        let mut v = $crate::__poly_term!($($rest)*);
        v[0].0 *= $c;
        v
    }};
    ($($rest:tt)+) => {{ $crate::__poly_term!($($rest)*) }};
}

/// Create a [`Poly`](super::Poly). Accepts 1-character ASCII variable names; <nobr>`poly!(2*x*y^5 + 8*z)`</nobr>.
#[doc(hidden)]
#[macro_export]
macro_rules! __poly {
    ($($body:tt)*) => {{
        use $crate::solver::poly::{Poly, mono::{Var, Pow, Mono}};
        let v: Vec<(i128, Vec<(Var, Pow)>)> = $crate::__poly_sum!($($body)*).into();
        Poly::from_terms(v.into_iter().map(|(c, exps)| (c.into(), Mono::new(exps))))
    }};
}

#[doc(inline)]
pub use crate::__poly as poly;
