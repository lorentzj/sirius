#[macro_export]
macro_rules! poly_helper_b {
    () => { vec![] };

    ($var:ident) => { vec![(1, vec![(stringify!($var), 1)])] };
    ($var:ident^$pow:literal) => { vec![(1, vec![(stringify!($var), $pow)])] };

    ($var:ident*$($next:tt)+) => {{
        let mut vars = vec![(1, vec![(stringify!($var), 1)])];
        let next = $crate::poly_helper_b!($($next)*);
        match next.first() {
            Some((_, next_vars)) => vars[0].1.extend(next_vars),
            None => ()
        }

        vars.extend(next.into_iter().skip(1));

        vars
    }};

    ($var:ident^$pow:literal*$($next:tt)+) => {{
        let mut vars = vec![(1, vec![(stringify!($var), $pow)])];
        let next = $crate::poly_helper_b!($($next)*);
        match next.first() {
            Some((_, next_vars)) => vars[0].1.extend(next_vars),
            None => ()
        }

        vars.extend(next.into_iter().skip(1));

        vars
    }};

    ($var:ident+$($next:tt)+) => {{
        let mut vars = vec![(1, vec![(stringify!($var), 1)])];
        let next = $crate::poly_helper_a!($($next)*);
        vars.extend(next);

        vars
    }};

    ($var:ident^$pow:literal+$($next:tt)+) => {{
        let mut vars = vec![(1, vec![(stringify!($var), $pow)])];
        let next = $crate::poly_helper_a!($($next)*);
        vars.extend(next);

        vars
    }};

    ($var:ident-$($next:tt)+) => {{
        let mut vars = vec![(1, vec![(stringify!($var), 1)])];
        let mut next = $crate::poly_helper_a!($($next)*);
        match next.first_mut() {
            Some((next_coef, _)) => *next_coef *= -1,
            None => ()
        }
        vars.extend(next);

        vars
    }};

    ($var:ident^$pow:literal-$($next:tt)+) => {{
        let mut vars = vec![(1, vec![(stringify!($var), $pow)])];
        let mut next = $crate::poly_helper_a!($($next)*);
        match next.first_mut() {
            Some((next_coef, _)) => *next_coef *= -1,
            None => ()
        }

        vars.extend(next);

        vars
    }};
}

#[macro_export]
macro_rules! poly_helper_a {
    () => {{
        let r: Vec<(i64, Vec<(&str, u64)>)> = vec![];

        r
    }};

    ($coef:literal) => {{
        let r: Vec<(i64, Vec<(&str, u64)>)> = if $coef == 0 {
            vec![]
        } else {
            vec![($coef, vec![])]
        };

        r
    }};

    ($coef:literal*$($v:tt)*) => {{
        let mut r: Vec<(i64, Vec<(&str, u64)>)> = $crate::poly_helper_b!($($v)*);

        if $coef == 0 {
            r = vec![];
        } else if r.is_empty() {
            r = vec![($coef, vec![])];
        } else {
            r[0].0 *= $coef;
        }

        r
    }};

    ($($v:tt)*) => {{
        let r: Vec<(i64, Vec<(&str, u64)>)> = $crate::poly_helper_b!($($v)*);

        r
    }};
}

#[macro_export]
macro_rules! system {
    () => {{
        use std::rc::Rc;
        use $crate::poly::system::System;

        System {
            members: vec![],
            var_dict: Rc::new(vec![])
        }
    }};

    (@accumulate [ $($accumulated:tt)* ] [ ]) => {{
        use std::collections::{HashSet, VecDeque};
        use std::rc::Rc;
        use $crate::solver::rational::Rat;
        use $crate::solver::poly::mono::Mono;
        use $crate::solver::poly::Poly;
        use $crate::solver::poly::system::System;

        let raw_polys = vec![$($accumulated)*];

        let mut var_dict = HashSet::new();

        for raw_poly in &raw_polys {
            for (_, vars) in raw_poly {
                for (var, _) in vars {
                    if !var_dict.contains(*var) {
                        var_dict.insert(var.to_string());
                    }
                }
            }
        }

        System {
            members: raw_polys
                .into_iter()
                .map(|monos| {
                    let terms = monos.into_iter().map(|(coef, mut vars)| {
                        vars.sort_by(|a, b| a.0.cmp(b.0));
                        Mono {
                            val: Rat::new(coef),
                            vars: vars.iter().map(|(v, p)| (Rc::new(v.to_string()), *p)).collect()
                        }}
                    ).collect::<VecDeque<_>>();

                    let mut acc = Poly::constant(Rat::from(0));

                    for term in terms {
                        acc = acc + Poly { terms: vec![term] };
                    }

                    acc
                })
                .collect::<Vec<_>>()
        }
    }};

    (@accumulate [ $($accumulated:tt)* ] [ $($this_line:tt)* ]) => {
        $crate::system! { @accumulate [
            $($accumulated)*
            $crate::poly_helper_a!( $($this_line)* ),
        ] [] }
    };

    (@accumulate [ $($accumulated:tt)* ] [ $($this_line:tt)* ] , $($rest:tt)* ) => {
        $crate::system! { @accumulate [
            $($accumulated)*
            $crate::poly_helper_a!( $($this_line)* ),
        ] [] $($rest)* }
    };

    (@accumulate [ $($accumulated:tt)* ] [ $($this_line:tt)* ] $current:tt $($rest:tt)* ) => {
        $crate::system! { @accumulate [ $($accumulated)* ] [ $($this_line)* $current ] $($rest)* }
    };

    ( $($t:tt)* ) => { $crate::system! { @accumulate [] [] $($t)* } }
}

#[macro_export]
macro_rules! poly {
    ($($t:tt)*) => ({
        let mut sys = $crate::system!{ $($t)* };
        assert!(sys.members.len() == 1);
        sys.members.pop().unwrap()
    })
}
