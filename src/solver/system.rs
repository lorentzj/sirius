use super::{Constraint, Poly, C};

enum ToZero {
    Eq,
    Neq,
    Gt,
    GtEq,
}

struct Relation {
    _providence: Vec<Constraint>,
    _data: (Poly, ToZero),
}

pub struct System(Vec<Relation>);

impl System {
    pub fn new<'a, I>(cs: I) -> Self
    where
        I: IntoIterator<Item = &'a Constraint>,
    {
        let mut sol_parts = vec![];
        for c in cs {
            sol_parts.push(Relation {
                _providence: vec![c.clone()],
                _data: match c.data.clone() {
                    C::Eq(a, b) => (a - b, ToZero::Eq),
                    C::NEq(a, b) => (a - b, ToZero::Neq),
                    C::Gt(a, b) => (a - b, ToZero::Gt),
                    C::GtEq(a, b) => (a - b, ToZero::GtEq),
                },
            });
        }

        System(sol_parts)
    }
}
