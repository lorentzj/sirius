use std::collections::HashMap;

use crate::solver::poly::{Poly, Var};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Ty {
    Bool,
    F32,
    F64,
    I64,
    Array { elem: Box<Ty>, shape: Vec<Poly> },
    Ind(Poly),
    Size(Poly),
    Option(Box<Self>),
    Tuple(Vec<Self>),
    Unit,
    Error,
}

impl Ty {
    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error)
    }

    pub fn is_int_like(&self) -> bool {
        matches!(self, Self::I64 | Self::Ind(_) | Self::Size(_))
    }

    pub fn is_numeric(&self) -> bool {
        self.is_int_like() || matches!(self, Self::F32 | Self::F64)
    }

    pub fn render(&self, names: &[&str]) -> String {
        match self {
            Self::Bool => "bool".to_string(),
            Self::I64 => "I64".to_string(),
            Self::F32 => "F32".to_string(),
            Self::F64 => "F64".to_string(),
            Self::Array { elem, shape } => {
                let dims: Vec<String> = shape.iter().map(|p| p.display_with(Some(names))).collect();
                format!("{}[{}]", elem.render(names), dims.join(", "))
            }
            Self::Ind(p) => {
                format!("Ind({})", p.display_with(Some(names)))
            }
            Self::Size(p) => p.display_with(Some(names)),
            Self::Option(t) => format!("Option({})", t.render(names)),
            Self::Tuple(ts) => {
                let parts: Vec<String> = ts.iter().map(|t| t.render(names)).collect();
                format!("({})", parts.join(", "))
            }
            Self::Unit => "()".to_string(),
            Self::Error => "?".to_string(),
        }
    }

    /// substitute vars, or return missing
    pub fn instantiate(&self, subst: &HashMap<Var, Poly>) -> Result<Self, Var> {
        let poly = |p: &Poly| -> Result<Poly, Var> {
            for v in p.vars() {
                if !subst.contains_key(&v) {
                    return Err(v);
                }
            }
            Ok(p.map_vars(|v| subst[&v].clone()))
        };

        Ok(match self {
            Self::Array { elem, shape } => Self::Array {
                elem: elem.clone(),
                shape: shape.iter().map(&poly).collect::<Result<_, _>>()?,
            },
            Self::Ind(p) => Self::Ind(poly(p)?),
            Self::Size(p) => Self::Size(poly(p)?),
            Self::Option(t) => Self::Option(Box::new(t.instantiate(subst)?)),
            Self::Tuple(ts) => Self::Tuple(
                ts.iter()
                    .map(|t| t.instantiate(subst))
                    .collect::<Result<_, _>>()?,
            ),
            a => a.clone(),
        })
    }

    pub fn new_arr(elem: Ty, shape: Vec<Poly>) -> Self {
        Self::Array {
            elem: Box::new(elem),
            shape,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Ty;
    use crate::solver::poly::{Poly, Var, poly};
    use std::collections::HashMap;

    #[test]
    fn instantiation() {
        let sig = Ty::new_arr(Ty::F32, vec![poly!(n + 1)]);
        let subst = HashMap::from([('n' as Var, poly!(3))]);
        assert_eq!(
            sig.instantiate(&subst),
            Ok(Ty::new_arr(Ty::F32, vec![poly!(4)]))
        );
    }

    #[test]
    fn poly_render() {
        let names = &["N", "M"];
        let t = Ty::new_arr(
            Ty::F32,
            vec![
                Poly::var(0u32, 1),
                Poly::var(1u32, 1).add(&Poly::constant(1)),
            ],
        );
        assert_eq!(t.render(names), "F32[N, M + 1]");
    }
}
