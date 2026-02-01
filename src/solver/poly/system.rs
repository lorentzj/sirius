use super::Poly;
use super::Rat;
use super::mono::{grevlex, monomial_div};
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub struct System {
    pub var_dict: Rc<Vec<String>>,
    pub members: Vec<Poly>,
}

impl System {
    pub fn var(&self, var: &str, pow: u64) -> Poly {
        match self.var_dict.iter().position(|v| v == var) {
            Some(i) => Poly::var(i, pow),
            None => panic!("variable {} not in system variable dict", var),
        }
    }

    pub fn get(&self, i: usize) -> Poly {
        self.members[i].clone()
    }
}

impl System {
    pub fn constant(&self, val: i64) -> Poly {
        Poly::constant(Rat::from(val))
    }

    pub fn gb(&self) -> System {
        let mut sys = self.clone();

        // buchberger

        let mut combs = {
            let mut combs = vec![];
            for i in 0..sys.members.len() {
                for j in 0..sys.members.len() {
                    if i != j {
                        combs.push((sys.get(i), sys.get(j)));
                    }
                }
            }

            combs
        };

        while let Some((a, b)) = combs.pop() {
            let s = Poly::s_poly(a, b);
            let (_, rem) = s.compound_divide(&sys.members);

            if !rem.is_zero() {
                for member in &sys.members {
                    combs.push((member.clone(), rem.clone()));
                }
                sys.members.push(rem);
            }
        }

        // reduce

        let mut keep = vec![];

        for i in 0..sys.members.len() {
            let mut divides_any = false;

            for j in 0..sys.members.len() {
                if i != j {
                    let i_lt = sys.members[i].lt_mono();
                    let j_lt = sys.members[j].lt_mono();
                    if let Some(m) = monomial_div(&i_lt, &j_lt) {
                        if m.vars.is_empty() {
                            divides_any = i > j;
                        } else {
                            divides_any = true;
                        }

                        if divides_any {
                            break;
                        }
                    }
                }
            }

            if !divides_any {
                keep.push(sys.members[i].clone());
            }
        }

        let mut keep2 = vec![];

        for (i, k) in keep.iter().enumerate() {
            let (_, rem) = k.compound_divide(
                &keep
                    .iter()
                    .enumerate()
                    .filter_map(|(j, p)| if j != i { Some(p.clone()) } else { None })
                    .collect::<Vec<_>>(),
            );
            keep2.push(rem);
        }

        keep2.sort_by(|p, q| grevlex(&p.lt_mono(), &q.lt_mono()).reverse());

        sys.members = keep2.iter().map(|p| p.norm()).collect();

        sys
    }
}

impl fmt::Debug for System {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (i, p) in self.members.iter().enumerate() {
            write!(f, "{}", p.format(&self.var_dict))?;
            if i + 1 < self.members.len() {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn gb() {
        let sys = crate::system! {
            x + y^2 + z,
            x - y + 3*z + 5,
            x - 2*y + 3
        };

        assert_eq!(
            "[9z^2 + 7z - 3, x + 6z + 7, y + 3z + 2]",
            format!("{:?}", sys.gb())
        );
    }
}
