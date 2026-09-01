//! A pipe to [Z3](https://github.com/z3prover/z3) for passing QF_LIA constraints.

use std::collections::HashMap;
use std::fmt;
use std::io::Write as _;
use std::path::PathBuf;
use std::process::{Command, Stdio};

use super::poly::{Poly, coef::Coef, mono::Var};

/// Obligation vocabulary.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Cmp {
    Eq,
    Ne,
    Le,
    Lt,
    Ge,
    Gt,
}

impl Cmp {
    pub fn negated(self) -> Cmp {
        match self {
            Cmp::Eq => Cmp::Ne,
            Cmp::Ne => Cmp::Eq,
            Cmp::Le => Cmp::Gt,
            Cmp::Lt => Cmp::Ge,
            Cmp::Ge => Cmp::Lt,
            Cmp::Gt => Cmp::Le,
        }
    }

    fn smt(self) -> &'static str {
        match self {
            Cmp::Eq => "=",
            Cmp::Ne => "distinct",
            Cmp::Le => "<=",
            Cmp::Lt => "<",
            Cmp::Ge => ">=",
            Cmp::Gt => ">",
        }
    }
}

impl fmt::Display for Cmp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Cmp::Eq => "==",
            Cmp::Ne => "!=",
            Cmp::Le => "<=",
            Cmp::Lt => "<",
            Cmp::Ge => ">=",
            Cmp::Gt => ">",
        })
    }
}

/// Format for facts and goals, e.g. `X > 2*Y`.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Constraint {
    pub lhs: Poly,
    pub cmp: Cmp,
    pub rhs: Poly,
}

impl Constraint {
    pub fn new(lhs: Poly, cmp: Cmp, rhs: Poly) -> Self {
        Self { lhs, cmp, rhs }
    }
}

/// Z3 response. Refutations provide counterexamples.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Verdict {
    Proved,
    Refuted(Vec<(Var, i128)>),
    Unknown,
}

/// Z3 instance, possibly with a filesystem cache.
pub struct Solver {
    cache: HashMap<u64, Verdict>,
    corpus_dir: Option<PathBuf>,
}

impl Solver {
    pub fn new(cache_dir: Option<PathBuf>) -> Option<Solver> {
        let z3_available = Command::new("z3")
            .arg("-version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok_and(|s| s.success());
        z3_available.then_some(Solver {
            cache: HashMap::new(),
            corpus_dir: cache_dir,
        })
    }

    pub fn entails_lia(
        &mut self,
        facts: &[Constraint],
        goal: &Constraint,
        nonneg: &[Var],
        names: &[&str],
    ) -> Verdict {
        let mut vars: Vec<Var> = Vec::new();
        for c in facts.iter().chain(std::iter::once(goal)) {
            for p in [&c.lhs, &c.rhs] {
                debug_assert!(p.total_degree() <= 1);
                for v in p.vars() {
                    if !vars.contains(&v) {
                        vars.push(v);
                    }
                }
            }
        }
        vars.sort_unstable();

        let script = build_script(facts, goal, nonneg, &vars);
        let key = fnv1a_hash(script.as_bytes());
        if let Some(v) = self.cache.get(&key) {
            return v.clone();
        }

        let verdict = run_z3(&script, &vars);
        if let Some(dir) = &self.corpus_dir {
            let _ = std::fs::create_dir_all(dir);
            let mut file = String::new();
            file.push_str(&format!("; obligation: {}\n", render(goal, names)));
            for f in facts {
                file.push_str(&format!("; given:      {}\n", render(f, names)));
            }
            file.push_str(&format!("; verdict:    {}\n", verdict_tag(&verdict)));
            file.push_str(&script);
            let _ = std::fs::write(dir.join(format!("{key:016x}.smt2")), file);
        }
        self.cache.insert(key, verdict.clone());
        verdict
    }
}

fn verdict_tag(v: &Verdict) -> &'static str {
    match v {
        Verdict::Proved => "proved (unsat)",
        Verdict::Refuted(_) => "refuted (sat)",
        Verdict::Unknown => "unknown",
    }
}

fn render(c: &Constraint, names: &[&str]) -> String {
    format!(
        "{} {} {}",
        c.lhs.display_with(Some(names)),
        c.cmp,
        c.rhs.display_with(Some(names))
    )
}

fn build_script(facts: &[Constraint], goal: &Constraint, nonneg: &[Var], vars: &[Var]) -> String {
    let mut s = String::new();
    s.push_str("(set-option :timeout 2000)\n");
    s.push_str("(set-logic QF_LIA)\n");
    for v in vars {
        s.push_str(&format!("(declare-const v{} Int)\n", v));
    }
    for v in vars {
        if nonneg.contains(v) {
            s.push_str(&format!("(assert (>= v{} 0))\n", v));
        }
    }
    for f in facts {
        s.push_str(&format!("(assert {})\n", smt_cmp(f)));
    }
    s.push_str(&format!("(assert (not {}))\n", smt_cmp(goal)));
    s.push_str("(check-sat)\n");
    if !vars.is_empty() {
        let list: Vec<String> = vars.iter().map(|v| format!("v{}", v)).collect();
        s.push_str(&format!("(get-value ({}))\n", list.join(" ")));
    }
    s
}

fn smt_cmp(c: &Constraint) -> String {
    format!(
        "({} {} {})",
        c.cmp.smt(),
        smt_poly(&c.lhs),
        smt_poly(&c.rhs)
    )
}

fn smt_poly(p: &Poly) -> String {
    if p.is_zero() {
        return "0".to_string();
    }
    let terms: Vec<String> = p
        .terms()
        .iter()
        .map(|(c, m)| {
            let coef = smt_int(*c);
            match m.exps() {
                [] => coef,
                [(v, 1)] => {
                    if *c == 1.into() {
                        format!("v{}", v)
                    } else {
                        format!("(* {coef} v{})", v)
                    }
                }
                _ => unreachable!("not linearized"),
            }
        })
        .collect();
    if terms.len() == 1 {
        terms.into_iter().next().unwrap()
    } else {
        format!("(+ {})", terms.join(" "))
    }
}

fn smt_int(c: Coef) -> String {
    if !c.is_zero() && !c.is_positive() {
        format!("(- {:?})", c.abs())
    } else {
        format!("{:?}", c.abs())
    }
}

fn run_z3(script: &str, vars: &[Var]) -> Verdict {
    let Ok(mut child) = Command::new("z3")
        .arg("-in")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
    else {
        return Verdict::Unknown;
    };
    if let Some(stdin) = child.stdin.take() {
        let mut stdin = stdin;
        if stdin.write_all(script.as_bytes()).is_err() {
            let _ = child.kill();
            return Verdict::Unknown;
        }
    }
    let Ok(out) = child.wait_with_output() else {
        return Verdict::Unknown;
    };
    let stdout = String::from_utf8_lossy(&out.stdout);
    let mut lines = stdout.lines();
    match lines.next().map(str::trim) {
        Some("unsat") => Verdict::Proved,
        Some("sat") => {
            let rest: String = lines.collect::<Vec<_>>().join(" ");
            Verdict::Refuted(parse_model(&rest, vars))
        }
        _ => Verdict::Unknown,
    }
}

fn parse_model(text: &str, vars: &[Var]) -> Vec<(Var, i128)> {
    let mut model = Vec::new();
    let cleaned = text.replace(['(', ')'], " ");
    let tokens: Vec<&str> = cleaned.split_whitespace().collect();
    let mut i = 0;
    while i < tokens.len() {
        if let Some(idx) = tokens[i]
            .strip_prefix('v')
            .and_then(|s| s.parse::<u32>().ok())
        {
            let (value, used) = match tokens.get(i + 1) {
                Some(&"-") => (
                    tokens
                        .get(i + 2)
                        .and_then(|t| t.parse::<i128>().ok())
                        .map(|k| -k),
                    3,
                ),
                Some(t) => (t.parse::<i128>().ok(), 2),
                None => (None, 1),
            };
            if let Some(value) = value
                && vars.contains(&(idx as Var))
            {
                model.push(((idx as Var), value));
            }
            i += used;
        } else {
            i += 1;
        }
    }
    model
}

fn fnv1a_hash(bytes: &[u8]) -> u64 {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for &b in bytes {
        hash ^= u64::from(b);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    hash
}

#[cfg(test)]
mod tests {
    use super::super::poly::poly;
    use super::*;

    #[test]
    fn entailment_with_context() {
        let mut s = Solver::new(None).unwrap();
        // N >= 1 => 0 < N
        let facts = [Constraint::new(poly!(n), Cmp::Ge, poly!(1))];
        let goal = Constraint::new(poly!(0), Cmp::Lt, poly!(n));
        assert_eq!(s.entails_lia(&facts, &goal, &[], &["N"]), Verdict::Proved);
        // N >= 1 =/> 1 < N
        let goal = Constraint::new(poly!(1), Cmp::Lt, poly!(n));
        match s.entails_lia(&facts, &goal, &['n' as Var], &["N"]) {
            Verdict::Refuted(model) => assert_eq!(model, vec![('n' as Var, 1)]),
            v => panic!("expected refutation, got {v:?}"),
        }
    }

    #[test]
    fn integer_reasoning_not_real() {
        let mut s = Solver::new(None).unwrap();
        // 2N >= 1 => N >= 1
        let facts = [Constraint::new(poly!(2 * n), Cmp::Ge, poly!(1))];
        let goal = Constraint::new(poly!(n), Cmp::Ge, poly!(1));
        assert_eq!(s.entails_lia(&facts, &goal, &[], &["N"]), Verdict::Proved);
    }

    #[test]
    fn commute() {
        let mut s = Solver::new(None).unwrap();
        // N == M => M == N
        let facts = [Constraint::new(poly!(n), Cmp::Eq, poly!(m))];
        let goal = Constraint::new(poly!(m), Cmp::Eq, poly!(n));
        assert_eq!(
            s.entails_lia(&facts, &goal, &[], &["N", "M"]),
            Verdict::Proved
        );
        // cached
        assert_eq!(
            s.entails_lia(&facts, &goal, &[], &["N", "M"]),
            Verdict::Proved
        );
    }
}
