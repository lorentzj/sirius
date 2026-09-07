//! Linearized systems of constraints and a pipe to [Z3](https://github.com/z3prover/z3) for QF_LIA solving.

use js_sys::Function;
use std::collections::HashMap;
use std::io::Write as _;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use wasm_bindgen::prelude::*;

use super::Constraint;
use super::Verdict;
use super::poly::{
    Poly,
    coef::Coef,
    mono::{Mono, Var},
};

/// A linearized representation of a system of constraints.
/// Non-linear atoms get fresh [`Var`]s and names like `X*Y` or `Z^2`.
pub struct Linearized {
    pub facts: Vec<Constraint>,
    pub goal: Constraint,
    pub nonneg: Vec<Var>,
    pub pure_linear: bool,
    pub names: Vec<String>,
}

impl Linearized {
    pub fn new(
        facts: &[Constraint],
        goal: &Constraint,
        vars: &[String],
        nonneg: &dyn Fn(Var) -> bool,
    ) -> Self {
        let base = vars.len() as u32;
        let mut atoms: Vec<Mono> = Vec::new();
        let mut linearize_poly = |p: &Poly| -> Poly {
            let terms = p.terms().into_iter().map(|(c, m)| {
                if m.total_degree() <= 1 {
                    (c, m.clone())
                } else {
                    let idx = match atoms.iter().position(|a| *a == m) {
                        Some(i) => i,
                        None => {
                            atoms.push(m.clone());
                            atoms.len() - 1
                        }
                    };
                    (c, Mono::new([(base + idx as u32, 1)]))
                }
            });
            Poly::from_terms(terms)
        };

        let mut linearize_constraint = |c: &Constraint| Constraint {
            lhs: linearize_poly(&c.lhs),
            cmp: c.cmp,
            rhs: linearize_poly(&c.rhs),
        };

        let facts: Vec<Constraint> = facts.iter().map(&mut linearize_constraint).collect();
        let goal = linearize_constraint(goal);

        let mut nonneg_vars: Vec<Var> = (0..base).filter(|&v| nonneg(v)).collect();
        let mut names: Vec<String> = vars.to_vec();
        for (i, m) in atoms.iter().enumerate() {
            // An atom over nonneg variables is nonneg
            if m.exps().iter().all(|&(v, _)| nonneg(v)) {
                nonneg_vars.push(base + i as u32);
            }
            let rendered: Vec<String> = m
                .exps()
                .iter()
                .map(|&(v, e)| {
                    let name = &vars[v as usize];
                    if e > 1 {
                        format!("{name}^{e}")
                    } else {
                        name.to_string()
                    }
                })
                .collect();
            names.push(rendered.join("*"));
        }

        Self {
            pure_linear: atoms.is_empty(),
            facts,
            goal,
            nonneg: nonneg_vars,
            names,
        }
    }
}

enum Z3Kind {
    Callback(Box<dyn Fn(&str) -> String>),
    Cli,
}

/// A cached Z3 pipe, using either the command line or a JavaScript callback.
pub struct Z3 {
    /// Obligations submitted.
    pub queries: usize,
    /// Obligations passed to z3.
    pub z3_calls: usize,
    cache: HashMap<u64, Verdict>,
    kind: Z3Kind,
    // optional filesystem cache
    cache_dir: Option<PathBuf>,
}

impl Z3 {
    /// Expect the `z3` command to be available in the environment.
    /// Optionally provide a filesystem cache for model output.
    pub fn new_cli(cache_dir: Option<PathBuf>) -> Option<Self> {
        let z3_available = Command::new("z3")
            .arg("-version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok_and(|s| s.success());

        z3_available.then_some(Self {
            cache: HashMap::new(),
            queries: 0,
            z3_calls: 0,
            kind: Z3Kind::Cli,
            cache_dir,
        })
    }

    /// Pass a javascript callback string->string for smt2 input.
    pub fn new_wasm(callback: Function) -> Self {
        let wrapped_callback = move |s: &str| {
            let res = callback
                .call1(&JsValue::NULL, &JsValue::from_str(s))
                .and_then(|val| val.as_string().ok_or("callback returned non-string".into()));
            match res {
                Ok(model) => model,
                Err(_) => "".into(),
            }
        };

        Self {
            cache: HashMap::new(),
            queries: 0,
            z3_calls: 0,
            kind: Z3Kind::Callback(Box::new(wrapped_callback)),
            cache_dir: None,
        }
    }

    pub fn entails_lia(
        &mut self,
        facts: &[Constraint],
        goal: &Constraint,
        nonneg: &[Var],
        names: &[&str],
    ) -> Verdict {
        self.queries += 1;
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

        self.z3_calls += 1;
        let verdict = self.run_z3(&script, &vars);
        if let Some(dir) = &self.cache_dir {
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

    fn run_z3(&mut self, script: &str, vars: &[Var]) -> Verdict {
        let stdout = match &mut self.kind {
            Z3Kind::Cli => {
                let Ok(mut child) = Command::new("z3")
                    .arg("-smt2")
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
                stdout.to_string()
            }
            Z3Kind::Callback(callback) => callback(script),
        };

        let mut stdout_lines = stdout.lines();

        match stdout_lines.next().map(str::trim) {
            Some("unsat") => Verdict::Proved,
            Some("sat") => {
                let rest: String = stdout_lines.collect::<Vec<_>>().join(" ");
                Verdict::RefutedAt(parse_model(&rest, vars))
            }
            _ => Verdict::Unknown,
        }
    }
}

fn verdict_tag(v: &Verdict) -> &'static str {
    match v {
        Verdict::Proved => "proved (unsat)",
        Verdict::RefutedBy(_) => "refuted (sat)",
        Verdict::RefutedAt { .. } => "refuted (sat)",
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
    use crate::solver::poly::{Poly, poly};
    use crate::solver::{Cmp, Constraint};

    use super::*;

    #[test]
    fn model_script() {
        let facts = [Constraint::new(poly!(n), Cmp::Eq, poly!(m))];
        let goal = Constraint::new(poly!(m), Cmp::Eq, poly!(n));
        let script = "(set-option :timeout 2000)
(set-logic QF_LIA)
(declare-const v109 Int)
(declare-const v110 Int)
(assert (= v110 v109))
(assert (not (= v109 v110)))
(check-sat)
(get-value (v109 v110))
";
        assert_eq!(script, build_script(&facts, &goal, &[], &[109, 110]))
    }

    #[test]
    fn entailment_with_context() {
        let mut s = Z3::new_cli(None).unwrap();
        // N >= 1 => 0 < N
        let facts = [Constraint::new(poly!(n), Cmp::Ge, poly!(1))];
        let goal = Constraint::new(poly!(0), Cmp::Lt, poly!(n));
        assert_eq!(s.entails_lia(&facts, &goal, &[], &["N"]), Verdict::Proved);
        // N >= 1 =/> 1 < N
        let goal = Constraint::new(poly!(1), Cmp::Lt, poly!(n));
        match s.entails_lia(&facts, &goal, &['n' as Var], &["N"]) {
            Verdict::RefutedAt(point) => assert_eq!(point, vec![('n' as Var, 1)]),
            v => panic!("expected refutation, got {v:?}"),
        }
    }

    #[test]
    fn integer_reasoning_not_real() {
        let mut s = Z3::new_cli(None).unwrap();
        // 2N >= 1 => N >= 1
        let facts = [Constraint::new(poly!(2 * n), Cmp::Ge, poly!(1))];
        let goal = Constraint::new(poly!(n), Cmp::Ge, poly!(1));
        assert_eq!(s.entails_lia(&facts, &goal, &[], &["N"]), Verdict::Proved);
    }

    #[test]
    fn commute() {
        let mut s = Z3::new_cli(None).unwrap();
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

    #[test]
    fn linearized() {
        let vars = ["N".into(), "M".into()];
        let nm = Poly::var(0u32, 1).mul(&Poly::var(1u32, 1));

        let fact = Constraint {
            lhs: nm.clone(),
            cmp: Cmp::Ge,
            rhs: poly!(4),
        };
        let goal = Constraint {
            lhs: poly!(4),
            cmp: Cmp::Le,
            rhs: nm,
        };

        let lin = Linearized::new(&[fact], &goal, &vars, &|_| true);
        assert!(!lin.pure_linear);
        assert_eq!(lin.facts[0].lhs, lin.goal.rhs);
        assert_eq!(lin.facts[0].lhs, Poly::var(2u32, 1));

        assert!(lin.nonneg.contains(&2));
        assert_eq!(lin.names[2], "N*M");
    }
}
