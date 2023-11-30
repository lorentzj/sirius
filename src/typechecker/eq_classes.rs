use super::{Constraint, Substitution, Type};
use crate::{
    error::{Error, ErrorType},
    parser::Positioned,
};
use std::cmp::Ordering;
use std::rc::{Rc, Weak};

#[derive(Clone, Debug)]
pub struct EqClasses {
    trees: Vec<(Option<Rc<Type>>, TypeTree)>,
    owned: Vec<Rc<Type>>,
    can_demote_or_promote: Vec<Positioned<(Rc<Type>, Rc<Type>)>>,
    must_demote: Vec<Positioned<(Rc<Type>, Rc<Type>)>>,
    must_promote: Vec<Positioned<(Rc<Type>, Rc<Type>)>>,
}

impl EqClasses {
    pub fn new() -> Self {
        Self {
            trees: vec![],
            owned: vec![],
            can_demote_or_promote: vec![],
            must_demote: vec![],
            must_promote: vec![],
        }
    }

    pub fn add_can_demote_or_promote_owned(
        &mut self,
        start: usize,
        a: &Rc<Type>,
        b: Type,
        end: usize,
    ) {
        for Positioned {
            start: _,
            inner: (curr_a, curr_b),
            end: _,
        } in &self.can_demote_or_promote
        {
            if a == curr_a && &b == curr_b.as_ref() {
                return;
            }
        }

        self.owned.push(Rc::new(b));
        let b = self.owned.last().unwrap().clone();

        self.add_can_demote_or_promote(start, a, &b, end);
    }

    pub fn add_can_demote_or_promote(
        &mut self,
        start: usize,
        a: &Rc<Type>,
        b: &Rc<Type>,
        end: usize,
    ) {
        for Positioned {
            start: _,
            inner: (curr_a, curr_b),
            end: _,
        } in &self.can_demote_or_promote
        {
            if a == curr_a && b == curr_b {
                return;
            }
        }

        self.can_demote_or_promote
            .push(Positioned::new(start, (a.clone(), b.clone()), end));
    }

    pub fn add_must_demote(&mut self, start: usize, a: &Rc<Type>, b: &Rc<Type>, end: usize) {
        for Positioned {
            start: _,
            inner: (curr_a, curr_b),
            end: _,
        } in &self.must_demote
        {
            if a == curr_a && b == curr_b {
                return;
            }
        }

        self.must_demote
            .push(Positioned::new(start, (a.clone(), b.clone()), end));
    }

    pub fn add_must_promote(&mut self, start: usize, a: &Rc<Type>, b: &Rc<Type>, end: usize) {
        for Positioned {
            start: _,
            inner: (curr_a, curr_b),
            end: _,
        } in &self.must_promote
        {
            if a == curr_a && b == curr_b {
                return;
            }
        }

        self.must_promote
            .push(Positioned::new(start, (a.clone(), b.clone()), end));
    }

    pub fn add_owned(&mut self, start: usize, a: &Rc<Type>, b: Type, end: usize) {
        self.owned.push(Rc::new(b));
        let b = self.owned.last().unwrap().clone();

        self.add(start, a, &b, end);
    }

    pub fn add_single(&mut self, start: usize, a: &Rc<Type>, end: usize) {
        for (_, tree) in &mut self.trees {
            if tree.contains(a) {
                return;
            }
        }

        let tree = TypeTree {
            start,
            val: Rc::downgrade(a),
            lhs: None,
            rhs: None,
            end,
        };

        self.trees.push((None, tree));
    }

    pub fn add(&mut self, start: usize, a: &Rc<Type>, b: &Rc<Type>, end: usize) {
        let mut merge: Option<(usize, usize)> = None;
        let mut insert: Option<(&Rc<Type>, usize)> = None;

        for (i, (_, tree)) in self.trees.iter().enumerate() {
            if tree.contains(a) {
                if !tree.contains(b) {
                    let mut found = false;
                    for (j, (_, tree)) in self.trees.iter().enumerate().skip(i) {
                        if tree.contains(b) {
                            merge = Some((i, j));
                            found = true;
                            break;
                        }
                    }

                    if !found {
                        insert = Some((b, i));
                    }
                }
                break;
            } else if tree.contains(b) {
                if !tree.contains(a) {
                    let mut found = false;
                    for (j, (_, tree)) in self.trees.iter().enumerate().skip(i) {
                        if tree.contains(a) {
                            merge = Some((i, j));
                            found = true;
                            break;
                        }
                    }

                    if !found {
                        insert = Some((a, i));
                    }
                }
                break;
            }
        }

        match (merge, insert) {
            (Some((i, j)), _) => {
                let prefix = self.trees.split_at(i).0;
                let merged = (None, self.trees[i].1.clone().merge(self.trees[j].1.clone()));
                let inner = self.trees.split_at(j).0.split_at(i + 1).1;
                let suffix = self.trees.split_at(j + 1).1;
                self.trees = prefix
                    .iter()
                    .chain(&[merged])
                    .chain(inner)
                    .chain(suffix)
                    .cloned()
                    .collect();
            }

            (_, Some((a, i))) => {
                self.trees[i].1.insert(start, a, end);
            }

            (None, None) => {
                let mut tree = TypeTree {
                    start,
                    val: Rc::downgrade(a),
                    lhs: None,
                    rhs: None,
                    end,
                };

                tree.insert(start, b, end);

                self.trees.push((None, tree));
            }
        }
    }

    #[cfg(test)]
    pub fn iter(&self) -> Vec<Vec<Positioned<Type>>> {
        self.trees.iter().map(|(_, t)| t.to_vec()).collect()
    }

    pub fn find_replacement(&self, t: &Rc<Type>) -> Option<Rc<Type>> {
        for (repr, tree) in &self.trees {
            if let Some(repr) = repr {
                if tree.contains(t) && repr != t {
                    return Some(repr.clone());
                }
            }
        }

        None
    }

    pub fn generate(
        &mut self,
        curr_ind_forall_var: &mut usize,
    ) -> Result<(bool, Vec<Constraint>), Error> {
        let mut constraints = vec![];
        let mut any_changes = false;

        let mut all_subs = SubstitutionList::new();

        for (repr, tree) in &mut self.trees {
            let ts = tree.to_vec();
            let mut tree_repr = ts.first().unwrap().clone().inner;

            for t in ts.iter().skip(1) {
                match tree_repr.unify(&t.inner, false, &mut None) {
                    Some((subs, mut cs)) => {
                        if !subs.is_empty() {
                            any_changes = true;
                        }
                        Constraint::apply_pos(&mut cs, t.start, t.end);
                        constraints.extend(cs);

                        for sub in &subs {
                            tree_repr = tree_repr.substitute(sub);
                        }

                        match all_subs.extend(subs) {
                            Ok(_) => (),
                            Err(bad_match) => {
                                return Err(Error::new(
                                    ErrorType::Type,
                                    format!(
                                        "cannot unify types \"{:?}\" and \"{:?}\"",
                                        t.inner, bad_match
                                    ),
                                    t.start,
                                    t.end,
                                ))
                            }
                        }
                    }
                    None => {
                        return Err(Error::new(
                            ErrorType::Type,
                            format!(
                                "cannot unify types \"{:?}\" and \"{:?}\"",
                                t.inner, tree_repr
                            ),
                            t.start,
                            t.end,
                        ))
                    }
                }
            }

            for Positioned {
                start,
                inner: (a, b),
                end,
            } in &self.must_demote
            {
                if tree.contains(a) {
                    match tree_repr.unify(&b.demote_inds(), false, &mut None) {
                        Some((mut subs, _)) => {
                            let a_forall_vars = tree_repr.forall_vars();
                            let b_forall_vars = b.forall_vars();
                            subs.retain(|sub| {
                                if !sub.1.forall_vars().is_empty() {
                                    return false;
                                }

                                if a_forall_vars.contains(&sub.0) {
                                    for var in &sub.1.forall_vars() {
                                        if b_forall_vars.contains(var) {
                                            return false;
                                        }
                                    }
                                }
                                true
                            });

                            if !subs.is_empty() {
                                any_changes = true;
                            }

                            for sub in &subs {
                                tree_repr = tree_repr.substitute(sub);
                            }

                            match all_subs.extend(subs) {
                                Ok(_) => (),
                                Err(bad_match) => {
                                    return Err(Error::new(
                                        ErrorType::Type,
                                        format!(
                                            "cannot unify types \"{:?}\" and \"{:?}\"",
                                            tree_repr, bad_match
                                        ),
                                        *start,
                                        *end,
                                    ))
                                }
                            }
                        }
                        None => {
                            return Err(Error::new(
                                ErrorType::Type,
                                format!(
                                    "cannot unify types \"{:?}\" and \"{:?}\"",
                                    b.demote_inds(),
                                    tree_repr
                                ),
                                *start,
                                *end,
                            ))
                        }
                    }
                }
            }

            for Positioned {
                start,
                inner: (a, b),
                end,
            } in &self.can_demote_or_promote
            {
                if tree.contains(a) {
                    match tree_repr.unify(b, true, &mut Some(curr_ind_forall_var)) {
                        Some((mut subs, mut cs)) => {
                            let a_forall_vars = tree_repr.forall_vars();
                            let b_forall_vars = b.forall_vars();
                            subs.retain(|sub| {
                                if a_forall_vars.contains(&sub.0) {
                                    for var in &sub.1.forall_vars() {
                                        if b_forall_vars.contains(var) {
                                            return false;
                                        }
                                    }
                                }
                                true
                            });

                            if !subs.is_empty() {
                                any_changes = true;
                            }
                            Constraint::apply_pos(&mut cs, *start, *end);
                            constraints.extend(cs);

                            for sub in &subs {
                                tree_repr = tree_repr.substitute(sub);
                            }

                            match all_subs.extend(subs) {
                                Ok(_) => (),
                                Err(bad_match) => {
                                    return Err(Error::new(
                                        ErrorType::Type,
                                        format!(
                                            "cannot unify types \"{:?}\" and \"{:?}\"",
                                            tree_repr, bad_match
                                        ),
                                        *start,
                                        *end,
                                    ))
                                }
                            }
                        }
                        None => {
                            return Err(Error::new(
                                ErrorType::Type,
                                format!("cannot unify types \"{:?}\" and \"{:?}\"", b, tree_repr),
                                *start,
                                *end,
                            ))
                        }
                    }
                }
            }

            for Positioned {
                start,
                inner: (a, b),
                end,
            } in &self.must_promote
            {
                if tree.contains(a) {
                    let promoted_type = b.promote_inds(curr_ind_forall_var);
                    match promoted_type.unify(&tree_repr, false, &mut None) {
                        Some((mut subs, mut cs)) => {
                            let a_forall_vars = tree_repr.forall_vars();
                            let b_forall_vars = b.forall_vars();
                            subs.retain(|sub| {
                                if !sub.1.forall_vars().is_empty() {
                                    return false;
                                }
                                if a_forall_vars.contains(&sub.0) {
                                    for var in &sub.1.forall_vars() {
                                        if b_forall_vars.contains(var) {
                                            return false;
                                        }
                                    }
                                }
                                true
                            });

                            if !subs.is_empty() {
                                any_changes = true;
                            }
                            Constraint::apply_pos(&mut cs, *start, *end);
                            constraints.extend(cs);

                            for sub in &subs {
                                tree_repr = tree_repr.substitute(sub);
                            }

                            match all_subs.extend(subs) {
                                Ok(_) => (),
                                Err(bad_match) => {
                                    return Err(Error::new(
                                        ErrorType::Type,
                                        format!(
                                            "cannot unify types \"{:?}\" and \"{:?}\"",
                                            tree_repr, bad_match
                                        ),
                                        *start,
                                        *end,
                                    ))
                                }
                            }
                        }
                        None => {
                            return Err(Error::new(
                                ErrorType::Type,
                                format!(
                                    "cannot unify types \"{:?}\" and \"{:?}\"",
                                    promoted_type, tree_repr
                                ),
                                *start,
                                *end,
                            ));
                        }
                    }
                }
            }

            *repr = Some(Rc::new(tree_repr))
        }

        for (repr, _) in &mut self.trees {
            if let Some(repr) = repr {
                let mut tree_repr = repr.as_ref().clone();

                for sub in &all_subs.0 {
                    tree_repr = tree_repr.substitute(sub)
                }

                *repr = Rc::new(tree_repr)
            }
        }

        let mut dedup_constraints = vec![];
        for constraint in constraints {
            if !dedup_constraints.contains(&constraint) {
                dedup_constraints.push(constraint.clone());
            }
        }

        Ok((any_changes, dedup_constraints))
    }
}

#[derive(Clone)]
struct TypeTree {
    start: usize,
    val: Weak<Type>,
    lhs: Option<Box<TypeTree>>,
    rhs: Option<Box<TypeTree>>,
    end: usize,
}

impl TypeTree {
    fn insert(&mut self, start: usize, v: &Rc<Type>, end: usize) {
        if self.contains(v) {
            return;
        }

        if Rc::as_ptr(v) >= Weak::as_ptr(&self.val) {
            if let Some(lhs) = &mut self.lhs {
                lhs.insert(start, v, end);
            } else {
                self.lhs = Some(Box::new(TypeTree {
                    start,
                    val: Rc::downgrade(v),
                    lhs: None,
                    rhs: None,
                    end,
                }));
            }
        } else if let Some(rhs) = &mut self.rhs {
            rhs.insert(start, v, end);
        } else {
            self.rhs = Some(Box::new(TypeTree {
                start,
                val: Rc::downgrade(v),
                lhs: None,
                rhs: None,
                end,
            }));
        }
    }

    fn contains(&self, v: &Rc<Type>) -> bool {
        match Weak::as_ptr(&self.val).cmp(&Rc::as_ptr(v)) {
            Ordering::Less => {
                if let Some(lhs) = &self.lhs {
                    lhs.contains(v)
                } else {
                    false
                }
            }
            Ordering::Equal => true,
            Ordering::Greater => {
                if let Some(rhs) = &self.rhs {
                    rhs.contains(v)
                } else {
                    false
                }
            }
        }
    }

    fn to_vec(&self) -> Vec<Positioned<Type>> {
        let mut v = vec![];

        if let Some(t) = self.val.upgrade() {
            v.push(Positioned {
                start: self.start,
                inner: t.as_ref().clone(),
                end: self.end,
            });
        }

        if let Some(lhs) = &self.lhs {
            v.extend(lhs.to_vec());
        }

        if let Some(rhs) = &self.rhs {
            v.extend(rhs.to_vec());
        }

        v
    }

    fn to_ref_vec(&self) -> Vec<Positioned<Rc<Type>>> {
        let mut v = vec![];

        if let Some(t) = self.val.upgrade() {
            v.push(Positioned {
                start: self.start,
                inner: t,
                end: self.end,
            });
        }

        if let Some(lhs) = &self.lhs {
            v.extend(lhs.to_ref_vec());
        }

        if let Some(rhs) = &self.rhs {
            v.extend(rhs.to_ref_vec());
        }

        v
    }

    fn merge(mut self, other: TypeTree) -> TypeTree {
        for t in other.to_ref_vec() {
            self.insert(t.start, &t.inner, t.end);
        }
        self
    }
}

impl std::fmt::Debug for TypeTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.val.upgrade() {
            Some(val) => write!(f, "{{ val: {:?}, ", val)?,
            None => write!(f, "{{ val: err, ")?,
        }

        write!(f, "{:?}, {:?} }}", self.lhs, self.rhs)
    }
}

struct SubstitutionList(Vec<Substitution>);

impl SubstitutionList {
    fn new() -> Self {
        Self(vec![])
    }

    fn add(&mut self, sub: Substitution) -> Result<(), Type> {
        if self.0.contains(&sub) {
            return Ok(());
        }

        let (new_var, new_sub) = sub;

        for (self_var, self_sub) in &self.0 {
            if new_var == *self_var && self_sub.unify(&new_sub, false, &mut None).is_none() {
                return Err(self_sub.clone());
            }
        }

        self.0.push((new_var, new_sub));

        Ok(())
    }

    fn extend(&mut self, subs: Vec<Substitution>) -> Result<(), Type> {
        for sub in subs {
            self.add(sub)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::EqClasses;
    use crate::typechecker::Type;

    #[test]
    fn union_check() {
        let a = Rc::new(Type::Bool);
        let b = Rc::new(Type::ForAll(0));
        let c = Rc::new(Type::ForAll(1));
        let d = Rc::new(Type::ForAll(2));
        let e = Rc::new(Type::ForAll(3));
        let f = Rc::new(Type::ForAll(4));
        let g = Rc::new(Type::ForAll(5));

        let mut eqc = EqClasses::new();

        eqc.add(0, &a, &b, 1);
        eqc.add(0, &b, &c, 1);
        eqc.add(0, &c, &d, 1);

        eqc.add(1, &e, &f, 2);

        eqc.add_owned(1, &e, Type::F64, 2);
        eqc.add_owned(2, &e, Type::ForAll(6), 3);

        eqc.add(1, &f, &g, 2);

        let class_vec: Vec<_> = eqc.iter();
        let class_0: Vec<_> = class_vec[0].iter().map(|t| t.inner.clone()).collect();
        assert!(class_0.contains(a.as_ref()));
        assert!(class_0.contains(b.as_ref()));
        assert!(class_0.contains(c.as_ref()));
        assert!(class_0.contains(d.as_ref()));

        assert_eq!(class_vec[1].len(), 5);
    }
}
