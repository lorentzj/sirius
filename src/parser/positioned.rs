#[derive(Clone)]
pub struct Pos<T> {
    pub start: usize,
    pub data: T,
    pub end: usize,
}

impl<T> Pos<T> {
    pub fn new(start: usize, data: T, end: usize) -> Self {
        Self { start, data, end }
    }

    pub fn map<U>(&self, f: fn(&T) -> U) -> Pos<U> {
        Pos {
            start: self.start,
            data: f(&self.data),
            end: self.end,
        }
    }
}

impl<T> Pos<T>
where T: Clone,
{
    pub fn collect(v: &[Pos<T>]) -> Vec<T> {
        v.into_iter().map(|p| p.data.clone()).collect()
    }
}

impl<T> std::hash::Hash for Pos<T>
where
    T: std::hash::Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.data.hash(state);
    }
}

impl Pos<String> {
    pub fn as_ref(&self) -> Pos<&str> {
        Pos::new(self.start, &self.data, self.end)
    }
}