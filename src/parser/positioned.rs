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

impl<T> std::hash::Hash for Pos<T>
where
    T: std::hash::Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.data.hash(state);
    }
}
