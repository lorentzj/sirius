use std::fmt;

#[derive(Clone)]
pub struct Pos<T> {
    pub start: usize,
    pub data: T,
    pub end: usize,
}

impl Pos<()> {
    pub fn span(start: usize, end: usize) -> Self {
        Self::new(start, (), end)
    }
}

impl<T> Pos<T> {
    pub fn new(start: usize, data: T, end: usize) -> Self {
        Self { start, data, end }
    }

    pub fn new_at<U>(data: T, pos: &Pos<U>) -> Self {
        Self::new(pos.start, data, pos.end)
    }
}

impl<T: PartialEq> PartialEq for Pos<T> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<T: Clone> Pos<T> {
    pub fn inner_collect<B: FromIterator<T>>(v: &[Pos<T>]) -> B {
        v.iter().map(|p| p.data.clone()).collect()
    }

    pub fn but_at<U>(&self, pos: &Pos<U>) -> Self {
        Self::new(pos.start, self.data.clone(), pos.end)
    }
}

impl Pos<String> {
    pub fn as_ref(&self) -> Pos<&str> {
        Pos::new(self.start, &self.data, self.end)
    }
}

impl<T> fmt::Debug for Pos<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} @ ({}, {})", self.data, self.start, self.end)
    }
}
