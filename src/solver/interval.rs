use super::{field::Zero, rational::Rat, Poly, Truth};

#[derive(Clone, Debug, PartialEq)]
pub enum Point {
    NegInf,
    P(Poly),
    Inf,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Interval {
    pub start: Point,
    pub end: Point,
}

impl Interval {
    pub fn new(start: Poly, end: Poly) -> Self {
        Interval {
            start: Point::P(start),
            end: Point::P(end),
        }
    }

    pub fn new_max(end: Poly) -> Self {
        Interval {
            start: Point::NegInf,
            end: Point::P(end),
        }
    }

    pub fn new_min(start: Poly) -> Self {
        Interval {
            start: Point::P(start),
            end: Point::Inf,
        }
    }

    pub fn contains(&self, point: Point) -> Truth {
        match point {
            Point::NegInf => {
                if self.start == Point::NegInf {
                    Truth::True
                } else {
                    Truth::False
                }
            }
            Point::P(p) => {
                let mut may_be_undetermined = false;
                match &self.start {
                    Point::NegInf => (),
                    Point::P(start_p) => {
                        match p.greater(start_p.clone()).or(p.equal(start_p.clone())) {
                            Truth::False => return Truth::False,
                            Truth::True => (),
                            Truth::Undetermined => may_be_undetermined = true,
                        }
                    }
                    Point::Inf => return Truth::False,
                }

                match &self.end {
                    Point::NegInf => Truth::False,
                    Point::P(end_p) => match p.less(end_p.clone()) {
                        Truth::False => Truth::False,
                        Truth::True => {
                            if may_be_undetermined {
                                Truth::Undetermined
                            } else {
                                Truth::True
                            }
                        }
                        Truth::Undetermined => Truth::Undetermined,
                    },

                    Point::Inf => {
                        if may_be_undetermined {
                            Truth::Undetermined
                        } else {
                            Truth::True
                        }
                    }
                }
            }

            Point::Inf => {
                if self.end == Point::Inf {
                    Truth::True
                } else {
                    Truth::False
                }
            }
        }
    }

    pub fn is_empty(&self) -> Truth {
        match &self.start {
            Point::NegInf => match self.end {
                Point::NegInf => Truth::True,
                _ => Truth::False,
            },

            Point::P(start_p) => match &self.end {
                Point::NegInf => Truth::True,
                Point::P(end_p) => match (start_p.clone() - end_p.clone()).get_constant_val() {
                    Some(val) => {
                        if val >= Rat::zero() {
                            Truth::True
                        } else {
                            Truth::False
                        }
                    }
                    None => Truth::Undetermined,
                },
                Point::Inf => Truth::False,
            },

            Point::Inf => Truth::True,
        }
    }

    pub fn intersection(&self, other: &Interval) -> Option<Interval> {
        let start = match &self.start {
            Point::NegInf => other.start.clone(),
            Point::P(self_start_p) => match &other.start {
                Point::NegInf => Point::P(self_start_p.clone()),
                Point::P(other_start_p) => match self_start_p.less(other_start_p.clone()) {
                    Truth::True => Point::P(other_start_p.clone()),
                    Truth::False => Point::P(self_start_p.clone()),
                    Truth::Undetermined => return None,
                },
                Point::Inf => return None,
            },
            Point::Inf => return None,
        };

        let end = match &self.end {
            Point::NegInf => return None,
            Point::P(self_end_p) => match &other.end {
                Point::NegInf => return None,
                Point::P(other_end_p) => match self_end_p.less(other_end_p.clone()) {
                    Truth::True => Point::P(self_end_p.clone()),
                    Truth::False => Point::P(other_end_p.clone()),
                    Truth::Undetermined => return None,
                },
                Point::Inf => Point::P(self_end_p.clone()),
            },
            Point::Inf => other.end.clone(),
        };

        let r = Interval { start, end };

        match r.is_empty() {
            Truth::False => Some(r),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Interval, Point};
    use crate::poly;

    #[test]
    fn interval_contains() {
        let x = Interval::new(poly!(a), poly!(a + 2));
        assert!(x.contains(Point::P(poly!(a))).is_true());
        assert!(x.contains(Point::P(poly!(a + 1))).is_true());
        assert!(x.contains(Point::P(poly!(a + 2))).is_false());
        assert!(x.contains(Point::P(poly!(5))).is_undetermined());
        assert!(x.contains(Point::P(poly!(a + b))).is_undetermined());
        assert!(x.contains(Point::P(poly!(2 * a))).is_undetermined());
        assert!(x.contains(Point::P(poly!(a ^ 2))).is_undetermined());

        let y = Interval::new(poly!(10), poly!(20));
        assert!(y.contains(Point::P(poly!(9))).is_false());
        assert!(y.contains(Point::P(poly!(10))).is_true());
        assert!(y.contains(Point::P(poly!(15))).is_true());
        assert!(y.contains(Point::P(poly!(20))).is_false());
        assert!(y.contains(Point::P(poly!(a))).is_undetermined());

        let z = Interval::new_max(poly!(100));
        assert!(z.contains(Point::P(poly!(-999))).is_true());
        assert!(z.contains(Point::P(poly!(100))).is_false());
        assert!(z.contains(Point::P(poly!(101))).is_false());
        assert!(z.contains(Point::P(poly!(a + 99999))).is_undetermined());

        let w = Interval::new_min(poly!(a + 10));
        assert!(w.contains(Point::P(poly!(a + 11))).is_true());
        assert!(w.contains(Point::P(poly!(a))).is_false());
        assert!(w.contains(Point::P(poly!(2 * a))).is_undetermined());
    }

    #[test]
    fn intersections() {
        let x = Interval::new(poly!(0), poly!(10));
        let y = Interval::new(poly!(5), poly!(15));
        let z = Interval::new(poly!(10), poly!(20));

        assert_eq!(Some(Interval::new(poly!(5), poly!(10))), x.intersection(&y));

        assert_eq!(
            Some(Interval::new(poly!(10), poly!(15))),
            y.intersection(&z)
        );

        assert_eq!(None, x.intersection(&z));
    }
}
