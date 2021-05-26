use super::{Queryable, Value};
use std::iter;

#[derive(Default, Debug)]
pub struct Path {
    segments: Vec<Segment>,
}

#[derive(Debug)]
pub struct Segment {
    segment_type: SegmentType,
    filter: Option<Pred>,
}

#[derive(Debug)]
pub enum SegmentType {
    Named(String),
    All,
}

#[derive(Debug)]
pub struct Pred {
    match_all: bool,
    path: Path,
    compare: Compare,
    value: Value,
}

#[derive(Debug)]
pub enum Compare {
    LessThan,
    LessThanEq,
    Eq,
    GreaterThan,
    GreaterThanEq,
}

impl Compare {
    fn compare(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::String(a), Value::String(b)) => match self {
                Compare::LessThan => a < b,
                Compare::LessThanEq => a <= b,
                Compare::Eq => a <= b,
                Compare::GreaterThan => a > b,
                Compare::GreaterThanEq => a >= b,
            },
            (Value::Int(a), Value::Int(b)) => match self {
                Compare::LessThan => a < b,
                Compare::LessThanEq => a <= b,
                Compare::Eq => a <= b,
                Compare::GreaterThan => a > b,
                Compare::GreaterThanEq => a >= b,
            },
            _ => false,
        }
    }
}

impl Path {
    pub fn append(mut self, segment: Segment) -> Self {
        self.segments.push(segment);
        self
    }

    pub fn exec<'a>(&'a self, q: &'a dyn Queryable<'a>) -> impl Iterator<Item = Value> + 'a {
        let init: Box<dyn Iterator<Item = &'a dyn Queryable<'a>>> = Box::new(iter::once(q as _));
        self.segments
            .iter()
            .fold(init, move |i, segment| {
                Box::from(i.flat_map(move |q| segment.exec(q)))
            })
            .flat_map(|q| Box::from(q.data().into_iter()))
    }
}

impl Segment {
    pub fn with_filter(mut self, pred: Pred) -> Self {
        self.filter = Some(pred);
        self
    }
    pub fn exec<'a>(
        &'a self,
        q: &'a dyn Queryable<'a>,
    ) -> Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a> {
        if let Some(pred) = &self.filter {
            Box::from(self.segment_type.exec(q).filter(move |&v| pred.filter(v)))
        } else {
            self.segment_type.exec(q)
        }
    }
}

impl Pred {
    pub fn new(path: Path, compare: Compare, value: Value) -> Pred {
        Self {
            match_all: false,
            path,
            compare,
            value,
        }
    }
    pub fn filter<'a>(&'a self, q: &'a dyn Queryable<'a>) -> bool {
        let mut values = self.path.exec(q);
        if self.match_all {
            values.all(|v| self.compare.compare(&v, &self.value))
        } else {
            values.any(|v| self.compare.compare(&v, &self.value))
        }
    }
}

impl SegmentType {
    pub fn exec<'a>(
        &'a self,
        q: &'a dyn Queryable<'a>,
    ) -> Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a> {
        match self {
            SegmentType::Named(s) => Box::from(q.member(&s).into_iter()),
            SegmentType::All => q.all(),
        }
    }

    pub fn to_segment(self) -> Segment {
        Segment {
            segment_type: self,
            filter: None,
        }
    }
}

impl From<SegmentType> for Segment {
    fn from(segment_type: SegmentType) -> Self {
        Segment {
            segment_type,
            filter: None,
        }
    }
}
