mod parser;
use super::Queryable;
pub use parser::parse_query;
use std::fmt;
use std::iter;

#[derive(Default, Debug)]
pub struct Query {
    path: Path,
}

#[derive(Debug)]
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
    Root,
    Named(String),
    All,
}

#[derive(Debug)]
pub struct Pred {
    match_type: MatchType,
    path: Path,
    compare: Compare,
    value: Value,
}

#[derive(Debug)]
pub enum MatchType {
    Any,
    All,
}

#[derive(Debug)]
pub enum Compare {
    LessThan,
    LessThanEq,
    Eq,
    GreaterThan,
    GreaterThanEq,
}

impl Query {
    pub fn exec<'a>(&'a self, q: &'a dyn Queryable<'a>) -> impl Iterator<Item = Value> + 'a {
        self.path.exec(q)
    }
}

impl Compare {
    fn compare(&self, a: &Value, b: &Value) -> bool {
        dbg!(&a, &b);
        match (a, b) {
            (Value::String(a), Value::String(b)) => match self {
                Compare::LessThan => a < b,
                Compare::LessThanEq => a <= b,
                Compare::Eq => a == b,
                Compare::GreaterThan => a > b,
                Compare::GreaterThanEq => a >= b,
            },
            (Value::Int(a), Value::Int(b)) => match self {
                Compare::LessThan => a < b,
                Compare::LessThanEq => a <= b,
                Compare::Eq => a == b,
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

    pub fn append_filter(mut self, filter: Pred) -> Self {
        if let Some(segment) = self.segments.last_mut() {
            segment.filter = Some(filter);
        }
        self
    }

    pub fn exec<'a>(&'a self, q: &'a dyn Queryable<'a>) -> impl Iterator<Item = Value> + 'a {
        let init: Box<dyn Iterator<Item = &'a dyn Queryable<'a>>> = Box::new(iter::once(q as _));
        self.segments
            .iter()
            .fold(init, move |i, segment| {
                Box::from(i.flat_map(move |qq| segment.exec(qq)))
            })
            .flat_map(|qq| Box::from(qq.data().into_iter()))
    }
}

impl Default for Path {
    fn default() -> Self {
        Path {
            segments: vec![Segment {
                segment_type: SegmentType::Root,
                filter: None,
            }],
        }
    }
}

impl Segment {
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
    pub fn new(path: Path, compare: Compare, value: Value, match_type: MatchType) -> Pred {
        Self {
            path,
            compare,
            value,
            match_type,
        }
    }
    pub fn filter<'a>(&'a self, q: &'a dyn Queryable<'a>) -> bool {
        let mut values = self.path.exec(q).peekable();
        match self.match_type {
            MatchType::Any => {
                values.peek().is_some() && values.any(|v| self.compare.compare(&v, &self.value))
            }
            MatchType::All => {
                values.peek().is_some() && values.all(|v| self.compare.compare(&v, &self.value))
            }
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
            SegmentType::Root => Box::from(iter::once(q)),
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

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum Value {
    String(String),
    Int(i64),
}

impl From<i64> for Value {
    fn from(v: i64) -> Value {
        Value::Int(v)
    }
}

impl From<u64> for Value {
    fn from(v: u64) -> Value {
        Value::Int(v as i64)
    }
}

impl From<i32> for Value {
    fn from(v: i32) -> Value {
        Value::Int(v as i64)
    }
}

impl From<u32> for Value {
    fn from(v: u32) -> Value {
        Value::Int(v as i64)
    }
}

impl From<String> for Value {
    fn from(v: String) -> Value {
        Value::String(v)
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Value {
        Value::String(v.to_string())
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(v) => write!(f, "{}", v),
            Value::Int(v) => write!(f, "{}", v),
        }
    }
}
