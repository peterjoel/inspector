use clouseau_core::{Queryable, Value};
use std::iter;

#[derive(Debug, Clone, Copy)]
pub struct Context<'a> {
    root: &'a dyn Queryable<'a>,
}

#[derive(Default, Debug)]
pub struct Query {
    path: Path,
}

#[derive(Debug, Default)]
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
    Current,
    Child(Value),
    Children,
}

#[derive(Debug)]
pub enum PathOrValue {
    Path(Path),
    Value(Value),
}

#[derive(Debug)]
pub struct Pred {
    match_type: MatchType,
    path: Path,
    compare: Compare,
    rhs: PathOrValue,
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
    pub fn new(path: Path) -> Self {
        Self { path }
    }

    pub fn exec<'a>(&'a self, q: &'a dyn Queryable<'a>) -> impl Iterator<Item = Value> + 'a {
        let ctx = Context { root: q };
        self.path.exec(ctx, q)
    }
}

impl Compare {
    fn compare(&self, a: &Value, b: &Value) -> bool {
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

    pub fn exec<'a>(
        &'a self,
        ctx: Context<'a>,
        q: &'a dyn Queryable<'a>,
    ) -> impl Iterator<Item = Value> + 'a {
        let init: Box<dyn Iterator<Item = &'a dyn Queryable<'a>>> = Box::new(iter::once(q as _));
        self.segments
            .iter()
            .fold(init, move |i, segment| {
                Box::from(i.flat_map(move |qq| segment.exec(ctx, qq)))
            })
            .flat_map(|qq| Box::from(qq.data().into_iter()))
    }
}

impl Segment {
    pub fn exec<'a>(
        &'a self,
        ctx: Context<'a>,
        q: &'a dyn Queryable<'a>,
    ) -> Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a> {
        if let Some(pred) = &self.filter {
            Box::from(
                self.segment_type
                    .exec(ctx, q)
                    .filter(move |&v| pred.filter(ctx, v)),
            )
        } else {
            self.segment_type.exec(ctx, q)
        }
    }
}

impl Pred {
    pub fn new(path: Path, compare: Compare, rhs: PathOrValue, match_type: MatchType) -> Pred {
        Self {
            path,
            compare,
            rhs,
            match_type,
        }
    }
    pub fn filter<'a>(&'a self, ctx: Context<'a>, q: &'a dyn Queryable<'a>) -> bool {
        let mut values = self.path.exec(ctx, q).peekable();
        match self.match_type {
            MatchType::Any => match &self.rhs {
                PathOrValue::Path(path) => {
                    values.peek().is_some()
                        && values.any(|v| {
                            let mut rhs = path.exec(ctx, q).peekable();
                            rhs.peek().is_some() && rhs.any(|r| self.compare.compare(&v, &r))
                        })
                }
                PathOrValue::Value(value) => {
                    values.peek().is_some() && values.any(|v| self.compare.compare(&v, value))
                }
            },
            MatchType::All => match &self.rhs {
                PathOrValue::Path(path) => {
                    values.peek().is_some()
                        && values.any(|v| {
                            let mut rhs = path.exec(ctx, q).peekable();
                            rhs.peek().is_some() && rhs.all(|r| self.compare.compare(&v, &r))
                        })
                }
                PathOrValue::Value(value) => {
                    values.peek().is_some() && values.all(|v| self.compare.compare(&v, value))
                }
            },
        }
    }
}

impl SegmentType {
    pub fn exec<'a>(
        &'a self,
        ctx: Context<'a>,
        q: &'a dyn Queryable<'a>,
    ) -> Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a> {
        match self {
            SegmentType::Child(s) => Box::from(q.member(&s).into_iter()),
            SegmentType::Children => q.all(),
            SegmentType::Current => Box::from(iter::once(q)),
            SegmentType::Root => Box::from(iter::once(ctx.root)),
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
