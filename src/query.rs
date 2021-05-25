use super::{Queryable, Value};
use std::iter;

#[derive(Default)]
pub struct Path {
    segments: Vec<Segment>,
}

pub enum Segment {
    Named(String),
    All,
}

impl Path {
    pub fn append(mut self, segment: Segment) -> Self {
        self.segments.push(segment);
        self
    }

    pub fn exec<'a>(&'a self, q: &'a impl Queryable<'a>) -> impl Iterator<Item = Value> + 'a {
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
    pub fn exec<'a, Q: Queryable<'a> + ?Sized>(
        &'a self,
        q: &'a Q,
    ) -> Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a> {
        match self {
            Segment::Named(s) => Box::from(q.member(&s).into_iter()),
            Segment::All => q.all(),
        }
    }
}
