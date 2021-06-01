mod std_fns;

use clouseau_core::{Queryable, TreeIter, Value, ValueIter};
use std::collections::HashMap;
use std::fmt;
use std::iter;

type ValueFn = dyn Fn(ValueIter<'_>) -> ValueIter<'_>;

pub struct Context {
    vars: HashMap<String, Value>,
    fns: HashMap<String, Box<ValueFn>>,
}

impl Default for Context {
    fn default() -> Self {
        let mut fns: HashMap<String, Box<ValueFn>> = HashMap::new();
        fns.insert(String::from("sum"), Box::new(std_fns::sum));
        fns.insert(String::from("count"), Box::new(std_fns::count));
        fns.insert(String::from("min"), Box::new(std_fns::min));
        fns.insert(String::from("max"), Box::new(std_fns::max));
        fns.insert(String::from("distinct"), Box::new(std_fns::distinct));
        fns.insert(String::from("first"), Box::new(std_fns::first));
        fns.insert(String::from("last"), Box::new(std_fns::last));
        let vars = HashMap::new();
        Context { fns, vars }
    }
}

impl Context {
    pub fn with_var<V: Into<Value>, S: Into<String>>(mut self, name: S, val: V) -> Self {
        self.vars.insert(name.into(), val.into());
        self
    }

    pub fn with_fun<F>(mut self, name: String, fun: F) -> Self
    where
        F: Fn(ValueIter<'_>) -> ValueIter<'_> + 'static,
    {
        self.fns.insert(name, Box::new(fun) as _);
        self
    }

    pub fn exec<'a>(
        &'a self,
        q: &'a Query,
        data: &'a dyn Queryable<'a>,
    ) -> impl Iterator<Item = Box<dyn fmt::Display + 'a>> + 'a {
        let ctx = ContextInner {
            ctx: self,
            root: data,
        };
        q.exec(ctx)
    }
}

impl<'a> ContextInner<'a> {
    fn var(&self, name: &str) -> Option<&Value> {
        self.ctx.vars.get(name)
    }

    fn fun(&self, name: &str) -> Option<&ValueFn> {
        self.ctx.fns.get(name).map(|f| &**f)
    }
}

#[derive(Clone, Copy)]
pub struct ContextInner<'a> {
    root: &'a dyn Queryable<'a>,
    ctx: &'a Context,
}

#[derive(Default, Debug)]
pub struct Query {
    path: Path,
}

#[derive(Debug, Default)]
pub struct Path {
    segments: Vec<Segment>,
    functions: Vec<Call>,
}

#[derive(Debug)]
pub struct Call(String);

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
pub enum OperatorRhs {
    Path(Path),
    Value(Value),
    Var(String),
}

#[derive(Debug)]
pub struct Pred {
    match_type: MatchType,
    path: Path,
    compare: Compare,
    rhs: OperatorRhs,
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

    pub fn exec<'a>(
        &'a self,
        ctx: ContextInner<'a>,
    ) -> impl Iterator<Item = Box<dyn fmt::Display + 'a>> + 'a {
        self.path.exec(ctx, ctx.root).map(|v| Box::new(v) as _)
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
    pub fn append_segment(mut self, segment: Segment) -> Self {
        self.segments.push(segment);
        self
    }

    pub fn append_filter(mut self, filter: Pred) -> Self {
        if let Some(segment) = self.segments.last_mut() {
            segment.filter = Some(filter);
        }
        self
    }

    pub fn append_function_call(mut self, name: String) -> Self {
        self.functions.push(Call(name));
        self
    }

    pub fn exec<'a>(&'a self, ctx: ContextInner<'a>, q: &'a dyn Queryable<'a>) -> ValueIter<'a> {
        let init = TreeIter(Box::new(iter::once(q as _)));
        let path_result = ValueIter::from_values(
            self.segments.iter().fold(init, move |i, segment| {
                TreeIter(Box::from(i.flat_map(move |qq| segment.exec(ctx, qq))))
            }), // .flat_map(|qq| qq.data().into_iter()),
        );
        self.functions
            .iter()
            .fold(path_result, |acc, f| f.exec(ctx, acc))
    }
}

impl Segment {
    pub fn exec<'a>(&'a self, ctx: ContextInner<'a>, q: &'a dyn Queryable<'a>) -> TreeIter<'a> {
        if let Some(pred) = &self.filter {
            TreeIter(Box::from(
                self.segment_type
                    .exec(ctx, q)
                    .filter(move |&v| pred.filter(ctx, v)),
            ))
        } else {
            self.segment_type.exec(ctx, q)
        }
    }
}

impl Call {
    pub fn exec<'a>(&self, ctx: ContextInner<'a>, values: ValueIter<'a>) -> ValueIter<'a> {
        if let Some(f) = ctx.fun(&self.0) {
            f(values)
        } else {
            ValueIter::once(format!("** Function `{}` not found **", &self.0))
        }
    }
}

impl Pred {
    pub fn new(path: Path, compare: Compare, rhs: OperatorRhs, match_type: MatchType) -> Pred {
        Self {
            path,
            compare,
            rhs,
            match_type,
        }
    }
    pub fn filter<'a>(&'a self, ctx: ContextInner<'a>, q: &'a dyn Queryable<'a>) -> bool {
        let mut values = self
            .path
            .exec(ctx, q)
            .filter_map(|v| v.into_value())
            .peekable();
        match self.match_type {
            MatchType::Any => match &self.rhs {
                OperatorRhs::Path(path) => {
                    values.peek().is_some()
                        && values.any(|v| {
                            let mut rhs =
                                path.exec(ctx, q).filter_map(|v| v.into_value()).peekable();
                            rhs.peek().is_some() && rhs.any(|r| self.compare.compare(&v, &r))
                        })
                }
                OperatorRhs::Value(value) => {
                    values.peek().is_some() && values.any(|v| self.compare.compare(&v, value))
                }
                OperatorRhs::Var(ident) => {
                    if let Some(value) = ctx.var(&ident) {
                        values.peek().is_some() && values.any(|v| self.compare.compare(&v, value))
                    } else {
                        false
                    }
                }
            },
            MatchType::All => match &self.rhs {
                OperatorRhs::Path(path) => {
                    values.peek().is_some()
                        && values.any(|v| {
                            let mut rhs =
                                path.exec(ctx, q).filter_map(|v| v.into_value()).peekable();
                            rhs.peek().is_some() && rhs.all(|r| self.compare.compare(&v, &r))
                        })
                }
                OperatorRhs::Value(value) => {
                    values.peek().is_some() && values.all(|v| self.compare.compare(&v, value))
                }
                OperatorRhs::Var(ident) => {
                    if let Some(value) = ctx.var(&ident) {
                        values.peek().is_some() && values.all(|v| self.compare.compare(&v, value))
                    } else {
                        false
                    }
                }
            },
        }
    }
}

impl SegmentType {
    pub fn exec<'a>(&'a self, ctx: ContextInner<'a>, q: &'a dyn Queryable<'a>) -> TreeIter<'a> {
        match self {
            SegmentType::Child(s) => TreeIter(Box::from(q.member(&s).into_iter())),
            SegmentType::Children => q.all(),
            SegmentType::Current => TreeIter(Box::from(iter::once(q))),
            SegmentType::Root => TreeIter(Box::from(iter::once(ctx.root))),
        }
    }

    pub fn into_segment(self) -> Segment {
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
