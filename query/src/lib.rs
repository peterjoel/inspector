mod std_fns;

use clouseau_core::{Error, Node, NodeOrValue, NodeOrValueIter, Queryable, Result, Value};
use itertools::process_results;
use std::collections::HashMap;
use std::fmt;
use std::iter;

type ValueFn = dyn for<'a, 'q> Fn(NodeOrValueIter<'a, 'q>) -> NodeOrValueIter<'a, 'q>;

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
        F: for<'a, 'q> Fn(NodeOrValueIter<'a, 'q>) -> NodeOrValueIter<'a, 'q> + 'static,
    {
        self.fns.insert(name, Box::new(fun) as _);
        self
    }

    pub fn exec<'a>(
        &'a self,
        q: &'a Query,
        root: &'a dyn Queryable<'a>,
    ) -> impl Iterator<Item = Box<dyn fmt::Display + 'a>> + 'a {
        let ctx = ContextInner { ctx: self, root };
        q.exec(ctx)
    }
}

impl<'a> ContextInner<'a> {
    fn var(&self, name: &str) -> Option<Value> {
        self.ctx.vars.get(name).cloned()
    }

    fn fun(&self, name: &str) -> Option<&ValueFn> {
        self.ctx.fns.get(name).map(|f| &**f)
    }
}

#[derive(Clone, Copy)]
pub struct ContextInner<'a> {
    root: Node<'a, 'a>,
    ctx: &'a Context,
}

#[derive(Default, Debug)]
pub struct Query {
    path: Path,
}

#[derive(Debug, Default)]
pub struct Path {
    selectors: Vec<Selector>,
}

#[derive(Debug)]
pub enum Selector {
    Segment(Segment),
    Filter(Pred),
    Call(Call),
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
pub struct Call(String);

impl Call {
    pub fn new(name: String) -> Self {
        Call(name)
    }
}

#[derive(Debug)]
pub struct Pred {
    match_type: MatchType,
    path: Path,
    compare: Compare,
    rhs: OperatorRhs,
}

#[derive(Debug)]
pub enum OperatorRhs {
    Path(Path),
    Value(Value),
    Var(String),
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
        self.path.exec(ctx, ctx.root).map(|res| match res {
            Ok(node_or_value) => Box::new(node_or_value) as _,
            Err(err) => Box::new(format!("Error: {}", err)) as _,
        })
    }
}

impl Path {
    pub fn add_selector(&mut self, selector: Selector) {
        self.selectors.push(selector);
    }

    pub fn exec<'a: 'q, 'q>(
        &'a self,
        ctx: ContextInner<'a>,
        q: Node<'a, 'q>,
    ) -> NodeOrValueIter<'a, 'q> {
        let init = NodeOrValueIter::one_node(q);
        NodeOrValueIter::from_raw(
            self.selectors
                .iter()
                .fold(init, move |acc, selector| selector.exec(ctx, acc)),
        )
    }
}

impl Selector {
    pub fn exec<'a: 'q, 'q>(
        &'a self,
        ctx: ContextInner<'a>,
        values: NodeOrValueIter<'a, 'q>,
    ) -> NodeOrValueIter<'a, 'q> {
        match self {
            Selector::Segment(segment) => {
                NodeOrValueIter::from_raw(values.flat_map(move |node_or_val| match node_or_val {
                    e @ Err(_) => NodeOrValueIter::from_raw(iter::once(e)),
                    Ok(node_or_value) => segment.exec(ctx, node_or_value),
                }))
            }
            Selector::Filter(pred) => {
                NodeOrValueIter::from_raw(values.flat_map(move |node_or_val| match node_or_val {
                    e @ Err(_) => NodeOrValueIter::from_raw(iter::once(e)),
                    Ok(node_or_value) => pred.exec(ctx, node_or_value),
                }))
            }
            Selector::Call(call) => call.exec(ctx, values),
        }
    }
}

impl Segment {
    pub fn exec<'a: 'q, 'q>(
        &'a self,
        ctx: ContextInner<'a>,
        node_or_value: NodeOrValue<'a, 'q>,
    ) -> NodeOrValueIter<'a, 'q> {
        if let NodeOrValue::Node(q) = node_or_value {
            self.segment_type.exec(ctx, q)
        } else {
            NodeOrValueIter::from_raw(iter::once(Err(Error::ExpectedNode)))
        }
    }
}

impl Call {
    pub fn exec<'a: 'q, 'q>(
        &'a self,
        ctx: ContextInner<'a>,
        values: NodeOrValueIter<'a, 'q>,
    ) -> NodeOrValueIter<'a, 'q> {
        if let Some(f) = ctx.fun(&self.0) {
            f(values)
        } else {
            NodeOrValueIter::one(Err(Error::VarNotFound))
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

    fn exec<'a: 'q, 'q>(
        &'a self,
        ctx: ContextInner<'a>,
        node_or_value: NodeOrValue<'a, 'q>,
    ) -> NodeOrValueIter<'a, 'q> {
        match node_or_value {
            NodeOrValue::Node(node) => match self.filter(ctx, node) {
                Ok(true) => NodeOrValueIter::one_node(node),
                Ok(false) => NodeOrValueIter::empty(),
                Err(err) => NodeOrValueIter::one(Err(err)),
            },
            NodeOrValue::Value(_) => NodeOrValueIter::one(Err(Error::ExpectedNode)),
        }
    }

    fn filter<'a: 'q, 'q>(&'a self, ctx: ContextInner<'a>, node: Node<'a, 'q>) -> Result<bool> {
        let mut lhs = self
            .path
            .exec(ctx, node)
            .map(|n| n.and_then(|n| n.try_into_value()))
            .peekable();
        Ok(match self.match_type {
            MatchType::Any => match &self.rhs {
                OperatorRhs::Path(path) => {
                    lhs.peek().is_some()
                        && process_results(lhs, |mut iter| {
                            // iter.any(), but with results in the predicate
                            iter.try_fold(false, |acc, v| {
                                if acc {
                                    // short-circuit most of the calculation if we already found one
                                    return Ok(true);
                                }
                                let mut rhs = path
                                    .exec(ctx, node)
                                    .map(|n| n.and_then(|n| n.try_into_value()))
                                    .peekable();
                                if rhs.peek().is_none() {
                                    Ok(acc)
                                } else {
                                    for r in rhs {
                                        if self.compare.compare(&v, &r?) {
                                            return Ok(true);
                                        }
                                    }
                                    Ok(acc)
                                }
                            })
                        })??
                }
                OperatorRhs::Value(value) => {
                    lhs.peek().is_some()
                        && process_results(lhs, |mut iter| {
                            iter.any(|v| self.compare.compare(&v, value))
                        })?
                }
                OperatorRhs::Var(ident) => {
                    if let Some(value) = ctx.var(&ident) {
                        lhs.peek().is_some()
                            && process_results(lhs, |mut iter| {
                                iter.any(|v| self.compare.compare(&v, &value))
                            })?
                    } else {
                        false
                    }
                }
            },
            MatchType::All => match &self.rhs {
                OperatorRhs::Path(path) => {
                    lhs.peek().is_some()
                        && process_results(lhs, |mut iter| {
                            iter.try_fold(true, |acc, v| {
                                if !acc {
                                    return Ok(false);
                                }
                                let mut rhs = path
                                    .exec(ctx, node)
                                    .map(|n| n.and_then(|n| n.try_into_value()))
                                    .peekable();
                                if rhs.peek().is_none() {
                                    Ok(false)
                                } else {
                                    for r in rhs {
                                        if !self.compare.compare(&v, &r?) {
                                            return Ok(false);
                                        }
                                    }
                                    Ok(true)
                                }
                            })
                        })??
                }
                OperatorRhs::Value(value) => {
                    lhs.peek().is_some()
                        && process_results(lhs, |mut iter| {
                            iter.any(|v| self.compare.compare(&v, value))
                        })?
                }
                OperatorRhs::Var(ident) => {
                    if let Some(value) = ctx.var(&ident) {
                        lhs.peek().is_some()
                            && process_results(lhs, |mut iter| {
                                iter.any(|v| self.compare.compare(&v, &value))
                            })?
                    } else {
                        false
                    }
                }
            },
        })
    }
}

impl OperatorRhs {
    pub fn exec<'a: 'q, 'q>(
        &'a self,
        ctx: ContextInner<'a>,
        q: Node<'a, 'q>,
    ) -> NodeOrValueIter<'a, 'q> {
        match self {
            OperatorRhs::Value(value) => NodeOrValueIter::one_value(value.clone()),
            OperatorRhs::Path(path) => path.exec(ctx, q),
            OperatorRhs::Var(name) => NodeOrValueIter::one(
                ctx.var(name)
                    .map(NodeOrValue::from)
                    .ok_or(Error::VarNotFound),
            ),
        }
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

impl SegmentType {
    pub fn exec<'a: 'q, 'q>(
        &'a self,
        ctx: ContextInner<'a>,
        q: Node<'a, 'q>,
    ) -> NodeOrValueIter<'a, 'q> {
        match self {
            SegmentType::Child(s) => {
                if let Some(q) = q.member(&s) {
                    NodeOrValueIter::one_node(q)
                } else {
                    NodeOrValueIter::empty()
                }
            }
            SegmentType::Children => q.all(),
            SegmentType::Current => NodeOrValueIter::one_node(q),
            SegmentType::Root => NodeOrValueIter::one_node(ctx.root),
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
