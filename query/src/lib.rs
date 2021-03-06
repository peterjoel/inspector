mod std_fns;

use clouseau_core::{Error, Node, NodeIter, Queryable, Result, Value};
use itertools::process_results;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::iter;

type ValueFn = dyn for<'a> Fn(NodeIter<'a>) -> NodeIter<'a>;

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
        fns.insert(String::from("keys"), Box::new(std_fns::keys));
        fns.insert(String::from("data"), Box::new(std_fns::data));
        fns.insert(String::from("integers"), Box::new(std_fns::integers));
        fns.insert(String::from("strings"), Box::new(std_fns::strings));
        let vars = HashMap::new();
        Context { vars, fns }
    }
}

impl Context {
    pub fn with_var<V: Into<Value>, S: Into<String>>(mut self, name: S, val: V) -> Self {
        self.vars.insert(name.into(), val.into());
        self
    }

    pub fn with_fun<F>(mut self, name: String, fun: F) -> Self
    where
        F: for<'a> Fn(NodeIter<'a>) -> NodeIter<'a> + 'static,
    {
        self.fns.insert(name, Box::new(fun) as _);
        self
    }

    pub fn exec<'a>(
        &'a self,
        q: &'a Query,
        root: &'a dyn Queryable,
    ) -> impl Iterator<Item = Box<dyn fmt::Display + 'a>> + 'a {
        let ctx = ContextInner {
            ctx: self,
            root: root.into(),
        };
        q.exec(ctx)
    }

    pub fn function_names(&self) -> impl Iterator<Item = String> + '_ {
        self.fns.keys().cloned()
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

#[derive(Clone)]
pub struct ContextInner<'a> {
    root: Node<'a>,
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
pub enum Segment {
    Root,
    Current,
    Child(KeyExpr),
    Children,
    Descendants,
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
    path: Path,
    compare: Compare,
    rhs: OperatorRhs,
    match_type: MatchType,
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
    Neq,
    GreaterThan,
    GreaterThanEq,
}

#[derive(Debug)]
pub enum KeyExpr {
    Literal(Value),
    Var(String),
}

impl Query {
    pub fn new(path: Path) -> Self {
        Self { path }
    }

    pub fn exec<'a>(
        &'a self,
        ctx: ContextInner<'a>,
    ) -> impl Iterator<Item = Box<dyn fmt::Display + 'a>> + 'a {
        self.path
            .exec(ctx.clone(), Some(ctx.root))
            .map(|res| match res {
                Ok(node) => Box::new(node) as _,
                Err(err) => Box::new(format!("Error: {}", err)) as _,
            })
    }
}

impl Path {
    pub fn add_selector(&mut self, selector: Selector) {
        self.selectors.push(selector);
    }

    pub fn exec<'a>(&'a self, ctx: ContextInner<'a>, q: Option<Node<'a>>) -> NodeIter<'a> {
        dbg!(&q);
        if let Some(init) = q
            .or_else(|| {
                if self.is_from_root() {
                    Some(ctx.root.clone())
                } else {
                    None
                }
            })
            .map(NodeIter::one_node)
        {
            NodeIter(Box::from(
                self.selectors
                    .iter()
                    .fold(init, move |acc, selector| selector.exec(ctx.clone(), acc)),
            ))
        } else {
            NodeIter::empty()
        }
    }

    fn is_from_root(&self) -> bool {
        matches!(
            self.selectors.first(),
            Some(Selector::Segment(Segment::Root))
        )
    }

    fn is_dot(&self) -> bool {
        self.selectors.len() == 1
            && matches!(self.selectors[0], Selector::Segment(Segment::Current))
    }
}

impl Selector {
    pub fn exec<'a>(&'a self, ctx: ContextInner<'a>, values: NodeIter<'a>) -> NodeIter<'a> {
        match self {
            Selector::Segment(segment) => {
                NodeIter(Box::from(values.flat_map(move |node| match node {
                    e @ Err(_) => NodeIter(Box::from(iter::once(e))),
                    Ok(node) => segment.exec(ctx.clone(), node),
                })))
            }
            Selector::Filter(pred) => {
                NodeIter(Box::from(values.flat_map(move |node| match node {
                    e @ Err(_) => NodeIter(Box::from(iter::once(e))),
                    Ok(node) => pred.exec(ctx.clone(), node),
                })))
            }
            Selector::Call(call) => call.exec(ctx, values),
        }
    }
}

impl Segment {
    pub fn exec<'a>(&'a self, ctx: ContextInner<'a>, node: Node<'a>) -> NodeIter<'a> {
        if let Node::Queryable(q) = node {
            match self {
                Self::Child(s) => {
                    if let Some(member) = match s {
                        KeyExpr::Literal(value) => q.member(value),
                        KeyExpr::Var(name) => {
                            if let Some(value) = ctx.var(name) {
                                q.member(&value)
                            } else {
                                return NodeIter::one(Err(Error::VarNotFound));
                            }
                        }
                    } {
                        NodeIter::one_node(member)
                    } else {
                        NodeIter::empty()
                    }
                }
                Self::Children => q.all(),
                Self::Current => NodeIter::one_dyn_queryable(q),
                Self::Root => NodeIter::one_node(ctx.root),
                Self::Descendants => q.descendants(),
            }
        } else {
            NodeIter::from_nodes(iter::once(Err(Error::ExpectedNode)))
        }
    }
}

impl Call {
    pub fn exec<'a>(&'a self, ctx: ContextInner<'a>, values: NodeIter<'a>) -> NodeIter<'a> {
        if let Some(f) = ctx.fun(&self.0) {
            f(values)
        } else {
            NodeIter::one(Err(Error::FnNotFound))
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

    fn exec<'a>(&self, ctx: ContextInner<'a>, node: Node<'a>) -> NodeIter<'a> {
        match node {
            Node::Queryable(_) => match self.pred(
                ctx.clone(),
                self.path
                    .exec(ctx, Some(node.clone()))
                    .map(|n| n.and_then(Value::try_from)),
                Some(node.clone()),
            ) {
                Ok(true) => NodeIter::one_node(node),
                Ok(false) => NodeIter::empty(),
                Err(err) => NodeIter::one(Err(err)),
            },
            Node::Value(value) => {
                if self.path.is_dot() {
                    match self.pred(ctx, iter::once(Ok(value.clone())), None) {
                        Ok(true) => NodeIter::one_value(value),
                        Ok(false) => NodeIter::empty(),
                        Err(err) => NodeIter::one(Err(err)),
                    }
                } else {
                    NodeIter::one(Err(Error::PathOnValue))
                }
            }
        }
    }

    fn pred<'a>(
        &self,
        ctx: ContextInner<'a>,
        lhs: impl Iterator<Item = Result<Value>> + 'a,
        node: Option<Node<'a>>,
    ) -> Result<bool> {
        Ok(match &self.rhs {
            OperatorRhs::Path(path) => {
                process_results(lhs, |mut iter| {
                    // iter.any() or iter.all(), but with results in the predicate
                    let is_all = match self.match_type {
                        MatchType::All => true,
                        MatchType::Any => false,
                    };
                    iter.try_fold(is_all, |acc, v| {
                        if acc != is_all {
                            // short-circuit most of the calculation
                            return Ok(!is_all);
                        }
                        let mut rhs = path
                            .exec(ctx.clone(), node.clone())
                            .map(|n| n.and_then(Value::try_from))
                            .peekable();
                        if rhs.peek().is_none() {
                            Err(Error::Empty("[Predicate]"))
                        } else {
                            for r in rhs {
                                if is_all != self.compare.compare_values(&v, &r?) {
                                    return Ok(!is_all);
                                }
                            }
                            Ok(acc)
                        }
                    })
                })??
            }
            OperatorRhs::Value(value) => process_results(lhs, |mut iter| {
                iter.any(|v| self.compare.compare_values(&v, value))
            })?,
            OperatorRhs::Var(ident) => {
                if let Some(value) = ctx.var(ident) {
                    process_results(lhs, |mut iter| {
                        iter.any(|v| self.compare.compare_values(&v, &value))
                    })?
                } else {
                    return Err(Error::VarNotFound);
                }
            }
        })
    }
}

impl OperatorRhs {
    pub fn exec<'a>(&'a self, ctx: ContextInner<'a>, q: Node<'a>) -> NodeIter<'a> {
        match self {
            OperatorRhs::Value(value) => NodeIter::one_value(value.clone()),
            OperatorRhs::Path(path) => path.exec(ctx, Some(q)),
            OperatorRhs::Var(name) => {
                NodeIter::one(ctx.var(name).map(Node::from).ok_or(Error::VarNotFound))
            }
        }
    }
}

impl Compare {
    fn compare_values(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::String(a), Value::String(b)) => match self {
                Compare::LessThan => a < b,
                Compare::LessThanEq => a <= b,
                Compare::Eq => a == b,
                Compare::Neq => a != b,
                Compare::GreaterThan => a > b,
                Compare::GreaterThanEq => a >= b,
            },
            (Value::Int(a), Value::Int(b)) => match self {
                Compare::LessThan => a < b,
                Compare::LessThanEq => a <= b,
                Compare::Eq => a == b,
                Compare::Neq => a != b,
                Compare::GreaterThan => a > b,
                Compare::GreaterThanEq => a >= b,
            },
            _ => false,
        }
    }
}
