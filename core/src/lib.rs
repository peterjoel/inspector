#[cfg(feature = "arrayvec")]
mod arrayvec;
mod float;
mod std_impls;
#[cfg(feature = "uuid")]
mod uuid;

use float::WellBehavedF64;
use std::convert::TryFrom;
use std::fmt;
use std::iter;
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Expected value")]
    ExpectedValue,
    #[error("Expected node")]
    ExpectedNode,
    // TODO include names of actual and expected types
    #[error("Type error")]
    TypeError,
    #[error("Expected integer")]
    ExpectedInt,
    #[error("Variable not found")]
    VarNotFound,
    #[error("Function not found")]
    FnNotFound,
    #[error("Cannot execute a path on a value")]
    PathOnValue,
    #[error("Input is empty in {0}")]
    Empty(&'static str),
    #[error("Conversion failed: {0}")]
    Conversion(#[from] Box<dyn std::error::Error>),
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Clone)]
pub enum Node<'a> {
    Queryable(&'a (dyn Queryable + 'a)),
    Value(Value),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Value {
    String(String),
    Int(i64),
    Float(WellBehavedF64),
    Bool(bool),
    Array(Array),
}

pub struct ValueIter<'a>(pub Box<dyn Iterator<Item = Value> + 'a>);

impl<'a> ValueIter<'a> {
    pub fn empty() -> Self {
        ValueIter(Box::new(iter::empty()))
    }

    pub fn once<V: Into<Value>>(val: V) -> Self {
        ValueIter(Box::new(iter::once(val.into())))
    }

    pub fn from_values<T: 'a>(values: impl IntoIterator<Item = T> + 'a) -> Self
    where
        T: Into<Value>,
    {
        ValueIter(Box::from(values.into_iter().map(|v| v.into())))
    }
}

impl<'a> Iterator for ValueIter<'a> {
    type Item = Value;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

pub struct NodeIter<'a>(pub Box<dyn Iterator<Item = Result<Node<'a>>> + 'a>);

impl<'a> NodeIter<'a> {
    pub fn empty() -> NodeIter<'a> {
        NodeIter(Box::new(iter::empty()))
    }

    pub fn from_values<I>(values: I) -> Self
    where
        I: IntoIterator + 'a,
        I::Item: Into<Value>,
    {
        NodeIter(Box::new(
            values
                .into_iter()
                .map(|value| Ok(Node::Value(value.into()))),
        ))
    }

    pub fn from_queryables<Q>(queryables: impl IntoIterator<Item = &'a Q> + 'a) -> Self
    where
        Q: Queryable + 'a,
    {
        NodeIter(Box::from(
            queryables.into_iter().map(|q| Ok(Node::Queryable(q as _))),
        ))
    }

    pub fn from_dyn_queryables<I>(nodes: I) -> Self
    where
        I: IntoIterator<Item = &'a dyn Queryable> + 'a,
    {
        NodeIter(Box::from(
            nodes.into_iter().map(|node| Ok(Node::Queryable(node))),
        ))
    }

    pub fn from_nodes<I>(nodes: I) -> Self
    where
        I: IntoIterator<Item = Result<Node<'a>>> + 'a,
    {
        NodeIter(Box::new(nodes.into_iter()))
    }

    pub fn one(item: Result<Node<'a>>) -> Self {
        NodeIter(Box::from(iter::once(item)))
    }

    pub fn one_value(value: Value) -> Self {
        Self::one(Ok(value.into()))
    }

    pub fn one_node(node: Node<'a>) -> Self {
        Self::one(Ok(node))
    }

    pub fn one_dyn_queryable(node: &'a dyn Queryable) -> Self {
        Self::one_node(node.into())
    }
}

impl<'a> Iterator for NodeIter<'a> {
    type Item = Result<Node<'a>>;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a> From<Value> for Node<'a> {
    fn from(v: Value) -> Self {
        Node::Value(v)
    }
}

impl<'a> From<&'a dyn Queryable> for Node<'a> {
    fn from(n: &'a dyn Queryable) -> Self {
        Node::Queryable(n)
    }
}

impl<'a> fmt::Display for Node<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Queryable(node) => write!(f, "{}", node),
            Node::Value(value) => write!(f, "{}", value),
        }
    }
}
pub trait Queryable {
    fn keys(&self) -> ValueIter<'_> {
        ValueIter::empty()
    }
    fn member<'f>(&self, _: &'f Value) -> Option<Node<'_>> {
        None
    }
    fn all(&self) -> NodeIter<'_> {
        NodeIter::empty()
    }
    fn descendants(&self) -> NodeIter<'_> {
        if self.all().next().is_some() {
            NodeIter::from_nodes(self.all().chain(self.all().flat_map(|node| {
                if let Ok(node) = node {
                    match node {
                        Node::Queryable(node) => node.descendants(),
                        Node::Value(value) => NodeIter::one_value(value),
                    }
                } else {
                    NodeIter::empty()
                }
            })))
        } else {
            self.all()
        }
    }
    fn name(&self) -> &'static str;
    fn data(&self) -> Option<Value> {
        None
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Array(pub Rc<[Value]>);

impl Array {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IntoIterator for Array {
    type Item = Value;
    type IntoIter = ArrayIter;
    fn into_iter(self) -> ArrayIter {
        ArrayIter {
            current: 0,
            array: self,
        }
    }
}

pub struct ArrayIter {
    current: usize,
    array: Array,
}

impl Iterator for ArrayIter {
    type Item = Value;
    fn next(&mut self) -> Option<Value> {
        if self.current == self.array.len() {
            None
        } else {
            let index = self.current;
            self.current += 1;
            Some(self.array.0[index].clone())
        }
    }
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("{")?;
        let mut it = self.clone().into_iter().peekable();
        while let Some(val) = it.next() {
            let mut format = |args| {
                if it.peek().is_some() {
                    write!(f, "{}, ", args)
                } else {
                    write!(f, "{}", args)
                }
            };
            if let Value::String(s) = val {
                format(format_args!(r#""{}""#, s))
            } else {
                format(format_args!("{}", val))
            }?;
        }
        f.write_str("}")
    }
}

impl<'a> TryFrom<Node<'a>> for Value {
    type Error = Error;
    fn try_from(node: Node<'a>) -> Result<Value, Self::Error> {
        match node {
            Node::Value(value) => Ok(value),
            Node::Queryable(node) => node.data().ok_or(Error::ExpectedValue),
        }
    }
}

impl<'a> TryFrom<Node<'a>> for i64 {
    type Error = Error;
    fn try_from(node: Node<'a>) -> Result<i64> {
        Value::try_from(node)
            .map_err(|_| Error::ExpectedInt)
            .and_then(i64::try_from)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(v) => write!(f, "{}", v),
            Value::Int(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Array(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
        }
    }
}

impl<'a> fmt::Debug for &'a dyn Queryable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{} {:?}]", self.name(), self.data())
    }
}

impl<'a> fmt::Display for &'a dyn Queryable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(data) = self.data() {
            write!(f, "{}", data)
        } else {
            write!(f, "[{}]", self.name())
        }
    }
}
