#[cfg(feature = "arrayvec")]
mod arrayvec;
mod float;
mod std_impls;
#[cfg(feature = "uuid")]
mod uuid;

use float::WellBehavedF64;
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

pub type Result<T> = std::result::Result<T, Error>;

pub type Node<'a, 'q> = &'a (dyn Queryable<'q> + 'a);

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

pub struct NodeIter<'a, 'q>(pub Box<dyn Iterator<Item = Node<'a, 'q>> + 'a>);

impl<'a, 'q> NodeIter<'a, 'q> {
    pub fn empty() -> NodeIter<'a, 'q> {
        NodeIter(Box::new(iter::empty()))
    }

    pub fn from_queryables<Q>(queryables: impl IntoIterator<Item = &'a Q> + 'a) -> Self
    where
        Q: Queryable<'q> + 'a,
    {
        NodeIter(Box::from(queryables.into_iter().map(|q| q as _)))
    }
}

impl<'a, 'q> Iterator for NodeIter<'a, 'q> {
    type Item = Node<'a, 'q>;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

#[derive(Debug)]
pub enum NodeOrValue<'a, 'q> {
    Node(Node<'a, 'q>),
    Value(Value),
}

impl<'a, 'q> NodeOrValue<'a, 'q> {
    pub fn try_into_int(self) -> Result<i64> {
        self.try_into_value()
            .map_err(|_| Error::ExpectedInt)
            .and_then(Value::try_into_int)
    }

    pub fn try_into_value(self) -> Result<Value> {
        match self {
            NodeOrValue::Node(node) => {
                if let Some(data) = node.data() {
                    Ok(data)
                } else {
                    Err(Error::ExpectedValue)
                }
            }
            NodeOrValue::Value(v) => Ok(v),
        }
    }

    pub fn try_into_node(self) -> Result<Node<'a, 'q>> {
        match self {
            NodeOrValue::Node(n) => Ok(n),
            NodeOrValue::Value(_) => Err(Error::ExpectedNode),
        }
    }
}

impl<'a, 'q> From<Value> for NodeOrValue<'a, 'q> {
    fn from(v: Value) -> Self {
        NodeOrValue::Value(v)
    }
}

impl<'a, 'q> From<Node<'a, 'q>> for NodeOrValue<'a, 'q> {
    fn from(n: Node<'a, 'q>) -> Self {
        NodeOrValue::Node(n)
    }
}

impl<'a, 'q> fmt::Display for NodeOrValue<'a, 'q> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NodeOrValue::Node(node) => write!(f, "{}", node),
            NodeOrValue::Value(value) => write!(f, "{}", value),
        }
    }
}

pub struct NodeOrValueIter<'a, 'q>(Box<dyn Iterator<Item = Result<NodeOrValue<'a, 'q>>> + 'a>);

impl<'a, 'q> NodeOrValueIter<'a, 'q> {
    pub fn empty() -> Self {
        NodeOrValueIter(Box::new(iter::empty()))
    }

    pub fn one(item: Result<NodeOrValue<'a, 'q>>) -> Self {
        NodeOrValueIter(Box::new(iter::once(item)))
    }

    pub fn one_node(node: Node<'a, 'q>) -> Self {
        Self::from_raw(iter::once(node).map(|n| Ok(n.into())))
    }

    pub fn one_value(value: Value) -> Self {
        Self::from_raw(iter::once(value).map(|v| Ok(v.into())))
    }

    pub fn from_queryables<I, Q>(nodes: I) -> Self
    where
        I: IntoIterator<Item = &'a Q> + 'a,
        Q: Queryable<'q> + 'a,
    {
        NodeOrValueIter(Box::new(
            nodes.into_iter().map(|node| Ok(NodeOrValue::Node(node))),
        ))
    }

    pub fn from_nodes<I>(nodes: I) -> Self
    where
        I: IntoIterator<Item = &'a dyn Queryable<'q>> + 'a,
    {
        NodeOrValueIter(Box::new(
            nodes.into_iter().map(|node| Ok(NodeOrValue::Node(node))),
        ))
    }

    pub fn from_values<I>(values: I) -> Self
    where
        I: IntoIterator + 'a,
        I::Item: Into<Value>,
    {
        NodeOrValueIter(Box::new(
            values
                .into_iter()
                .map(|value| Ok(NodeOrValue::Value(value.into()))),
        ))
    }

    pub fn from_raw<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Result<NodeOrValue<'a, 'q>>> + 'a,
    {
        NodeOrValueIter(Box::new(iter.into_iter()))
    }
}

impl<'a, 'q> Iterator for NodeOrValueIter<'a, 'q> {
    type Item = Result<NodeOrValue<'a, 'q>>;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

pub trait Queryable<'q> {
    fn keys(&self) -> ValueIter<'_> {
        ValueIter::empty()
    }
    fn member<'a, 'f>(&'a self, _: &'f Value) -> Option<Node<'a, 'q>> {
        None
    }
    fn all<'a>(&'a self) -> NodeOrValueIter<'a, 'q> {
        NodeOrValueIter::empty()
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

impl Value {
    pub fn try_into_int(self) -> Result<i64> {
        if let Value::Int(i) = self {
            Ok(i)
        } else {
            Err(Error::ExpectedInt)
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(v) => write!(f, "{}", v),
            Value::Int(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Array(v) => write!(f, "{:?}", v),
            Value::Float(v) => write!(f, "{}", v),
        }
    }
}

impl<'a, 'q> fmt::Display for Node<'a, 'q> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(data) = self.data() {
            write!(f, "{}", data)
        } else {
            write!(f, "[{}]", self.name())
        }
    }
}

impl<'a, 'q> fmt::Debug for &'a dyn Queryable<'q> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{} {:?}]", self.name(), self.data())
    }
}
