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

pub enum ValueOrQueryable<'a> {
    Value(Value),
    Queryable(&'a dyn Queryable<'a>),
}

pub struct ValueIter<'a>(pub Box<dyn Iterator<Item = Value> + 'a>);
pub struct TreeIter<'a>(pub Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a>);

pub trait Queryable<'a> {
    fn member<'f>(&'a self, _: &'f Value) -> Option<&'a dyn Queryable<'a>> {
        None
    }
    fn all(&'a self) -> TreeIter<'a> {
        TreeIter::empty()
    }
    fn data(&self) -> Option<Value> {
        None
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Value {
    String(String),
    Int(i64),
    Float(WellBehavedF64),
    Bool(bool),
    Array(Array),
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
    pub fn into_int(self) -> Option<i64> {
        if let Value::Int(i) = self {
            Some(i)
        } else {
            None
        }
    }
    pub fn into_value_or_queryable(self) -> ValueOrQueryable<'static> {
        ValueOrQueryable::Value(self)
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

impl<'a> ValueIter<'a> {
    pub fn empty() -> Self {
        ValueIter(Box::new(iter::empty()))
    }

    pub fn once<V: Into<Value>>(val: V) -> Self {
        ValueIter(Box::new(iter::once(val.into())))
    }

    pub fn from_values<T: 'a>(values: impl IntoIterator<Item = T> + 'a) -> Self
    where
        Value: From<T>,
    {
        ValueIter(Box::from(values.into_iter().map(Value::from)))
    }
}

impl<'a> TreeIter<'a> {
    pub fn empty() -> Self {
        TreeIter(Box::new(iter::empty()))
    }

    pub fn from_queryables<Q>(queryables: impl IntoIterator<Item = &'a Q> + 'a) -> Self
    where
        Q: Queryable<'a> + 'a,
    {
        TreeIter(Box::from(queryables.into_iter().map(|q| q as _)))
    }
}

impl<'a> Iterator for ValueIter<'a> {
    type Item = Value;
    fn next(&mut self) -> Option<Value> {
        self.0.next()
    }
}

impl<'a> Queryable<'a> for Value {
    fn data(&self) -> Option<Value> {
        Some(self.clone())
    }
}

impl<'a> Iterator for TreeIter<'a> {
    type Item = &'a dyn Queryable<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a> Queryable<'a> for ValueOrQueryable<'a> {
    fn member<'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'a>> {
        if let ValueOrQueryable::Queryable(q) = self {
            q.member(field)
        } else {
            None
        }
    }
    fn all(&'a self) -> TreeIter<'a> {
        if let ValueOrQueryable::Queryable(q) = self {
            q.all()
        } else {
            TreeIter::empty()
        }
    }
    fn data(&self) -> Option<Value> {
        match self {
            ValueOrQueryable::Value(v) => Some(v.clone()),
            ValueOrQueryable::Queryable(q) => q.data(),
        }
    }
}

// TODO add more detail to this error
pub struct ValueConvertError;
