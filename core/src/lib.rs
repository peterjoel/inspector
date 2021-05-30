#[cfg(feature = "arrayvec")]
mod arrayvec;
mod std_impls;
#[cfg(feature = "uuid")]
mod uuid;

use std::borrow::Cow;
use std::collections::*;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::hash::Hash;
use std::iter;

pub enum ValueOrQueryable<'a> {
    Value(Value),
    Queryable(&'a dyn Queryable<'a>),
}

pub struct ValueIter<'a>(pub Box<dyn Iterator<Item = Value> + 'a>);
pub struct TreeIter<'a>(pub Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a>);

pub trait Queryable<'a> {
    fn keys(&'a self) -> ValueIter<'a> {
        ValueIter::empty()
    }
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

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum Value {
    String(String),
    Int(i64),
    Bool(bool),
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

impl<'a> ValueIter<'a> {
    pub fn empty() -> Self {
        ValueIter(Box::new(iter::empty()))
    }

    pub fn once<V: Into<Value>>(val: V) -> Self {
        ValueIter(Box::new(iter::once(val.into())))
    }
}

impl<'a> TreeIter<'a> {
    pub fn empty() -> Self {
        TreeIter(Box::new(iter::empty()))
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
    fn keys(&'a self) -> ValueIter<'a> {
        if let ValueOrQueryable::Queryable(q) = self {
            q.keys()
        } else {
            ValueIter::empty()
        }
    }
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
