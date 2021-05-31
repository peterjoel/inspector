#[cfg(feature = "arrayvec")]
mod arrayvec;
mod std_impls;
#[cfg(feature = "uuid")]
mod uuid;

use std::iter;

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
