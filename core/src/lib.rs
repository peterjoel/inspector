use std::borrow::Cow;
use std::collections::*;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::hash::Hash;
use std::iter;

pub trait Queryable<'a> {
    fn keys(&'a self) -> Box<dyn Iterator<Item = Value> + 'a> {
        Box::new(iter::empty())
    }
    fn member<'f>(&'a self, _: &'f Value) -> Option<&'a dyn Queryable<'a>> {
        None
    }
    fn all(&'a self) -> Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a> {
        Box::new(iter::empty())
    }
    fn data(&self) -> Option<Value> {
        None
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum Value {
    String(String),
    Int(i64),
}

pub struct ValueConvertError;

macro_rules! value_int {
    ($($ty: ty $( => $via: ty)?),* $(,)?) => {
        $(
            impl From<$ty> for Value {
                fn from(v: $ty) -> Value {
                    $( let v: $via = v.into(); )?
                    Value::Int(v.try_into().unwrap_or_else(|e| {
                        panic!("{} {} could not be converted to i64: {}", stringify!($ty), v, e);
                    }))
                }
            }

            impl From<&$ty> for Value {
                fn from(v: &$ty) -> Value {
                    Value::from(*v)
                }
            }

            impl TryFrom<Value> for $ty {
                type Error = ValueConvertError;
                fn try_from(value: Value) -> Result<Self, Self::Error> {
                    Self::try_from(&value)
                }
            }

            impl TryFrom<&Value> for $ty {
                type Error = ValueConvertError;
                fn try_from(value: &Value) -> Result<Self, Self::Error> {
                    match value {
                        &Value::Int(v) => {
                            $( let v: $via = v.try_into().map_err(|_|ValueConvertError)?; )?
                            v.try_into().map_err(|_| ValueConvertError)
                        },
                        Value::String(s) => s.parse().map_err(|_| ValueConvertError),
                    }
                }
            }

            impl<'a> Queryable<'a> for $ty {
                fn data<'f>(&self) -> Option<Value> {
                    Some((*self).into())
                }
            }
        )*
    }
}

value_int!(
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    usize,
    isize,
    std::num::NonZeroU8 => u8,
    std::num::NonZeroI8 => i8,
    std::num::NonZeroU16 => u16,
    std::num::NonZeroI16 => i16,
    std::num::NonZeroU32 => u32,
    std::num::NonZeroI32 => i32,
    std::num::NonZeroU64 => u64,
    std::num::NonZeroI64 => i64,
    std::num::NonZeroU128 => u128,
    std::num::NonZeroI128 => i128,
    std::num::NonZeroUsize => usize,
    std::num::NonZeroIsize => isize,
);

impl From<&str> for Value {
    fn from(v: &str) -> Value {
        Value::String(v.to_owned())
    }
}

impl From<String> for Value {
    fn from(v: String) -> Value {
        Value::String(v)
    }
}

impl From<&String> for Value {
    fn from(v: &String) -> Value {
        Value::String(v.clone())
    }
}

impl From<Value> for String {
    fn from(v: Value) -> String {
        match v {
            Value::Int(i) => i.to_string(),
            Value::String(s) => s,
        }
    }
}

impl<'b, T> From<Cow<'b, T>> for Value
where
    T: ToOwned,
    Value: From<&'b T>,
    Value: From<T::Owned>,
{
    fn from(v: Cow<'b, T>) -> Value {
        Value::from(v.into_owned())
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(v) => write!(f, "{}", v),
            Value::Int(v) => write!(f, "{}", v),
        }
    }
}

impl<'a> fmt::Debug for &'a dyn Queryable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let keys: Vec<_> = self.keys().collect();
        write!(f, "{:?}", keys)
    }
}

impl<'a> Queryable<'a> for str {
    fn data(&self) -> Option<Value> {
        Some(self.into())
    }
}

impl<'a> Queryable<'a> for String {
    fn data(&self) -> Option<Value> {
        Some(self.into())
    }
}

impl<'a> Queryable<'a> for &String {
    fn data(&self) -> Option<Value> {
        Some(self.clone().into())
    }
}

impl<'a, K, V> Queryable<'a> for HashMap<K, V>
where
    K: Queryable<'a> + Hash + Eq + Clone,
    V: Queryable<'a>,
    Value: From<K>,
    K: TryFrom<Value>,
{
    fn keys(&'a self) -> Box<dyn Iterator<Item = Value> + 'a> {
        Box::from(self.keys().map(|k| k.clone().into()))
    }

    fn member<'f>(&'a self, field: &'f Value) -> Option<&'a dyn Queryable<'a>> {
        let key = K::try_from(field.clone()).ok()?;
        self.get(&key).map(|v| v as _)
    }

    fn all(&'a self) -> Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a> {
        Box::new(self.values().map(|v| v as _))
    }
}

impl<'a, K, V> Queryable<'a> for BTreeMap<K, V>
where
    K: Queryable<'a> + Ord + Eq + 'a,
    V: Queryable<'a>,
    Value: From<&'a K>,
    K: TryFrom<Value>,
{
    fn keys(&'a self) -> Box<dyn Iterator<Item = Value> + 'a> {
        Box::from(self.keys().map(|k| k.into()))
    }

    fn member<'f>(&'a self, field: &'f Value) -> Option<&dyn Queryable<'a>> {
        let key = K::try_from(field.clone()).ok()?;
        self.get(&key).map(|v| v as _)
    }

    fn all(&'a self) -> Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a> {
        Box::new(self.values().map(|v| v as _))
    }
}

impl<'a, T> Queryable<'a> for Vec<T>
where
    T: Queryable<'a>,
{
    fn keys(&'a self) -> Box<dyn Iterator<Item = Value> + 'a> {
        Box::from((0..self.len()).map(|v| v.into()))
    }

    fn member<'f>(&'a self, field: &'f Value) -> Option<&dyn Queryable<'a>> {
        let index = usize::try_from(field).ok()?;
        self.get(index).map(|v| v as _)
    }

    fn all(&'a self) -> Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a> {
        Box::new(self.iter().map(|v| v as _))
    }
}
