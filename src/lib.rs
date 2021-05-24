use std::collections::*;
use std::fmt::Debug;
use std::fmt::{self, Display};
use std::hash::Hash;
use std::iter;
use std::str::FromStr;
use thiserror::Error;
use velcro::vec;
use velcro::*;

macro_rules! query {
    ($expr: tt $(/$field: tt)*) => {{
        fn query<'a>(v: impl Queryable<'a> + 'a) -> Result<'static, Option<Value>> {
            query!(@ v, $(/$field)*);
            Ok(None)
        }
        query($expr)
    }
    };
    (@ $expr: expr,) => {
        return Ok(Some($expr.data()?));
    };
    (@ $expr: expr, /$field: tt $(/ $others: tt)*) => {
        if let Some(v) = $expr.member(stringify!($field))? {
            query!(@ v, $(/$others)*);
        }
    }
}

#[test]
fn test() {
    let root = hash_map! {
        1: vec![
            btree_map! {
                55: Custom::default()
            },
            btree_map! {
                66: Custom {
                    a: 55,
                    ..Custom::default()
                },
                100: Custom{
                    a: 5,
                    b:"hello".to_string(),
                    c: vec![1,2,3]
                }
            }
        ],
        2: vec![],
        8: vec![
            btree_map!{
                4: Custom::default()
            },
            btree_map!{
                6: Custom::default(),
                7: Custom {
                    a: 444,
                    b: "SDFDF".to_string(),
                    c: vec![99]
                }
            }
        ],
    };

    let x = query!(root / 8 / 1 / 7 / a);

    println!("x = {:?}", x);
}

#[derive(Debug, Error, Clone, Copy)]
pub enum Error<'e> {
    #[error("Field '{0}' doesn't exist")]
    NoSuchField(&'e str),
    #[error("Not a terminal node")]
    NotATerminal,
}

pub type Result<'e, T, E = Error<'e>> = std::result::Result<T, E>;

#[derive(Debug, Clone)]
enum Value {
    String(String),
    Int(i64),
}

struct Pred {
    a: Value,
    comp: Compare,
    b: Value,
}
enum Compare {
    LessThan,
    LessThanEq,
    Eq,
    GreaterThan,
    GreaterThanEq,
}

impl Pred {
    fn eval(&self) -> bool {
        match (&self.a, &self.b) {
            (Value::String(a), Value::String(b)) => match self.comp {
                Compare::LessThan => a < b,
                Compare::LessThanEq => a <= b,
                Compare::Eq => a <= b,
                Compare::GreaterThan => a > b,
                Compare::GreaterThanEq => a >= b,
            },
            (Value::Int(a), Value::Int(b)) => match self.comp {
                Compare::LessThan => a < b,
                Compare::LessThanEq => a <= b,
                Compare::Eq => a <= b,
                Compare::GreaterThan => a > b,
                Compare::GreaterThanEq => a >= b,
            },
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(v) => write!(f, "{}", v),
            Value::Int(v) => write!(f, "{}", v),
        }
    }
}

trait Queryable<'a> {
    fn members(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::new(iter::empty())
    }
    fn member<'f>(&self, field: &'f str) -> Result<'f, Option<&dyn Queryable<'a>>> {
        Err(Error::NoSuchField(field))
    }
    fn data(&self) -> Result<'static, Value> {
        Err(Error::NotATerminal)
    }
    fn display(&self) -> Option<String> {
        None
    }
}

impl<'a> Queryable<'a> for i32 {
    fn data(&self) -> Result<'static, Value> {
        Ok(Value::Int((*self).into()))
    }
    fn display(&self) -> Option<String> {
        Some(format!("{}", self))
    }
}
impl<'a> Queryable<'a> for i64 {
    fn data(&self) -> Result<'static, Value> {
        Ok(Value::Int((*self).into()))
    }
    fn display(&self) -> Option<String> {
        Some(format!("{}", self))
    }
}
impl<'a> Queryable<'a> for &str {
    fn data(&self) -> Result<'static, Value> {
        Ok(Value::String(self.to_string()))
    }
    fn display(&self) -> Option<String> {
        Some(format!("{}", self))
    }
}

impl<'a> Queryable<'a> for String {
    fn data(&self) -> Result<'static, Value> {
        Ok(Value::String(self.clone()))
    }
    fn display(&self) -> Option<String> {
        Some(format!("{}", self))
    }
}

impl<'a, K: Queryable<'a> + Hash + Eq + FromStr + Debug, V: Queryable<'a>> Queryable<'a>
    for HashMap<K, V>
where
    <K as FromStr>::Err: Debug,
{
    fn members(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::from(self.keys().filter_map(|k| k.display()))
    }
    fn member<'f>(&self, field: &'f str) -> Result<'f, Option<&dyn Queryable<'a>>> {
        Ok(K::from_str(field)
            .ok()
            .and_then(|field| self.get(&field))
            .map(|v| v as _))
    }
}

impl<'a, K: Queryable<'a> + Ord + Eq + FromStr, V: Queryable<'a>> Queryable<'a> for BTreeMap<K, V> {
    fn members(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::from(self.keys().filter_map(|k| k.display()))
    }
    fn member<'f>(&self, field: &'f str) -> Result<'f, Option<&dyn Queryable<'a>>> {
        Ok(K::from_str(field)
            .ok()
            .and_then(|field| self.get(&field))
            .map(|v| v as _))
    }
}

impl<'a, T: Queryable<'a>> Queryable<'a> for Vec<T> {
    fn members(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::from((0..self.len()).map(|v| v.to_string()))
    }
    fn member<'f>(&self, field: &'f str) -> Result<'f, Option<&dyn Queryable<'a>>> {
        Ok(usize::from_str(field)
            .ok()
            .and_then(|index| self.get(index))
            .map(|v| v as _))
    }
}

#[derive(Default, Debug)]
pub struct Custom {
    a: i64,
    b: String,
    c: Vec<i32>,
}

impl<'a> Queryable<'a> for Custom {
    fn members(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::from(iter!["a", "b", "c"].map(str::to_string))
    }
    fn member<'f>(&self, field: &'f str) -> Result<'f, Option<&dyn Queryable<'a>>> {
        match field {
            "a" => Ok(Some(&self.a as _)),
            "b" => Ok(Some(&self.b as _)),
            "c" => Ok(Some(&self.c as _)),
            _ => Err(Error::NoSuchField(field)),
        }
    }
}
