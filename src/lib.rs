mod query;

use query::{Compare, Path, Pred, Segment, SegmentType};
use std::collections::*;
use std::fmt::Debug;
use std::fmt::{self, Display};
use std::hash::Hash;
use std::iter;
use std::str::FromStr;
use velcro::vec;
use velcro::*;

#[macro_export]
macro_rules! query {
    ($expr: tt $(/$field: tt)*) => {{
        fn query<'a>(v: &'a impl Queryable<'a>) -> Box<dyn Iterator<Item = Value> + 'a> {
            query!(@ v, $(/$field)*)
        }
        query(&$expr)
    }
    };
    (@ $expr: expr,) => {{
        let r: Box<dyn Iterator<Item = Value>> = Box::from($expr.data().into_iter());
        r
    }};
    (@ $expr: expr, / * $(/ $others: tt)*) => {
        Box::new($expr.all().flat_map(|v| {
            query!(@ v, $(/$others)*)
        }))
    };
    (@ $expr: expr, /$field: tt $(/ $others: tt)*) => {
        match $expr.member(stringify!($field)) {
            Some(v) => query!(@ v, $(/ $others)*),
            None => {
                let r: Box<dyn Iterator<Item = Value>> = Box::from(iter::empty());
                r
            }
        }
    }
}

#[test]
fn test() {
    let root = hash_map! {
        1: vec![
            btree_map! {
                55: Custom{a: 7,.. Custom::default()}
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
        ..(2..100): vec![],
        ..(8..12): vec![
            btree_map!{
                55: Custom::default()
            },
            btree_map!{
                6: Custom { a: 6,..Custom::default()},
                55: Custom {
                    a: 444,
                    b: "SDFDF".to_string(),
                    c: vec![99]
                }
            }
        ],
    };

    let x = query!(root / * / * / 55 / a);
    println!("x..");
    for y in x {
        println!("   y = {:?}", y);
    }

    let q = Path::default()
        .append(SegmentType::All.into())
        .append(
            SegmentType::All.to_segment().with_filter(Pred::new(
                Path::default()
                    .append(SegmentType::Named("55".to_string()).into())
                    .append(SegmentType::Named("a".to_string()).into()),
                Compare::LessThan,
                Value::Int(444),
            )),
        )
        .append(SegmentType::Named("55".to_string()).into())
        .append(SegmentType::Named("a".to_string()).into());
    let x = q.exec(&root);
    for y in x {
        println!("** y = {:?}", y);
    }

    use CustomEnum::*;
    let tree = hash_map! {
        1: One,
        2: Two("Hello".to_string()),
        3: Three(vec![One], 44),
        4: Three(vec![Three(vec![One, Two("ABC".to_string())],1), One, One], 3),
        5: Three(vec![Three(vec![One, Two("x".to_string())],1), One, One], 6),
    };
    let q = Path::default()
        .append(SegmentType::All.to_segment().with_filter(Pred::new(
            Path::default(),
            Compare::Eq,
            Value::String("Two".to_string()),
        )))
        .append(SegmentType::Named("0".to_string()).to_segment())
        .append(SegmentType::Named("0".to_string()).to_segment());
    let x = q.exec(&tree);
    for y in x {
        println!("---- y = {:?}", y);
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Int(i64),
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(v) => write!(f, "{}", v),
            Value::Int(v) => write!(f, "{}", v),
        }
    }
}

pub trait Queryable<'a> {
    fn keys(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::new(iter::empty())
    }
    fn member<'f>(&'a self, _: &'f str) -> Option<&dyn Queryable<'a>> {
        None
    }
    fn all(&'a self) -> Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a> {
        Box::new(iter::empty())
    }
    fn data(&self) -> Option<Value> {
        None
    }
    fn display(&self) -> Option<String> {
        None
    }
}

impl<'a> Queryable<'a> for i32 {
    fn data(&self) -> Option<Value> {
        Some(Value::Int((*self).into()))
    }
    fn display(&self) -> Option<String> {
        Some(format!("{}", self))
    }
}
impl<'a> Queryable<'a> for i64 {
    fn data(&self) -> Option<Value> {
        Some(Value::Int((*self).into()))
    }
    fn display(&self) -> Option<String> {
        Some(format!("{}", self))
    }
}
impl<'a> Queryable<'a> for &str {
    fn data(&self) -> Option<Value> {
        Some(Value::String(self.to_string()))
    }
    fn display(&self) -> Option<String> {
        Some(format!("{}", self))
    }
}

impl<'a> Queryable<'a> for String {
    fn data(&self) -> Option<Value> {
        Some(Value::String(self.clone()))
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
    fn keys(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::from(self.keys().filter_map(|k| k.display()))
    }
    fn member<'f>(&'a self, field: &'f str) -> Option<&dyn Queryable<'a>> {
        K::from_str(field)
            .ok()
            .and_then(|field| self.get(&field))
            .map(|v| v as _)
    }
    fn all(&'a self) -> Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a> {
        Box::new(self.values().map(|v| v as _))
    }
}

impl<'a, K: Queryable<'a> + Ord + Eq + FromStr, V: Queryable<'a>> Queryable<'a> for BTreeMap<K, V> {
    fn keys(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::from(self.keys().filter_map(|k| k.display()))
    }
    fn member<'f>(&'a self, field: &'f str) -> Option<&dyn Queryable<'a>> {
        K::from_str(field)
            .ok()
            .and_then(|field| self.get(&field))
            .map(|v| v as _)
    }
    fn all(&'a self) -> Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a> {
        Box::new(self.values().map(|v| v as _))
    }
}

impl<'a, T: Queryable<'a>> Queryable<'a> for Vec<T> {
    fn keys(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::from((0..self.len()).map(|v| v.to_string()))
    }
    fn member<'f>(&'a self, field: &'f str) -> Option<&dyn Queryable<'a>> {
        usize::from_str(field)
            .ok()
            .and_then(|index| self.get(index))
            .map(|v| v as _)
    }
    fn all(&'a self) -> Box<dyn Iterator<Item = &'a dyn Queryable<'a>> + 'a> {
        Box::new(self.iter().map(|v| v as _))
    }
}

#[derive(Default, Debug)]
pub struct Custom {
    a: i64,
    b: String,
    c: Vec<i32>,
}

impl<'a> Queryable<'a> for Custom {
    fn keys(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::from(iter!["a", "b", "c"].map(str::to_string))
    }
    fn member<'f>(&self, field: &'f str) -> Option<&dyn Queryable<'a>> {
        match field {
            "a" => Some(&self.a as _),
            "b" => Some(&self.b as _),
            "c" => Some(&self.c as _),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum CustomEnum {
    One,
    Two(String),
    Three(Vec<CustomEnum>, i64),
}

impl<'a> Queryable<'a> for CustomEnum {
    fn keys(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::from(iter!["0", "1"].map(str::to_string))
    }
    fn member<'f>(&self, field: &'f str) -> Option<&dyn Queryable<'a>> {
        match self {
            CustomEnum::One => None,
            CustomEnum::Two(s) => {
                if field == "0" {
                    Some(s as _)
                } else {
                    None
                }
            }
            CustomEnum::Three(b, i) => match field {
                "0" => Some(b as _),
                "1" => Some(i as _),
                _ => None,
            },
            _ => None,
        }
    }
    fn data(&self) -> Option<Value> {
        match self {
            CustomEnum::One => Some(Value::String(String::from("One"))),
            CustomEnum::Two(_) => Some(Value::String(String::from("Two"))),
            CustomEnum::Three(_, _) => Some(Value::String(String::from("Three"))),
        }
    }
}

impl<'a> Debug for &'a dyn Queryable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let keys: Vec<_> = self.keys().collect();
        write!(f, "{:?}", keys)
    }
}
