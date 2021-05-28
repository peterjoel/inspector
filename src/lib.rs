mod query;

use query::Value;
use std::collections::*;
use std::fmt::{self, Debug};
use std::hash::Hash;
use std::iter;
use std::str::FromStr;
use velcro::iter;

pub use query::parse_query;

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

impl<'a> Debug for &'a dyn Queryable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let keys: Vec<_> = self.keys().collect();
        write!(f, "{:?}", keys)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use velcro::vec;
    use velcro::*;

    #[derive(Default, Debug)]
    pub struct Custom {
        a: i64,
        b: String,
        c: Vec<CustomEnum>,
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
            match self {
                CustomEnum::One => Box::new(iter::empty()),
                CustomEnum::Two(_) => Box::new(iter::once("0".to_string())),
                CustomEnum::Three(..) => Box::new(iter_from!["0", "1"]),
            }
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

    //   ./*[./*/*/c/1 ?= "Two"]/*/*/b
    //   ./*[./*/*/c/1 ?= "Two"]/*/*/a
    fn example() -> HashMap<i32, Vec<BTreeMap<i64, Custom>>> {
        hash_map! {
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
                        b:"xxx".to_string(),
                        c: vec![CustomEnum::One, CustomEnum::Two("s".to_string())]
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
                        c: vec![CustomEnum::One, CustomEnum::One, CustomEnum::One, CustomEnum::Three(vec![], 4)]
                    }
                }
            ],
        }
    }

    #[test]
    fn select_integer() {
        let t = 123;
        let q = parse_query(".").unwrap();
        let result: Vec<Value> = q.exec(&t).collect();
        assert_eq!(result, vec_from![123]);
    }

    #[test]
    fn select_nth_from_vec() {
        let t = vec![7, 8, 9];
        let q = parse_query("./2").unwrap();
        dbg!(&q);
        let result: Vec<Value> = q.exec(&t).collect();
        assert_eq!(result, vec_from![9]);
    }

    #[test]
    fn select_all_from_vec() {
        let t = vec![7, 8, 9];
        let q = parse_query("./*").unwrap();
        dbg!(&q);
        let result: Vec<Value> = q.exec(&t).collect();
        assert_eq!(result, vec_from![7, 8, 9]);
    }

    #[test]
    fn select_all_from_vec_with_filter() {
        let t = vec![7, 8, 9];
        let q = parse_query("./*[. > 7]").unwrap();
        dbg!(&q);
        let result: Vec<Value> = q.exec(&t).collect();
        assert_eq!(result, vec_from![8, 9]);
    }

    #[test]
    fn select_all_from_map() {
        let t = hash_map! { 7: 3, 8: 4, 9: 5 };
        let q = parse_query("./*").unwrap();
        dbg!(&q);
        let mut result: Vec<Value> = q.exec(&t).collect();
        result.sort();
        assert_eq!(result, vec_from![3, 4, 5]);
    }

    #[test]
    fn test_nested_filter() {
        let t: HashMap<i32, HashMap<String, i32>> = hash_map! {
            7: hash_map_from! {
                "a": 5,
                "b": 6,
            },
            8: hash_map_from! {
                "a": 15,
                "b": 16,
            },
            9: hash_map_from! {
                "a": 105,
                "b": 106,
            },
        };
        let q = parse_query("./*[./a = 15]/b").unwrap();
        let result: Vec<Value> = q.exec(&t).collect();
        assert_eq!(result, vec_from![16]);
    }

    #[test]
    fn enums() {
        let t = vec![
            CustomEnum::One,
            CustomEnum::Two(String::from("x")),
            CustomEnum::Two(String::from("y")),
            CustomEnum::Three(vec![CustomEnum::One], 5),
        ];
        let q = parse_query(r#"./*[. = "Two"]/0"#).unwrap();
        let v: Vec<_> = q.exec(&t).collect();
        assert_eq!(v, vec_from!["x", "y"])
    }

    #[test]
    fn complex_query() {
        let q = parse_query(r#"./*[./*/*/c/1 ?= "Two"]/*/*/a"#).unwrap();
        let mut result: Vec<Value> = q.exec(&example()).collect();
        result.sort();
        assert_eq!(result, vec_from![5, 7, 55]);
    }
}
