pub use inspector_core::*;

#[cfg(feature = "parser_pest")]
pub use inspector_pest::parse_query;
pub use inspector_query::*;

#[cfg(test)]
#[cfg(feature = "parser_pest")]
mod tests {

    use std::collections::*;
    use std::fmt::Debug;
    use std::iter;
    use velcro::iter;

    use std::convert::TryInto;

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
        fn keys(&'a self) -> Box<dyn Iterator<Item = Value> + 'a> {
            Box::from(iter!["a", "b", "c"].map(Value::from))
        }

        fn member<'f>(&self, field: &'f Value) -> Option<&dyn Queryable<'a>> {
            match field.to_string().as_str() {
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
        fn keys(&'a self) -> Box<dyn Iterator<Item = Value> + 'a> {
            match self {
                CustomEnum::One => Box::new(iter::empty()),
                CustomEnum::Two(_) => Box::new(iter::once(Value::from("0"))),
                CustomEnum::Three(..) => Box::new(iter_from!["0", "1"]),
            }
        }
        fn member<'f>(&self, field: &'f Value) -> Option<&dyn Queryable<'a>> {
            let index: usize = field.try_into().ok()?;
            match self {
                CustomEnum::One => None,
                CustomEnum::Two(s) => {
                    if index == 0 {
                        Some(s as _)
                    } else {
                        None
                    }
                }
                CustomEnum::Three(b, i) => match index {
                    0 => Some(b as _),
                    1 => Some(i as _),
                    _ => None,
                },
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
