use clouseau::*;
use std::collections::*;
use std::fmt::Debug;
use velcro::vec;
use velcro::*;

#[derive(Default, Debug, Queryable)]
pub struct Custom {
    a: i64,
    b: String,
    c: Vec<CustomEnum>,
}

#[derive(Debug, Queryable)]
pub enum CustomEnum {
    One,
    Two(String),
    Three(Vec<CustomEnum>, i64),
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

#[test]
fn compare_paths() {
    #[derive(Queryable)]
    struct Data {
        a: Vec<i32>,
        b: Vec<i32>,
    }
    let d = Data {
        a: vec![10, 11, 12, 13, 14],
        b: vec![0, 3, 6, 12, 15],
    };
    let q = parse_query(r#"/b/*[. ?= /a/*]"#).unwrap();
    let result: Vec<Value> = q.exec(&d).collect();
    assert_eq!(result, vec_from![12]);

    let q = parse_query(r#"/b/*[. ?>= /a/*]"#).unwrap();
    let result: Vec<Value> = q.exec(&d).collect();
    assert_eq!(result, vec_from![12, 15]);

    let q = parse_query(r#"/b/*[. >= /a/*]"#).unwrap();
    let result: Vec<Value> = q.exec(&d).collect();
    assert_eq!(result, vec_from![15]);
}
