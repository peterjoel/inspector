use clouseau::query::Context;
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
    let ctx = Context::default();
    let q = parse_query(".").unwrap();
    let result: Vec<String> = ctx.exec(&q, &t).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["123"];
    assert_eq!(result, expected);
}

#[test]
fn select_nth_from_vec() {
    let t = vec![7, 8, 9];
    let ctx = Context::default();
    let q = parse_query("./2").unwrap();
    let result: Vec<String> = ctx.exec(&q, &t).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["9"];
    assert_eq!(result, expected);
}

#[test]
fn select_all_from_vec() {
    let t = vec![7, 8, 9];
    let ctx = Context::default();
    let q = parse_query("./*").unwrap();
    let result: Vec<String> = ctx.exec(&q, &t).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["7", "8", "9"];
    assert_eq!(result, expected);
}

#[test]
fn select_all_from_vec_with_filter() {
    let t = vec![7, 8, 9];
    let ctx = Context::default();
    let q = parse_query("./*[. > 7]").unwrap();
    let result: Vec<String> = ctx.exec(&q, &t).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["8", "9"];
    assert_eq!(result, expected);
}

#[test]
fn select_all_from_map() {
    let t = hash_map! { 7: 3, 8: 4, 9: 5 };
    let ctx = Context::default();
    let q = parse_query("./*").unwrap();
    let result: HashSet<String> = ctx.exec(&q, &t).map(|v| v.to_string()).collect();
    assert_eq!(result, hash_set_from!["3", "4", "5"]);
}

#[test]
fn select_first_value() {
    let t = vec![7, 8, 9];
    let ctx = Context::default();
    let q = parse_query("./*.first()").unwrap();
    let result: Vec<String> = ctx.exec(&q, &t).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["7"];
    assert_eq!(result, expected);
}

#[test]
fn select_last_value() {
    let t = vec![7, 8, 9];
    let ctx = Context::default();
    let q = parse_query("./*.last()").unwrap();
    let result: Vec<String> = ctx.exec(&q, &t).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["9"];
    assert_eq!(result, expected);
}

#[test]
fn select_first_non_value() {
    let t = vec![vec![7], vec![8], vec![9]];
    let ctx = Context::default();
    let q = parse_query("./*.first()/0").unwrap();
    let result: Vec<String> = ctx.exec(&q, &t).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["7"];
    assert_eq!(result, expected);
}

#[test]
fn select_last_non_value() {
    let t = vec![vec![7], vec![8], vec![9]];
    let ctx = Context::default();
    let q = parse_query("./*.last()/0").unwrap();
    let result: Vec<String> = ctx.exec(&q, &t).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["9"];
    assert_eq!(result, expected);
}

#[test]
fn select_non_value() {
    let ctx = Context::default();
    let q = parse_query("./1/*").unwrap();
    let result: Vec<String> = ctx.exec(&q, &example()).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["[BTreeMap]", "[BTreeMap]"];
    assert_eq!(result, expected);
}

#[test]
fn count_non_value() {
    let ctx = Context::default();
    let q = parse_query("./1/*.count()").unwrap();
    let result: Vec<String> = ctx.exec(&q, &example()).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["2"];
    assert_eq!(result, expected);
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
    let ctx = Context::default();
    let q = parse_query("./*[./a = 15]/b").unwrap();
    let result: Vec<String> = ctx.exec(&q, &t).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["16"];
    assert_eq!(result, expected);
}

#[test]
fn enums() {
    let t = vec![
        CustomEnum::One,
        CustomEnum::Two(String::from("x")),
        CustomEnum::Two(String::from("y")),
        CustomEnum::Three(vec![CustomEnum::One], 5),
    ];
    let ctx = Context::default();
    let q = parse_query(r#"./*[. = "Two"]/0"#).unwrap();
    let v: Vec<_> = ctx.exec(&q, &t).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["x", "y"];
    assert_eq!(v, expected);
}

#[test]
fn complex_query() {
    let ctx = Context::default();
    let q = parse_query(r#"./*[./*/*/c/1 ?= "Two"]/*/*/a"#).unwrap();
    let result: Vec<String> = ctx.exec(&q, &example()).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["7", "55", "5"];
    assert_eq!(result, expected);
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
    let ctx = Context::default();
    let q = parse_query(r#"/b/*[. ?= /a/*]"#).unwrap();
    let result: Vec<String> = ctx.exec(&q, &d).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["12"];
    assert_eq!(result, expected);

    let q = parse_query(r#"/b/*[. ?>= /a/*]"#).unwrap();
    let result: Vec<String> = ctx.exec(&q, &d).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["12", "15"];
    assert_eq!(result, expected);

    let q = parse_query(r#"/b/*[. >= /a/*]"#).unwrap();
    let result: Vec<String> = ctx.exec(&q, &d).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["15"];
    assert_eq!(result, expected);
}

#[test]
fn function_sum() {
    #[derive(Queryable)]
    struct Data {
        a: Vec<i32>,
        b: Vec<i32>,
    }
    let d = Data {
        a: vec![10, 11, 12, 13, 14],
        b: vec![12, 14, 16, 18],
    };

    let ctx = Context::default();
    // sum all elements of `a`
    let q = parse_query(r#"/a/*.sum()"#).unwrap();
    let result: Vec<String> = ctx.exec(&q, &d).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["60"];
    assert_eq!(result, expected);
}

#[test]
fn function_sum_on_filtered_values() {
    #[derive(Queryable)]
    struct Data {
        a: Vec<i32>,
        b: Vec<i32>,
    }
    let d = Data {
        a: vec![10, 11, 12, 13, 14],
        b: vec![12, 14, 16, 18],
    };
    let ctx = Context::default();
    // sum all elements of `a` that are also present in `b`
    let q = parse_query(r#"/a/*[. ?= /b/*].sum()"#).unwrap();
    let result: Vec<String> = ctx.exec(&q, &d).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["26"];
    assert_eq!(result, expected);
}

#[test]
fn function_count() {
    #[derive(Queryable)]
    struct Data {
        a: Vec<i32>,
        b: Vec<i32>,
    }
    let d = Data {
        a: vec![10, 11, 12, 13, 14],
        b: vec![12, 14, 16, 18],
    };

    let ctx = Context::default();
    // count elements of `a`
    let q = parse_query(r#"/a/*.count()"#).unwrap();
    let result: Vec<String> = ctx.exec(&q, &d).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["5"];
    assert_eq!(result, expected);
}

#[test]
fn function_keys() {
    #[derive(Queryable)]
    struct Data {
        a: Vec<i32>,
        b: HashMap<i32, i32>,
    }
    let d = Data {
        a: vec![10, 11, 12, 13, 14],
        b: hash_map![12: 100, 14: 200, 16: 300, 18: 400],
    };

    let ctx = Context::default();
    // ugh, need to fix that syntax
    let q = parse_query(r#"..keys()"#).unwrap();
    let result: Vec<String> = ctx.exec(&q, &d).map(|v| v.to_string()).collect();
    let expected: Vec<String> = vec_from!["a", "b"];
    assert_eq!(result, expected);
}

#[test]
fn function_keys_are_filterable() {
    #[derive(Queryable)]
    struct Data {
        a: Vec<i32>,
        b: HashMap<i32, i32>,
    }
    let d = Data {
        a: vec![10, 11, 12, 13, 14],
        b: hash_map![12: 100, 14: 200, 16: 300, 18: 400],
    };

    let ctx = Context::default();
    let q = parse_query(r#"/b.keys()[. > 14]"#).unwrap();
    let result: HashSet<String> = ctx.exec(&q, &d).map(|v| v.to_string()).collect();
    let expected: HashSet<String> = hash_set_from!["16", "18"];
    assert_eq!(result, expected);
}

#[test]
fn non_dot_paths_cannot_be_executed_on_values_in_predicates() {
    #[derive(Queryable)]
    struct Data {
        a: Vec<i32>,
        b: HashMap<i32, i32>,
    }
    let d = Data {
        a: vec![10, 11, 12, 13, 14],
        b: hash_map![12: 100, 14: 200, 16: 300, 18: 400],
    };

    let ctx = Context::default();
    let q = parse_query(r#"/b.keys()[./path > 14]"#).unwrap();
    let result: HashSet<String> = ctx.exec(&q, &d).map(|v| v.to_string()).collect();
    let expected: HashSet<String> = hash_set_from!["Error: Cannot execute a path on a value"];
    assert_eq!(result, expected);
}
