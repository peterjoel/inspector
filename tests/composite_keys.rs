use clouseau::query::Context;
use clouseau::*;
use std::collections::*;
use std::string::ToString;
use velcro::{vec, *};

#[test]
fn map_with_composite_keys() {
    let t: HashMap<(u64, String, i32), i32> = hash_map! {
        (7, String::from("abc"), 3): 3,
        (8, String::from("def"), 4): 4,
        (8, String::from("def"), 88): 88,
        (9, String::from("xyz"), 5): 5,
    };
    let ctx = Context::default();
    let q = parse_query(r#" ./key:{8, "def", 4} "#).unwrap();
    let mut result: Vec<String> = ctx.exec(&q, &t).map(|v| v.to_string()).collect();
    result.sort();
    assert_eq!(result, vec![String::from("4")]);
}
