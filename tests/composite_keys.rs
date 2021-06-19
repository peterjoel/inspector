use clouseau::query::Context;
use clouseau::*;
use std::collections::*;
use std::string::ToString;
use velcro::*;

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
    let result: HashSet<String> = ctx.exec(&q, &t).map(|v| v.to_string()).collect();
    assert_eq!(result, hash_set_from!["4"]);
}

#[test]
fn display_composite_keys() {
    let t: HashMap<(u64, String, i32), i32> = hash_map! {
        (7, String::from("abc"), 3): 3,
        (8, String::from("def"), 4): 4,
        (8, String::from("def"), 88): 88,
        (9, String::from("xyz"), 5): 5,
    };
    let ctx = Context::default();
    let q = parse_query(r#" .keys().min() "#).unwrap();
    let result: HashSet<String> = ctx.exec(&q, &t).map(|v| v.to_string()).collect();
    assert_eq!(result, hash_set_from![r#"{7, "abc", 3}"#]);
}
