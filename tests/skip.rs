use clouseau::query::Context;
use clouseau::*;
use std::fmt::Debug;

#[derive(Default, Debug, Queryable)]
pub struct Custom {
    a: i64,
    b: String,
    #[clouseau(skip)]
    c: i64,
}

#[test]
fn skipped_field_is_not_a_member() {
    let root = Custom {
        a: 4,
        b: String::from("hi"),
        c: 7,
    };
    let ctx = Context::default();
    let q = parse_query("./c").unwrap();
    let res: Vec<_> = ctx.exec(&q, &root).map(|v| v.to_string()).collect();
    assert!(res.is_empty());
}

#[test]
fn skipped_field_is_not_found_with_wildcard() {
    let root = Custom {
        a: 4,
        b: String::from("hi"),
        c: 7,
    };
    let ctx = Context::default();
    let q = parse_query("./*").unwrap();
    let res: Vec<_> = ctx.exec(&q, &root).map(|v| v.to_string()).collect();
    assert_eq!(res, vec![String::from("4"), String::from("hi")]);
}

#[test]
fn skipped_field_is_not_listed_as_a_key() {
    let root = Custom {
        a: 4,
        b: String::from("hi"),
        c: 7,
    };
    let ctx = Context::default();
    let q = parse_query(".keys()").unwrap();
    let res: Vec<_> = ctx.exec(&q, &root).map(|v| v.to_string()).collect();
    assert_eq!(res, vec![String::from("a"), String::from("b")]);
}

#[derive(Default, Debug, Queryable)]
#[clouseau(transparent)]
struct Transparent {
    #[clouseau(skip)]
    a: i32,
    b: Custom,
    #[clouseau(skip)]
    c: i32,
}

#[test]
fn struct_with_one_nonskipped_field_can_be_transparent() {
    let root = Transparent {
        a: 4,
        b: Custom {
            a: 10,
            b: String::from("x"),
            c: 100,
        },
        c: 6,
    };
    let ctx = Context::default();
    let q = parse_query("./a").unwrap();
    let res: Vec<_> = ctx.exec(&q, &root).map(|v| v.to_string()).collect();
    assert_eq!(res, vec![String::from("10")]);
}
