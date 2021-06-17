use clouseau::parse_query;
use clouseau::{query::Context, Queryable, Value};
use std::convert::{TryFrom, TryInto};

#[derive(Debug, Clone, Queryable)]
struct NonTransparent(u64);

// Transparent, treat the struct as a u64
#[derive(Debug, Clone, Queryable)]
#[clouseau(transparent)]
struct Transparent1(u64);

// Treated as an opaque value but converted to a string with `_u64` suffix
#[derive(Debug, Clone, Queryable)]
#[clouseau(value, as = "String")]
struct Transparent2(u64);

impl TryFrom<String> for Transparent2 {
    type Error = std::num::ParseIntError;
    fn try_from(s: String) -> Result<Transparent2, Self::Error> {
        let n: u64 = s.parse()?;
        Ok(Transparent2(n))
    }
}

impl From<Transparent2> for String {
    fn from(t: Transparent2) -> String {
        format!("{}_u64", t.0)
    }
}

#[test]
fn query_custom_struct() {
    let ctx = Context::default();
    let q = parse_query("./0").unwrap();
    let q2 = parse_query("./0/0").unwrap();

    let non_transparents = vec![NonTransparent(1), NonTransparent(2)];
    let res: Vec<String> = ctx
        .exec(&q, &non_transparents)
        .map(|v| v.to_string())
        .collect();
    let res2: Vec<String> = ctx
        .exec(&q2, &non_transparents)
        .map(|v| v.to_string())
        .collect();
    assert_eq!(res, vec![String::from("[NonTransparent]")]);
    assert_eq!(res2, vec![String::from("1")]);

    let atomic1s = vec![Transparent1(1), Transparent1(2)];
    let res: Vec<String> = ctx.exec(&q, &atomic1s).map(|v| v.to_string()).collect();
    let res2: Vec<String> = ctx.exec(&q2, &atomic1s).map(|v| v.to_string()).collect();
    assert_eq!(res, vec![String::from("1")]);
    assert!(res2.is_empty());

    let atomic2s = vec![Transparent2(1), Transparent2(2)];
    let res: Vec<String> = ctx.exec(&q, &atomic2s).map(|v| v.to_string()).collect();
    let res2: Vec<String> = ctx.exec(&q2, &atomic1s).map(|v| v.to_string()).collect();
    assert_eq!(res, vec![String::from("1_u64")]);
    assert!(res2.is_empty());
}
