use clouseau::parse_query;
use clouseau::{query::Context, Queryable, Value};
use std::convert::{TryFrom, TryInto};

#[derive(Debug, Clone, Queryable)]
struct NonTransparent(u64);

// with manual impls for Value conversions
#[derive(Debug, Clone, Queryable)]
#[clouseau(transparent)]
struct Transparent1(u64);

impl From<Transparent1> for Value {
    fn from(a: Transparent1) -> Self {
        // convert to a string instead of using the integer - which the derived impl would have done
        format!("Transparent1_{}", a.0).into()
    }
}

impl TryFrom<Value> for Transparent1 {
    type Error = <u64 as TryFrom<Value>>::Error;
    fn try_from(v: Value) -> Result<Self, Self::Error> {
        Ok(Transparent1(v.try_into()?))
    }
}

// with derived impls for Value conversions
#[derive(Debug, Clone, Queryable)]
#[clouseau(transparent, value)]
struct Transparent2(u64);

#[test]
fn query_custom_struct() {
    let ctx = Context::default();
    let q = parse_query("./0").unwrap();
    let q2 = parse_query("./0/0").unwrap();

    let nonatomics = vec![NonTransparent(1), NonTransparent(2)];
    let res: Vec<String> = ctx.exec(&q, &nonatomics).map(|v| v.to_string()).collect();
    let res2: Vec<String> = ctx.exec(&q2, &nonatomics).map(|v| v.to_string()).collect();
    assert_eq!(res, vec![String::from("[NonTransparent]")]);
    assert_eq!(res2, vec![String::from("1")]);

    let atomic1s = vec![Transparent1(1), Transparent1(2)];
    let res: Vec<String> = ctx.exec(&q, &atomic1s).map(|v| v.to_string()).collect();
    let res2: Vec<String> = ctx.exec(&q2, &atomic1s).map(|v| v.to_string()).collect();
    assert_eq!(res, vec![String::from("Transparent1_1")]);
    assert!(res2.is_empty());

    let atomic2s = vec![Transparent2(1), Transparent2(2)];
    let res: Vec<String> = ctx.exec(&q, &atomic2s).map(|v| v.to_string()).collect();
    let res2: Vec<String> = ctx.exec(&q2, &atomic1s).map(|v| v.to_string()).collect();
    assert_eq!(res, vec![String::from("1")]);
    assert!(res2.is_empty());
}
