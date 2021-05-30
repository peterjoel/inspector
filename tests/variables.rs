use clouseau::query::Context;
use clouseau::*;
use velcro::vec;
use velcro::*;

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
    let ctx = Context::default().with_var("foo", 12);
    let q = parse_query(r#"/b/*[. >= $foo]"#).unwrap();
    let result: Vec<Value> = ctx.exec(&q, &d).collect();
    assert_eq!(result, vec_from![12, 15]);
}
