use clouseau::parse_query;
use clouseau::{core::*, Context};

#[derive(Debug)]
struct Foo {
    a: i64,
    b: &'static str,
    v: Vec<u32>,
}

impl Queryable for Foo {
    fn keys(&self) -> ValueIter<'_> {
        ValueIter::from_values(vec!["a", "b", "v"].into_iter())
    }
    fn name(&self) -> &'static str {
        "Foo"
    }
    fn member<'f>(&self, field: &'f Value) -> Option<&dyn Queryable> {
        if let Value::String(s) = field {
            match s.as_str() {
                "a" => Some(&self.a as _),
                "b" => Some(&self.b as _),
                "v" => Some(&self.v as _),
                _ => None,
            }
        } else {
            None
        }
    }
    fn all(&self) -> NodeOrValueIter<'_> {
        NodeOrValueIter::from_nodes(vec![&self.a as _, &self.b as _, &self.v as _])
    }
}

#[test]
fn query_custom_struct() {
    let f = Foo {
        a: 1,
        b: "hello",
        v: vec![2, 4, 6],
    };
    let ctx = Context::default();
    let q = parse_query("./b").unwrap();
    let res: Vec<_> = ctx.exec(&q, &f).map(|v| v.to_string()).collect();
    assert_eq!(res, vec![String::from("hello")]);
}
