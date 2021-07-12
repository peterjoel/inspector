use clouseau::parse_query;
use clouseau::{query::Context, Queryable};
use std::collections::*;
use velcro::{vec, *};

#[derive(Debug, Queryable)]
#[clouseau(accessor(name = "foo", getter = "MyBytes::foo"))]
#[clouseau(accessor(name = "bar", getter = "MyBytes::bar"))]
#[clouseau(accessor(name = "wibble", getter = "MyBytes::wibble"))]
struct MyBytes(#[clouseau(skip)] [u8; 3]);

impl MyBytes {
    fn foo(&self) -> &u8 {
        &self.0[0]
    }

    fn bar(&self) -> &u8 {
        &self.0[1]
    }

    fn wibble(&self) -> &u8 {
        &self.0[2]
    }
}

#[test]
fn query_custom_struct() {
    let ctx = Context::default();
    let data: HashMap<String, MyBytes> = hash_map_from! {
        "a": MyBytes([3,4,5]),
        "b": MyBytes([13,14,15]),
    };
    let q = parse_query("./b/foo").unwrap();
    let res: Vec<_> = ctx.exec(&q, &data).map(|v| v.to_string()).collect();
    assert_eq!(res, vec![String::from("13")]);
}
