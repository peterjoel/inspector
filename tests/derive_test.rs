use clouseau::parse_query;
use clouseau::{query::Context, Queryable};
use std::marker::PhantomData;
use velcro::vec_from;

#[derive(Debug, Queryable)]
struct Foo {
    a: i64,
    b: &'static str,
    v: Vec<HasGenerics<Bar, bool>>,
}

#[derive(Debug, Queryable)]
struct Bar(bool, AnEnum, Foo);

#[derive(Debug, Queryable)]
struct NewType(Foo);

#[derive(Debug, Queryable)]
enum AnEnum {
    Zero,
    One(i64),
    Named { a: u32, b: Vec<i64> },
}

#[derive(Debug, Queryable)]
struct HasGenerics<X: for<'a> Queryable<'a>, Unused> {
    x: X,
    _marker: PhantomData<Unused>,
}

fn has_generics<X: for<'a> Queryable<'a>>(x: X) -> HasGenerics<X, bool> {
    HasGenerics {
        x,
        _marker: PhantomData,
    }
}

#[test]
fn query_custom_struct() {
    let f = NewType(Foo {
        a: 1,
        b: "hello",
        v: vec![
            has_generics(Bar(
                false,
                AnEnum::Zero,
                Foo {
                    a: 10,
                    b: "bye",
                    v: vec![],
                },
            )),
            has_generics(Bar(
                false,
                AnEnum::One(20),
                Foo {
                    a: 11,
                    b: "bonjour",
                    v: vec![has_generics(Bar(
                        false,
                        AnEnum::One(1),
                        Foo {
                            a: 30,
                            b: "aaaaa",
                            v: vec![],
                        },
                    ))],
                },
            )),
            has_generics(Bar(
                false,
                AnEnum::Named {
                    a: 1,
                    b: vec![3, 4, 5],
                },
                Foo {
                    a: 12,
                    b: "ciao",
                    v: vec![],
                },
            )),
        ],
    });

    let ctx = Context::default();
    let q = parse_query("./v/*/x[./1/a = 1]/2/b").unwrap();
    let res: Vec<_> = ctx.exec(&q, &f).collect();
    assert_eq!(res, vec_from!["ciao"]);
}
