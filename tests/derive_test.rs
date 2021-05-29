use inspector::parse_query;
use inspector::Queryable;
use velcro::vec_from;

#[derive(Debug, Queryable)]
struct Foo {
    a: i64,
    b: &'static str,
    v: Vec<Bar>,
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

#[test]
fn query_custom_struct() {
    let f = NewType(Foo {
        a: 1,
        b: "hello",
        v: vec![
            Bar(
                false,
                AnEnum::Zero,
                Foo {
                    a: 10,
                    b: "bye",
                    v: vec![],
                },
            ),
            Bar(
                false,
                AnEnum::One(20),
                Foo {
                    a: 11,
                    b: "bonjour",
                    v: vec![Bar(
                        false,
                        AnEnum::One(1),
                        Foo {
                            a: 30,
                            b: "aaaaa",
                            v: vec![],
                        },
                    )],
                },
            ),
            Bar(
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
            ),
        ],
    });

    let q = parse_query("./v/*[./1/a = 1]/2/b").unwrap();
    let res: Vec<_> = q.exec(&f).collect();
    assert_eq!(res, vec_from!["ciao"]);
}
