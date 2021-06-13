use clouseau::*;
use clouseau_cli::*;
use std::collections::*;
use velcro::{vec, *};

#[derive(Debug, Queryable)]
struct A {
    foo: Vec<B>,
    bar: HashMap<String, i32>,
}

#[derive(Debug, Queryable)]
enum B {
    One(A),
    Another,
}

fn main() {
    let data = A {
        foo: vec![
            B::One(A {
                foo: vec![],
                bar: hash_map_from! {
                    "frog": 10,
                    "camel": 4010,
                    "cow": 3764,
                    "spider": 1,
                    "salamander": 45,
                    "iguana": 45,
                    "butterfly": 3,
                    "caterpillar": 4,
                    "tortoise": 145,
                    "elephant": 14980,
                },
            }),
            B::Another,
        ],
        bar: hash_map_from! {
            "eel": 4,
            "snapper": 60,
            "barnacle": 3,
        },
    };

    let ctx = Context::default();

    repl(&data, &ctx);
}
