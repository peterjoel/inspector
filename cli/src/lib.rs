mod console;
use clouseau_core::Queryable;
use clouseau_pest::parse_query;
use clouseau_query::{Context, Query};
use console::{Cli, CliResult};
use itertools::Either;

pub fn repl<'q, D: Queryable<'q>>(data: &'q D, ctx: &'q Context) {
    let mut cli = Cli::default();
    let mut state = CliResult::Ready;
    loop {
        state = match state {
            CliResult::Ready => cli.run(),
            CliResult::Quit => return,
            CliResult::Done(query) => match parse_query(&query) {
                Ok(query) => {
                    // I have screwed up the lifetimes somewhere, but this is safe to do
                    // Um.. the occasional segfaults say otherwise...
                    let query: &'q Query = unsafe { &*(&query as *const _) };
                    let results: Vec<_> = ctx.exec(&query, data as _).take(100).collect();
                    cli.print_results(results)
                }
                Err(e) => cli.print_results(vec![format!("{}", e)]),
            },
            CliResult::Suggestions(query_part) => {
                let suggestions = get_suggestions(data, ctx, query_part);
                cli.set_suggestions(suggestions)
            }
        };
    }
}

fn get_suggestions<'q, D: Queryable<'q>>(
    data: &'q D,
    ctx: &'q Context,
    query: String,
) -> Vec<String> {
    match query.chars().last() {
        Some('/') => {
            let query = if query.len() == 1 {
                String::from(".keys()")
            } else {
                format!("{}.keys()", &query[..query.len() - 1])
            };
            run_query(data, ctx, &query).collect()
        }
        None => run_query(data, ctx, ".keys()")
            .map(|k| format!("/{}", k))
            .collect(),
        Some('.') => ctx
            .function_names()
            .map(|name| format!("{}()", name))
            .collect(),
        _ => vec![],
    }
}

fn run_query<'q, D: Queryable<'q>>(
    data: &'q D,
    ctx: &'q Context,
    query: &str,
) -> impl Iterator<Item = String> + 'q {
    if let Ok(query) = parse_query(query) {
        // I have screwed up the lifetimes somewhere, but this is safe to do
        // Um.. the occasional segfaults say otherwise...
        let query: &'q Query = unsafe { &*(&query as *const _) };
        let mut results = ctx.exec(&query, data as _).peekable();
        if results.peek().is_some() {
            return Either::Left(
                std::iter::once(String::from("*")).chain(results.take(100).map(|d| d.to_string())),
            );
        }
    }
    Either::Right(std::iter::empty())
}
