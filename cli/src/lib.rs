mod console;
use clouseau_core::Queryable;
use clouseau_pest::{parse_query, Result};
use clouseau_query::{Context, Query};
use console::{Cli, CliResult};

pub fn repl<'q, D: Queryable<'q>>(data: &'q D, ctx: &'q Context) {
    let mut cli = Cli::default();
    let mut state = CliResult::Ready;
    loop {
        state = match state {
            CliResult::Ready => cli.run(),
            CliResult::Quit => return,
            CliResult::Done(query) => match run_query(data, ctx, &query) {
                Ok(results) => cli.print_results(results),
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
    if query.ends_with("./") {
        run_query(data, ctx, &format!("{}.keys()", &query[..query.len() - 2]))
            .ok()
            .unwrap_or_else(Vec::new)
    } else if query.ends_with('/') {
        run_query(data, ctx, &format!("{}.keys()", &query[..query.len() - 1]))
            .ok()
            .unwrap_or_else(Vec::new)
    } else if query.ends_with('.') {
        ctx.function_names()
            .map(|name| format!("{}()", name))
            .collect()
    } else if query.is_empty() {
        vec![String::from("/"), String::from("./"), String::from(".")]
    } else {
        Vec::new()
    }
}

fn run_query<'q, D: Queryable<'q>>(
    data: &'q D,
    ctx: &'q Context,
    query: &str,
) -> Result<Vec<String>> {
    parse_query(query).map(|query| {
        // I have probably screwed up the lifetimes somewhere for this to be necessary, but this is safe to
        // do (provided that we don't do anything stupid, like return an iterator that references the query)
        let query: &'q Query = unsafe { &*(&query as *const _) };
        let mut results = ctx.exec(&query, data as _).peekable();
        if results.peek().is_some() {
            std::iter::once(String::from("*"))
                .chain(results.take(100).map(|d| d.to_string()))
                .collect()
        } else {
            Vec::new()
        }
    })
}
