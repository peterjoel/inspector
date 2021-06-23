mod console;
mod history;

use clouseau_core::Queryable;
use clouseau_pest::{parse_query, Result};
use clouseau_query::Context;
use console::{Cli, CliResult};
use directories::BaseDirs;
use history::History;
use std::fs::OpenOptions;
use std::io::{BufRead, BufReader, Write};
use std::{fs::File, path::PathBuf};

pub struct ClouseauConsole {
    persist_history: Option<PathBuf>,
    max_history_len: usize,
}

impl Default for ClouseauConsole {
    fn default() -> Self {
        Self {
            persist_history: Default::default(),
            max_history_len: 100,
        }
    }
}

impl ClouseauConsole {
    /// Pass `None` for the default location, in the user data directory
    pub fn with_persistent_history(mut self, path: Option<PathBuf>) -> ClouseauConsole {
        self.persist_history = path.or_else(|| {
            if let Some(base_dirs) = BaseDirs::new() {
                let mut path = base_dirs.data_dir().to_path_buf();
                path.push("clouseau_history");
                Some(path)
            } else {
                eprintln!("Could not determine user home directory");
                None
            }
        });
        self
    }

    pub fn run<D: Queryable>(self, data: &D, ctx: &Context) {
        let mut state = CliResult::Ready;
        let history = match self.load_history() {
            Err(e) => {
                eprintln!("Could not load query history: {}", e);
                History::default()
            }
            Ok(history) => history,
        };
        let mut cli = Cli::new(Some(history));
        loop {
            state = match state {
                CliResult::Ready => cli.run(),
                CliResult::Quit => return,
                CliResult::Done(query) => {
                    if let Err(e) = self.save_history(cli.history()) {
                        eprintln!("Could not save query history: {}", e);
                    }
                    match run_query(data, ctx, &query) {
                        Ok(results) => cli.print_results(results),
                        Err(e) => cli.print_results(vec![format!("{}", e)]),
                    }
                }
                CliResult::Suggestions(query_part) => {
                    let suggestions = get_suggestions(data, ctx, query_part);
                    cli.set_suggestions(suggestions)
                }
            };
        }
    }

    fn load_history(&self) -> std::io::Result<History> {
        if let Some(path) = &self.persist_history {
            if path.exists() {
                let file = File::open(&path)?;
                let reader = BufReader::new(file);
                let history = reader.lines().collect::<Result<_, _>>()?;
                return Ok(History::new(self.max_history_len, history));
            }
        }
        Ok(History::default())
    }

    fn save_history(&self, history: &History) -> std::io::Result<()> {
        if let Some(path) = &self.persist_history {
            let mut file = OpenOptions::new().write(true).create(true).open(&path)?;
            for query in history.iter() {
                file.write_all(query.as_bytes())?;
                file.write_all(b"\n")?;
            }
        }
        Ok(())
    }
}

fn get_suggestions<D: Queryable>(data: &D, ctx: &Context, query: String) -> Vec<String> {
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

fn run_query<D: Queryable>(data: &D, ctx: &Context, query: &str) -> Result<Vec<String>> {
    parse_query(query).map(|query| {
        let results = ctx.exec(&query, data as _).peekable();
        results.take(100).map(|d| d.to_string()).collect()
    })
}
