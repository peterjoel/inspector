use itertools::Itertools;
use std::collections::VecDeque;
use std::fmt::Display;
use std::io::{stdin, stdout, StdoutLock, Write};
use std::ops::Deref;
use std::ops::Range;
use termion::cursor::DetectCursorPos;
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;

pub enum CliResult {
    Suggestions(String),
    Done(String),
    Quit,
    Ready,
}

#[derive(Debug, Copy, Clone)]
enum ConsoleState {
    Start,
    Typing,
    GetSuggestions,
    FilterSuggestions,
    NoSuggestions,
    Suggesting,
    AcceptSuggestion,
    HistoryUp,
    HistoryDown,
    Done,
    Quit,
}

pub struct Cli {
    state: ConsoleState,
    history: History,
    line: InputLine,
    suggestions: Suggestions,
}

impl Default for Cli {
    fn default() -> Self {
        Cli {
            state: ConsoleState::Start,
            history: History::default(),
            line: InputLine::default(),
            suggestions: Suggestions::default(),
        }
    }
}

impl Cli {
    pub fn run(&mut self) -> CliResult {
        let stdout = stdout().into_raw_mode().unwrap();
        let mut stdout = stdout.lock();
        let mut keys = stdin().keys();

        loop {
            self.render(&mut stdout);

            self.state = match self.state {
                ConsoleState::Quit => {
                    return CliResult::Quit;
                }
                ConsoleState::Done => {
                    let query = std::mem::take(&mut self.line).into_string();
                    self.history.save(query.clone());
                    // ugly fix to put it in the right state for next time
                    self.state = ConsoleState::Typing;
                    if query.is_empty() {
                        None
                    } else {
                        return CliResult::Done(query);
                    }
                }
                ConsoleState::HistoryUp => {
                    self.history.up();
                    self.line = InputLine::from_string(
                        self.history.current().unwrap_or_default().to_string(),
                    );
                    Some(ConsoleState::Typing)
                }
                ConsoleState::HistoryDown => {
                    self.history.down();
                    self.line = InputLine::from_string(
                        self.history.current().unwrap_or_default().to_string(),
                    );
                    Some(ConsoleState::Typing)
                }
                ConsoleState::Typing => {
                    let key = keys.next().unwrap().unwrap();
                    typing(key, &mut self.line)
                }
                ConsoleState::Suggesting => {
                    let key = keys.next().unwrap().unwrap();
                    suggesting(key, &mut self.line, &mut self.suggestions)
                }
                ConsoleState::GetSuggestions => {
                    return CliResult::Suggestions(self.line.preceding_current_word())
                }
                ConsoleState::FilterSuggestions => {
                    filter_suggestions(&self.line, &mut self.suggestions)
                }
                ConsoleState::NoSuggestions => Some(ConsoleState::Typing),
                ConsoleState::AcceptSuggestion => {
                    if let Some(word) = self.suggestions.accepted_word() {
                        self.line.replace_word(&word)
                    }
                    Some(ConsoleState::Typing)
                }
                ConsoleState::Start => Some(ConsoleState::Typing),
            }
            .unwrap_or(self.state);
        }
    }

    pub fn print_results<T: Display>(&mut self, results: impl IntoIterator<Item = T>) -> CliResult {
        {
            let stdout = stdout().into_raw_mode().unwrap();
            stdout.suspend_raw_mode().unwrap();
            let mut stdout = stdout.lock();
            write!(&mut stdout, "\n\n").unwrap();
            for item in results {
                writeln!(&mut stdout, "{}", item).unwrap();
            }
            stdout.flush().unwrap();
        }
        let stdout = stdout().into_raw_mode().unwrap();
        stdout.activate_raw_mode().unwrap();
        self.state = ConsoleState::Start;
        CliResult::Ready
    }

    pub fn set_suggestions(&mut self, suggestions: Vec<String>) -> CliResult {
        self.suggestions = Suggestions::new(suggestions);
        self.state = ConsoleState::FilterSuggestions;
        self.run()
    }

    fn render(&mut self, stdout: &mut StdoutLock) {
        let cursor = stdout.cursor_pos().unwrap();
        match self.state {
            ConsoleState::Start => {
                write!(
                    stdout,
                    "{}{}>\r\n{}",
                    termion::cursor::Goto(1, cursor.1),
                    termion::clear::CurrentLine,
                    termion::cursor::Goto(self.line.cursor() as u16 + 3, cursor.1 - 1),
                )
                .unwrap();
            }
            ConsoleState::Quit => {
                // just a bit of cleaning up
                write!(stdout, "\r\n{}", termion::clear::AfterCursor).unwrap();
            }
            ConsoleState::Done => {
                write!(
                    stdout,
                    "\r\n{}{}",
                    termion::clear::CurrentLine,
                    termion::cursor::Goto(self.line.cursor() as u16 + 3, cursor.1)
                )
                .unwrap();
            }
            ConsoleState::Typing => {
                write!(
                    stdout,
                    "{}{}> {}\r\n{}{}",
                    termion::cursor::Goto(1, cursor.1),
                    termion::clear::CurrentLine,
                    self.line.as_str(),
                    termion::clear::CurrentLine,
                    termion::cursor::Goto(self.line.cursor() as u16 + 3, cursor.1),
                )
                .unwrap();
            }
            ConsoleState::NoSuggestions => {
                write!(
                    stdout,
                    "\r\n{}No suggestions{}",
                    termion::clear::CurrentLine,
                    termion::cursor::Goto(cursor.0, cursor.1)
                )
                .unwrap();
            }
            ConsoleState::Suggesting => {
                write!(
                    stdout,
                    "{}{}> {}\r\n{}[{}]{}",
                    termion::cursor::Goto(1, cursor.1),
                    termion::clear::CurrentLine,
                    self.line.as_str(),
                    termion::clear::CurrentLine,
                    self.suggestions
                        .matches()
                        .iter()
                        .enumerate()
                        .map(|(i, s)| {
                            if Some(i) == self.suggestions.selected {
                                format!("{}{}{}", termion::style::Invert, s, termion::style::Reset)
                            } else {
                                s.clone()
                            }
                        })
                        .join(", "),
                    termion::cursor::Goto(self.line.cursor() as u16 + 3, cursor.1)
                )
                .unwrap();
            }
            ConsoleState::AcceptSuggestion => {
                write!(
                    stdout,
                    "\r\n{}{}",
                    termion::clear::CurrentLine,
                    termion::cursor::Goto(self.line.cursor() as u16 + 3, cursor.1)
                )
                .unwrap();
            }
            _ => {}
        }
        stdout.flush().unwrap();
    }
}

fn typing(key: Key, line: &mut InputLine) -> Option<ConsoleState> {
    match key {
        Key::Ctrl('c') => {
            return Some(ConsoleState::Quit);
        }
        Key::Char(c) => {
            if c == '\n' {
                return Some(ConsoleState::Done);
            } else if c == '\t' {
                return Some(ConsoleState::GetSuggestions);
            } else {
                line.insert(c);
            }
        }
        Key::Left => {
            line.cursor_left();
        }
        Key::Right => {
            line.cursor_right();
        }
        Key::Up => return Some(ConsoleState::HistoryUp),
        Key::Down => return Some(ConsoleState::HistoryDown),
        Key::Backspace => {
            if line.len() > 0 {
                line.backspace();
            }
        }
        _ => {
            // write!(stdout, "{:?}", k).unwrap();
        }
    }
    None
}

#[allow(clippy::unnecessary_wraps)]
fn filter_suggestions(line: &InputLine, suggestions: &mut Suggestions) -> Option<ConsoleState> {
    suggestions.position = line
        .current_word()
        .map(|word| word.position)
        .unwrap_or_default();
    let matches = line
        .current_word()
        .map(|word| word.to_string())
        .or_else(|| Some(String::new()))
        .map(|word| {
            suggestions
                .suggestions
                .iter()
                .filter(|cmd| cmd.starts_with(&word))
                .map(ToString::to_string)
                .take(10)
                .collect()
        })
        .unwrap_or_default();
    suggestions.set_matches(matches);
    if suggestions.matches().is_empty() {
        Some(ConsoleState::NoSuggestions)
    } else {
        Some(ConsoleState::Suggesting)
    }
}

#[allow(clippy::unnecessary_wraps)]
fn suggesting(
    key: Key,
    line: &mut InputLine,
    suggestions: &mut Suggestions,
) -> Option<ConsoleState> {
    match key {
        Key::Ctrl('c') => Some(ConsoleState::Quit),
        Key::Char('\t') => {
            let num_suggestions = suggestions.matches().len();
            let selected = (suggestions.selected.unwrap_or(num_suggestions) + 1) % num_suggestions;
            suggestions.selected = Some(selected);
            None
        }
        Key::Char('\n') => Some(ConsoleState::AcceptSuggestion),
        Key::Esc => Some(ConsoleState::Typing),
        _ => {
            // n.b. not sending TAB to typing
            typing(key, line);
            Some(ConsoleState::FilterSuggestions)
        }
    }
}

#[derive(Debug)]
struct History {
    history: VecDeque<String>,
    index: usize,
    max: usize,
}

impl Default for History {
    fn default() -> Self {
        History {
            history: VecDeque::new(),
            index: 0,
            max: 100,
        }
    }
}

impl History {
    pub fn current(&self) -> Option<&str> {
        self.history.get(self.index).map(|s| s.as_str())
    }

    pub fn save(&mut self, item: String) {
        if self.history.len() == self.max {
            self.history.pop_front();
        }
        self.history.push_back(item);
        self.index = self.history.len() - 1;
    }

    pub fn up(&mut self) {
        self.index = self.index.saturating_sub(1);
    }

    pub fn down(&mut self) {
        self.index = self.history.len().min(self.index + 1);
    }
}

#[derive(Default, Clone)]
struct Suggestions {
    suggestions: Vec<String>,
    matches: Vec<String>,
    position: Range<usize>,
    selected: Option<usize>,
}

impl Suggestions {
    fn new(suggestions: Vec<String>) -> Self {
        Suggestions {
            suggestions,
            ..Default::default()
        }
    }

    fn set_matches(&mut self, matches: Vec<String>) {
        if matches.is_empty() {
            self.selected = None;
        } else {
            self.selected = Some(0);
        }
        self.matches = matches;
    }

    fn matches(&self) -> &[String] {
        &self.matches
    }

    fn accepted_word(&self) -> Option<String> {
        self.selected
            .and_then(|index| self.matches.get(index).cloned())
    }
}

#[derive(Default, Clone)]
struct InputLine {
    line: String,
    cursor: usize,
}

impl InputLine {
    pub fn from_string(line: String) -> Self {
        InputLine {
            cursor: line.len(),
            line,
        }
    }

    pub fn as_str(&self) -> &str {
        &self.line
    }

    pub fn into_string(self) -> String {
        self.line
    }

    pub fn len(&self) -> usize {
        self.line.len()
    }

    pub fn insert(&mut self, c: char) {
        if self.cursor > self.line.len() {
            self.line.push(c);
            self.cursor = self.line.len() + 1;
        } else {
            self.line = self
                .line
                .chars()
                .take(self.cursor)
                .chain(std::iter::once(c))
                .chain(self.line.chars().skip(self.cursor))
                .collect();
            self.cursor += 1;
        }
    }

    pub fn backspace(&mut self) {
        if self.cursor == 0 {
            return;
        }
        if self.cursor > self.line.len() {
            self.line.pop();
        } else {
            self.line = self
                .line
                .chars()
                .take(self.cursor - 1)
                .chain(self.line.chars().skip(self.cursor))
                .collect();
        }
        self.cursor -= 1;
    }

    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn cursor_left(&mut self) {
        self.cursor = self.cursor.saturating_sub(1);
    }

    pub fn cursor_right(&mut self) {
        self.cursor = self.line.len().min(self.cursor + 1);
    }

    pub fn replace_word(&mut self, replacement: &str) {
        if let Some((len, position)) = self
            .current_word()
            .map(|word| (word.len(), word.position))
            .or_else(|| Some((0, (self.len()..self.len()))))
        {
            self.line = self
                .line
                .chars()
                .take(position.start)
                .chain(replacement.chars())
                .chain(self.line.chars().skip(position.end))
                .collect();
            self.cursor += replacement.len() - len;
        }
    }

    pub fn current_word(&self) -> Option<Word> {
        let (index, _) = self
            .line
            .chars()
            .enumerate()
            .fold((0, None), |acc, (ind, c)| {
                if c.is_alphanumeric() {
                    acc
                } else {
                    (ind + 1, Some(c))
                }
            });
        let word = self.line[index..].to_string();
        if !word.is_empty() {
            Some(Word {
                word,
                position: index..self.line.len(),
            })
        } else {
            None
        }
    }

    pub fn preceding_current_word(&self) -> String {
        if let Some(word) = self.current_word() {
            self.line[..word.position.start].to_string()
        } else {
            self.line.to_string()
        }
    }
}

pub struct Word {
    position: Range<usize>,
    word: String,
}

impl Deref for Word {
    type Target = str;
    fn deref(&self) -> &str {
        &self.word
    }
}
