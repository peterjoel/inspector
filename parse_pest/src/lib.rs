use clouseau_core::*;
use clouseau_query::*;
use pest::error::Error as PestError;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Parse error: {0}")]
    Parse(#[from] PestError<Rule>),
    #[error("Parse error: Pest says this should be impossible!")]
    Pest,
    #[error("Parse error: {0}")]
    Integer(#[from] std::num::ParseIntError),
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Parser)]
#[grammar = "query.pest"]
pub struct QueryParser;

pub fn parse_query(input: &str) -> Result<Query> {
    let ast = QueryParser::parse(Rule::query, input)?
        .next()
        .ok_or(Error::Pest)?;
    Ok(Query::new(parse_path(
        ast.into_inner().next().ok_or(Error::Pest)?,
    )?))
}

fn parse_path(pair: Pair<Rule>) -> Result<Path> {
    let mut path = Path::default();
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::absolute => path.add_selector(Selector::Segment(Segment::Root)),
            Rule::relative => path.add_selector(Selector::Segment(Segment::Current)),
            Rule::selector => path.add_selector(parse_selector(pair)?),
            _ => return Err(Error::Pest),
        }
    }
    Ok(path)
}

fn parse_selector(pair: Pair<Rule>) -> Result<Selector> {
    let pair = pair.into_inner().next().ok_or(Error::Pest)?;
    match pair.as_rule() {
        Rule::segment => Ok(Selector::Segment(parse_segment(pair)?)),
        Rule::filter => Ok(Selector::Filter(parse_filter(pair)?)),
        Rule::call => Ok(Selector::Call(parse_call(pair)?)),
        _ => Err(Error::Pest),
    }
}

fn parse_var(pair: Pair<Rule>) -> Result<String> {
    let var = pair.into_inner().next().ok_or(Error::Pest)?;
    let tokens = var.as_str();
    let rule = var.as_rule();
    match rule {
        Rule::ident => Ok(tokens.to_string()),
        _ => Err(Error::Pest),
    }
}

fn parse_segment(pair: Pair<Rule>) -> Result<Segment> {
    let pair = pair.into_inner().next().ok_or(Error::Pest)?;
    match pair.as_rule() {
        Rule::wildcard => Ok(Segment::Children),
        Rule::descendants => Ok(Segment::Descendants),
        Rule::ident | Rule::integer => Ok(Segment::Child(pair.as_str().into())),
        Rule::literal => Ok(Segment::Child(parse_value(pair)?)),
        _ => Err(Error::Pest),
    }
}

fn parse_call(pair: Pair<Rule>) -> Result<Call> {
    let pair = pair.into_inner().next().ok_or(Error::Pest)?;
    let name = pair.as_str().to_string();
    let rule = pair.as_rule();
    match rule {
        Rule::ident => Ok(Call::new(name)),
        _ => Err(Error::Pest),
    }
}

fn parse_filter(pair: Pair<Rule>) -> Result<Pred> {
    let mut parts = pair.into_inner();
    let path = parse_path(parts.next().ok_or(Error::Pest)?)?;
    let (match_type, pred) = parse_operator(parts.next().ok_or(Error::Pest)?)?;
    let op_rhs = parts
        .next()
        .ok_or(Error::Pest)?
        .into_inner()
        .next()
        .ok_or(Error::Pest)?;
    let rhs = match op_rhs.as_rule() {
        Rule::literal => OperatorRhs::Value(parse_value(op_rhs)?),
        Rule::path => OperatorRhs::Path(parse_path(op_rhs)?),
        Rule::var => OperatorRhs::Var(parse_var(op_rhs)?),
        _ => return Err(Error::Pest),
    };
    Ok(Pred::new(path, pred, rhs, match_type))
}

fn parse_operator(pair: Pair<Rule>) -> Result<(MatchType, Compare)> {
    use MatchType::*;
    match pair.as_str() {
        ">" => Ok((All, Compare::GreaterThan)),
        ">=" => Ok((All, Compare::GreaterThanEq)),
        "<" => Ok((All, Compare::LessThan)),
        "<=" => Ok((All, Compare::LessThanEq)),
        "=" => Ok((All, Compare::Eq)),
        "!=" => Ok((All, Compare::Neq)),
        "?>" => Ok((Any, Compare::GreaterThan)),
        "?>=" => Ok((Any, Compare::GreaterThanEq)),
        "?<" => Ok((Any, Compare::LessThan)),
        "?<=" => Ok((Any, Compare::LessThanEq)),
        "?=" => Ok((Any, Compare::Eq)),
        "?!=" => Ok((Any, Compare::Neq)),
        _ => Err(Error::Pest),
    }
}

fn parse_value(pair: Pair<Rule>) -> Result<Value> {
    let tokens = pair.as_str();
    let value = pair.into_inner().next().ok_or(Error::Pest)?;
    match value.as_rule() {
        Rule::integer => Ok(Value::Int(tokens.parse()?)),
        Rule::string => Ok(Value::String(tokens[1..tokens.len() - 1].to_string())),
        Rule::bool => match tokens {
            "true" => Ok(Value::Bool(true)),
            "false" => Ok(Value::Bool(false)),
            _ => Err(Error::Pest),
        },
        Rule::array => Ok(Value::Array(parse_array(value)?)),
        _ => Err(Error::Pest),
    }
}

fn parse_array(pair: Pair<Rule>) -> Result<Array> {
    let mut values = vec![];
    for pair in pair.into_inner() {
        values.push(parse_value(pair)?);
    }
    Ok(Array(Rc::from(values.into_boxed_slice())))
}
