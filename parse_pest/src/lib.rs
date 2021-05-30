use clouseau_core::*;
use clouseau_query::*;
use pest::error::Error as PestError;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
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
    pair.into_inner().fold(Ok(Path::default()), |path, pair| {
        path.and_then(|path| match pair.as_rule() {
            Rule::absolute => Ok(path.append_segment(SegmentType::Root.to_segment())),
            Rule::relative | Rule::current => {
                Ok(path.append_segment(SegmentType::Current.to_segment()))
            }
            Rule::segment => {
                Ok(path
                    .append_segment(parse_segment(pair.into_inner().next().ok_or(Error::Pest)?)?))
            }
            Rule::filter => {
                Ok(path.append_filter(parse_filter(pair.into_inner().next().ok_or(Error::Pest)?)?))
            }
            Rule::call => Ok(path
                .append_function_call(parse_call(pair.into_inner().next().ok_or(Error::Pest)?)?)),
            Rule::EOI => Ok(path),
            _ => Err(Error::Pest),
        })
    })
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
    match pair.as_rule() {
        Rule::wildcard => Ok(SegmentType::Children.to_segment()),
        Rule::ident | Rule::integer => Ok(SegmentType::Child(pair.as_str().into()).to_segment()),
        _ => Err(Error::Pest),
    }
}

fn parse_call(pair: Pair<Rule>) -> Result<String> {
    let name = pair.as_str().to_string();
    let rule = pair.as_rule();
    match rule {
        Rule::ident => Ok(name),
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
        "?>" => Ok((Any, Compare::GreaterThan)),
        "?>=" => Ok((Any, Compare::GreaterThanEq)),
        "?<" => Ok((Any, Compare::LessThan)),
        "?<=" => Ok((Any, Compare::LessThanEq)),
        "?=" => Ok((Any, Compare::Eq)),
        _ => Err(Error::Pest),
    }
}

fn parse_value(pair: Pair<Rule>) -> Result<Value> {
    let tokens = pair.as_str();
    let rule = pair.into_inner().next().ok_or(Error::Pest)?.as_rule();
    match rule {
        Rule::integer => Ok(Value::Int(tokens.parse()?)),
        Rule::string => Ok(Value::String(tokens[1..tokens.len() - 1].to_string())),
        Rule::bool => match tokens {
            "true" => Ok(Value::Bool(true)),
            "false" => Ok(Value::Bool(false)),
            _ => Err(Error::Pest),
        },
        _ => Err(Error::Pest),
    }
}
