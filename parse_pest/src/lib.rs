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
    #[error("Unexpected rule {1:?} in {0:?}. This is probably a bug!")]
    UnexpectedRule(Rule, Rule),
    #[error("Expected {1:?} rule in {0:?}. This is probably a bug!")]
    ExpectedRule(Rule, Rule),
    #[error("Expected child rule in {0:?}. This is probably a bug!")]
    ExpectedChildRule(Rule),
    #[error("Unexpected input {1} in rule {0:?}. This is probably a bug!")]
    UnexpectedInput(Rule, String),
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
        .ok_or(Error::ExpectedChildRule(Rule::query))?;
    Ok(Query::new(parse_path(
        ast.into_inner()
            .next()
            .ok_or(Error::ExpectedRule(Rule::query, Rule::path))?,
    )?))
}

fn parse_path(pair: Pair<Rule>) -> Result<Path> {
    let mut path = Path::default();
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::absolute => path.add_selector(Selector::Segment(Segment::Root)),
            Rule::relative => path.add_selector(Selector::Segment(Segment::Current)),
            Rule::selector => path.add_selector(parse_selector(pair)?),
            eh => return Err(Error::UnexpectedRule(Rule::path, eh)),
        }
    }
    Ok(path)
}

fn parse_selector(pair: Pair<Rule>) -> Result<Selector> {
    let pair = pair
        .into_inner()
        .next()
        .ok_or(Error::ExpectedChildRule(Rule::selector))?;
    match pair.as_rule() {
        Rule::segment => Ok(Selector::Segment(parse_segment(pair)?)),
        Rule::explicit_key => Ok(Selector::Segment(parse_explicit_key(pair)?)),
        Rule::filter => Ok(Selector::Filter(parse_filter(pair)?)),
        Rule::call => Ok(Selector::Call(parse_call(pair)?)),
        wtf => Err(Error::UnexpectedRule(Rule::selector, wtf)),
    }
}

fn parse_var(pair: Pair<Rule>) -> Result<String> {
    let var = pair
        .into_inner()
        .next()
        .ok_or(Error::ExpectedChildRule(Rule::var))?;
    let tokens = var.as_str();
    let rule = var.as_rule();
    match rule {
        Rule::ident => Ok(tokens.to_string()),
        umm => Err(Error::UnexpectedRule(Rule::var, umm)),
    }
}

fn parse_segment(pair: Pair<Rule>) -> Result<Segment> {
    let pair = pair
        .into_inner()
        .next()
        .ok_or(Error::ExpectedChildRule(Rule::segment))?;
    match pair.as_rule() {
        Rule::wildcard => Ok(Segment::Children),
        Rule::descendants => Ok(Segment::Descendants),
        Rule::ident | Rule::integer => Ok(Segment::Child(KeyExpr::Literal(pair.as_str().into()))),
        what_the_ => Err(Error::UnexpectedRule(Rule::segment, what_the_)),
    }
}

fn parse_explicit_key(pair: Pair<Rule>) -> Result<Segment> {
    let pair = pair
        .into_inner()
        .next()
        .ok_or(Error::ExpectedChildRule(Rule::explicit_key))?;
    Ok(Segment::Child(parse_key_expr(pair)?))
}

fn parse_key_expr(pair: Pair<Rule>) -> Result<KeyExpr> {
    let pair = pair
        .into_inner()
        .next()
        .ok_or(Error::ExpectedChildRule(Rule::key_expr))?;
    match pair.as_rule() {
        Rule::literal => Ok(KeyExpr::Literal(parse_literal(pair)?)),
        Rule::var => Ok(KeyExpr::Var(parse_var(pair)?)),
        unexpected => Err(Error::UnexpectedRule(Rule::key_expr, unexpected)),
    }
}

fn parse_call(pair: Pair<Rule>) -> Result<Call> {
    let pair = pair
        .into_inner()
        .next()
        .ok_or(Error::ExpectedChildRule(Rule::call))?;
    let name = pair.as_str().to_string();
    let rule = pair.as_rule();
    match rule {
        Rule::ident => Ok(Call::new(name)),
        eh => Err(Error::UnexpectedRule(Rule::call, eh)),
    }
}

fn parse_filter(pair: Pair<Rule>) -> Result<Pred> {
    let mut parts = pair.into_inner();
    let path = parse_path(
        parts
            .next()
            .ok_or(Error::ExpectedRule(Rule::filter, Rule::path))?,
    )?;
    let (match_type, pred) = parse_operator(
        parts
            .next()
            .ok_or(Error::ExpectedRule(Rule::filter, Rule::op_compare))?,
    )?;
    let op_rhs = parts
        .next()
        .ok_or(Error::ExpectedChildRule(Rule::filter))?
        .into_inner()
        .next()
        .ok_or(Error::ExpectedRule(Rule::filter, Rule::op_rhs))?;
    let rhs = match op_rhs.as_rule() {
        Rule::literal => OperatorRhs::Value(parse_literal(op_rhs)?),
        Rule::path => OperatorRhs::Path(parse_path(op_rhs)?),
        Rule::var => OperatorRhs::Var(parse_var(op_rhs)?),
        what_on_earth => return Err(Error::UnexpectedRule(Rule::op_rhs, what_on_earth)),
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
        just_why => Err(Error::UnexpectedInput(
            Rule::op_compare,
            just_why.to_string(),
        )),
    }
}

fn parse_literal(pair: Pair<Rule>) -> Result<Value> {
    let tokens = pair.as_str();
    let value = pair
        .into_inner()
        .next()
        .ok_or(Error::ExpectedChildRule(Rule::literal))?;
    match value.as_rule() {
        Rule::integer => Ok(Value::Int(tokens.parse()?)),
        Rule::string => Ok(Value::String(tokens[1..tokens.len() - 1].to_string())),
        Rule::bool => match tokens {
            "true" => Ok(Value::Bool(true)),
            "false" => Ok(Value::Bool(false)),
            umm => Err(Error::UnexpectedInput(Rule::bool, umm.to_string())),
        },
        Rule::array => Ok(Value::Array(parse_array(value)?)),
        what => Err(Error::UnexpectedRule(Rule::literal, what)),
    }
}

fn parse_array(pair: Pair<Rule>) -> Result<Array> {
    Ok(Array(
        pair.into_inner()
            .map(parse_literal)
            .collect::<Result<Vec<_>, _>>()?
            .into_boxed_slice()
            .into(),
    ))
}
