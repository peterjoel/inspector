use clouseau_core::{Error, Node, NodeIter, Value};
use itertools::{process_results, Itertools as _};
use std::collections::HashSet;
use std::convert::TryFrom;
use std::iter;

pub fn count(input: NodeIter<'_>) -> NodeIter<'_> {
    NodeIter::from_nodes(iter::once(
        process_results(input, |iter| iter.count())
            .map(Value::from)
            .map(Node::from),
    ))
}

pub fn sum(input: NodeIter<'_>) -> NodeIter<'_> {
    NodeIter::from_nodes(iter::once(
        process_results(
            input.map_ok(i64::try_from).map(|v| v.and_then(|r| r)), // flatten?
            |iter| iter.sum::<i64>(),
        )
        .map(Value::from)
        .map(Node::from),
    ))
}

pub fn max(input: NodeIter<'_>) -> NodeIter<'_> {
    match process_results(
        input.map_ok(Value::try_from).map(|v| v.and_then(|r| r)), // flatten?
        |iter| iter.max(),
    )
    .transpose()
    .ok_or(Error::Empty("max()"))
    {
        Err(e) | Ok(Err(e)) => NodeIter::one(Err(e)),
        Ok(Ok(max)) => NodeIter::one_value(max),
    }
}

pub fn min(input: NodeIter<'_>) -> NodeIter<'_> {
    match process_results(
        input.map_ok(Value::try_from).map(|v| v.and_then(|r| r)), // flatten
        |iter| iter.min(),
    )
    .transpose()
    .ok_or(Error::Empty("min()"))
    {
        Err(e) | Ok(Err(e)) => NodeIter::one(Err(e)),
        Ok(Ok(min)) => NodeIter::one_value(min),
    }
}

pub fn first(input: NodeIter<'_>) -> NodeIter<'_> {
    match process_results(input, |iter| iter.take(1).next())
        .transpose()
        .ok_or(Error::Empty("first()"))
    {
        Err(e) | Ok(Err(e)) => NodeIter::one(Err(e)),
        Ok(node) => NodeIter::one(node),
    }
}

pub fn last(input: NodeIter<'_>) -> NodeIter<'_> {
    match process_results(input, |iter| {
        iter.fold(None, |mut last, val| {
            last.replace(val);
            last
        })
    })
    .transpose()
    .ok_or(Error::Empty("last()"))
    {
        Err(e) | Ok(Err(e)) => NodeIter::one(Err(e)),
        Ok(node) => NodeIter::one(node),
    }
}

pub fn distinct(input: NodeIter<'_>) -> NodeIter<'_> {
    match input
        .map_ok(Value::try_from)
        .map(|v| v.and_then(|r| r)) // flatten?
        .fold_ok(HashSet::new(), |mut seen, v| {
            seen.insert(v);
            seen
        }) {
        Ok(values) => NodeIter::from_values(values.into_iter()),
        Err(e) => NodeIter::one(Err(e)),
    }
}

pub fn keys(input: NodeIter<'_>) -> NodeIter<'_> {
    NodeIter::from_nodes(input.flat_map(|node| match node {
        Ok(Node::Queryable(node)) => NodeIter::from_values(node.keys()),
        Ok(Node::Value(_)) => NodeIter::from_nodes(Err(Error::ExpectedNode)),
        Err(e) => NodeIter::from_nodes(Err(e)),
    }))
}

pub fn data(input: NodeIter<'_>) -> NodeIter<'_> {
    NodeIter::from_nodes(input.filter_map_ok(|node| match node {
        val @ Node::Value(_) => Some(val),
        Node::Queryable(node) => node.data().map(Into::into),
    }))
}

pub fn integers(input: NodeIter<'_>) -> NodeIter<'_> {
    NodeIter::from_nodes(input.filter_map_ok(|node| {
        match node {
            val @ Node::Value(Value::Int(_)) => Some(val),
            Node::Value(_) => None,
            Node::Queryable(node) => node
                .data()
                .filter(|value| matches!(value, Value::Int(_)))
                .map(Into::into),
        }
    }))
}

pub fn strings(input: NodeIter<'_>) -> NodeIter<'_> {
    NodeIter::from_nodes(input.filter_map_ok(|node| {
        match node {
            val @ Node::Value(Value::String(_)) => Some(val),
            Node::Value(_) => None,
            Node::Queryable(node) => node
                .data()
                .filter(|value| matches!(value, Value::String(_)))
                .map(Into::into),
        }
    }))
}
