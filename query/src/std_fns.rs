use clouseau_core::{Error, NodeOrValue, NodeOrValueIter, Value};
use itertools::{process_results, Itertools as _};
use std::collections::HashSet;
use std::iter;

pub fn count(input: NodeOrValueIter<'_>) -> NodeOrValueIter<'_> {
    NodeOrValueIter::from_raw(iter::once(
        process_results(input, |iter| iter.count())
            .map(Value::from)
            .map(NodeOrValue::from), // TODO add utility to collapse these intos
    ))
}

pub fn sum(input: NodeOrValueIter<'_>) -> NodeOrValueIter<'_> {
    NodeOrValueIter::from_raw(iter::once(
        process_results(
            input
                .map_ok(NodeOrValue::try_into_int)
                .map(|v| v.and_then(|r| r)), // flatten?
            |iter| iter.sum::<i64>(),
        )
        .map(Value::from)
        .map(NodeOrValue::from), // TODO add utility to collapse these intos
    ))
}

pub fn max(input: NodeOrValueIter<'_>) -> NodeOrValueIter<'_> {
    match process_results(
        input
            .map_ok(NodeOrValue::try_into_value)
            .map(|v| v.and_then(|r| r)), // flatten?
        |iter| iter.max(),
    )
    .transpose()
    .ok_or(Error::Empty("max()"))
    {
        Err(e) | Ok(Err(e)) => NodeOrValueIter::one(Err(e)),
        Ok(Ok(max)) => NodeOrValueIter::one_value(max),
    }
}

pub fn min(input: NodeOrValueIter<'_>) -> NodeOrValueIter<'_> {
    match process_results(
        input
            .map_ok(NodeOrValue::try_into_value)
            .map(|v| v.and_then(|r| r)), // flatten
        |iter| iter.min(),
    )
    .transpose()
    .ok_or(Error::Empty("min()"))
    {
        Err(e) | Ok(Err(e)) => NodeOrValueIter::one(Err(e)),
        Ok(Ok(min)) => NodeOrValueIter::one_value(min),
    }
}

pub fn first(input: NodeOrValueIter<'_>) -> NodeOrValueIter<'_> {
    match process_results(input, |iter| iter.take(1).next())
        .transpose()
        .ok_or(Error::Empty("first()"))
    {
        Err(e) | Ok(Err(e)) => NodeOrValueIter::one(Err(e)),
        Ok(node) => NodeOrValueIter::one(node),
    }
}

pub fn last(input: NodeOrValueIter<'_>) -> NodeOrValueIter<'_> {
    match process_results(input, |iter| {
        iter.fold(None, |mut last, val| {
            last.replace(val);
            last
        })
    })
    .transpose()
    .ok_or(Error::Empty("last()"))
    {
        Err(e) | Ok(Err(e)) => NodeOrValueIter::one(Err(e)),
        Ok(node) => NodeOrValueIter::one(node),
    }
}

pub fn distinct(input: NodeOrValueIter<'_>) -> NodeOrValueIter<'_> {
    match input
        .map_ok(NodeOrValue::try_into_value)
        .map(|v| v.and_then(|r| r)) // flatten?
        .fold_ok(HashSet::new(), |mut seen, v| {
            seen.insert(v);
            seen
        }) {
        Ok(values) => NodeOrValueIter::from_values(values.into_iter()),
        Err(e) => NodeOrValueIter::from_raw(iter::once(Err(e))),
    }
}

pub fn keys(input: NodeOrValueIter<'_>) -> NodeOrValueIter<'_> {
    NodeOrValueIter::from_raw(input.flat_map(|node| match node {
        Ok(NodeOrValue::Node(node)) => NodeOrValueIter::from_values(node.keys()),
        Ok(NodeOrValue::Value(_)) => NodeOrValueIter::from_nodes(Err(Error::ExpectedNode)),
        Err(e) => NodeOrValueIter::from_nodes(Err(e)),
    }))
}

pub fn data(input: NodeOrValueIter<'_>) -> NodeOrValueIter<'_> {
    NodeOrValueIter::from_raw(input.filter_map_ok(|node| match node {
        val @ NodeOrValue::Value(_) => Some(val),
        NodeOrValue::Node(node) => node.data().map(Into::into),
    }))
}

pub fn integers(input: NodeOrValueIter<'_>) -> NodeOrValueIter<'_> {
    NodeOrValueIter::from_raw(input.filter_map_ok(|node| {
        match node {
            val @ NodeOrValue::Value(Value::Int(_)) => Some(val),
            NodeOrValue::Value(_) => None,
            NodeOrValue::Node(node) => node
                .data()
                .filter(|value| matches!(value, Value::Int(_)))
                .map(Into::into),
        }
    }))
}

pub fn strings(input: NodeOrValueIter<'_>) -> NodeOrValueIter<'_> {
    NodeOrValueIter::from_raw(input.filter_map_ok(|node| {
        match node {
            val @ NodeOrValue::Value(Value::String(_)) => Some(val),
            NodeOrValue::Value(_) => None,
            NodeOrValue::Node(node) => node
                .data()
                .filter(|value| matches!(value, Value::String(_)))
                .map(Into::into),
        }
    }))
}
