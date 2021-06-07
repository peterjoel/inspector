use clouseau_core::{Error, NodeOrValue, NodeOrValueIter, Value};
use itertools::{process_results, Itertools as _};
use std::collections::HashSet;
use std::iter;

pub fn count<'a, 'q>(input: NodeOrValueIter<'a, 'q>) -> NodeOrValueIter<'a, 'q> {
    NodeOrValueIter::from_raw(iter::once(
        process_results(input, |iter| iter.count())
            .map(Value::from)
            .map(NodeOrValue::from), // TODO add utility to collapse these intos
    ))
}

pub fn sum<'a, 'q>(input: NodeOrValueIter<'a, 'q>) -> NodeOrValueIter<'a, 'q> {
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

pub fn max<'a, 'q>(input: NodeOrValueIter<'a, 'q>) -> NodeOrValueIter<'a, 'q> {
    match process_results(
        input
            .map_ok(NodeOrValue::try_into_int)
            .map(|v| v.and_then(|r| r)), // flatten?
        |iter| iter.max(),
    )
    .transpose()
    .ok_or(Error::Empty("max()"))
    {
        Err(e) | Ok(Err(e)) => NodeOrValueIter::one(Err(e)),
        Ok(Ok(max)) => NodeOrValueIter::one_value(Value::from(max)),
    }
}

pub fn min<'a, 'q>(input: NodeOrValueIter<'a, 'q>) -> NodeOrValueIter<'a, 'q> {
    match process_results(
        input
            .map_ok(NodeOrValue::try_into_int)
            .map(|v| v.and_then(|r| r)), // flatten
        |iter| iter.min(),
    )
    .transpose()
    .ok_or(Error::Empty("min()"))
    {
        Err(e) | Ok(Err(e)) => NodeOrValueIter::one(Err(e)),
        Ok(Ok(min)) => NodeOrValueIter::one_value(Value::from(min)),
    }
}

pub fn first<'a, 'q>(input: NodeOrValueIter<'a, 'q>) -> NodeOrValueIter<'a, 'q> {
    match process_results(input, |iter| iter.take(1).next())
        .transpose()
        .ok_or(Error::Empty("first()"))
    {
        Err(e) | Ok(Err(e)) => NodeOrValueIter::one(Err(e)),
        Ok(node) => NodeOrValueIter::one(node),
    }
}

pub fn last<'a, 'q>(input: NodeOrValueIter<'a, 'q>) -> NodeOrValueIter<'a, 'q> {
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

pub fn distinct<'a, 'q>(input: NodeOrValueIter<'a, 'q>) -> NodeOrValueIter<'a, 'q> {
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
