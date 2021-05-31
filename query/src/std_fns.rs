use super::{Value, ValueIter};
use std::collections::HashSet;

pub fn count(input: ValueIter<'_>) -> ValueIter<'_> {
    ValueIter::once(input.count())
}

pub fn sum(input: ValueIter<'_>) -> ValueIter<'_> {
    ValueIter::once(input.filter_map(Value::into_int).sum::<i64>())
}

pub fn max(input: ValueIter<'_>) -> ValueIter<'_> {
    ValueIter::from_values(input.filter_map(Value::into_int).max())
}

pub fn min(input: ValueIter<'_>) -> ValueIter<'_> {
    ValueIter::from_values(input.filter_map(Value::into_int).min())
}

pub fn first(input: ValueIter<'_>) -> ValueIter<'_> {
    ValueIter::from_values(input.filter_map(Value::into_int).take(1))
}

pub fn last(input: ValueIter<'_>) -> ValueIter<'_> {
    ValueIter::from_values(
        input
            .filter_map(Value::into_int)
            .fold(None, |mut last, val| {
                last.replace(val);
                last
            })
            .into_iter(),
    )
}

pub fn distinct(input: ValueIter<'_>) -> ValueIter<'_> {
    ValueIter::from_values(
        input
            .fold(HashSet::new(), |mut seen, v| {
                seen.insert(v);
                seen
            })
            .into_iter(),
    )
}
