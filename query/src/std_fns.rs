use super::{Value, ValueIter};

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
