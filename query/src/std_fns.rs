use super::{Value, ValueIter};

pub fn count<'a>(input: ValueIter<'a>) -> ValueIter<'a> {
    ValueIter::once(input.count())
}

pub fn sum<'a>(input: ValueIter<'a>) -> ValueIter<'a> {
    ValueIter::once(input.filter_map(Value::into_int).sum::<i64>())
}

pub fn max<'a>(input: ValueIter<'a>) -> ValueIter<'a> {
    ValueIter(Box::from(
        input
            .filter_map(Value::into_int)
            .max()
            .into_iter()
            .map(Value::from),
    ))
}

pub fn min<'a>(input: ValueIter<'a>) -> ValueIter<'a> {
    ValueIter(Box::from(
        input
            .filter_map(Value::into_int)
            .min()
            .into_iter()
            .map(Value::from),
    ))
}
